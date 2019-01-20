import string
import math
from Examiner import Examiner
from TokenBuilders import *
from PascalTokenBuilders import *
from Tokenizer import Tokenizer

class PascalExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = ListTokenBuilder([';'], 'statement separator', False)

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(True, True)
    real_exponent_tb = RealExponentTokenBuilder(True, True)
    hex_constant_tb = PrefixedIntegerTokenBuilder('$', '0123456789ABCDEFabcdef')
    octal_constant_tb = PrefixedIntegerTokenBuilder('&', '01234567')
    binary_constant_tb = PrefixedIntegerTokenBuilder('%', '01')
    char_constant_tb = PrefixedIntegerTokenBuilder('#', '0123456789')
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(["'"])

    brace_comment_tb = BraceCommentTokenBuilder()
    paren_star_comment_tb = ParenStarCommentTokenBuilder()

    known_operators = [
      '+', '-', '*', '/',
      '=', '<>', '>', '>=', '<', '<=',
      'and', 'or', 'not',
      '&', '|', '~', '<<', '>>',
      ':=', '^', '~', '@',
      '(', ')', ',', '.', ':',
      '[', ']', '..',
      'div', 'mod', 'shl', 'shr', 'in'
    ]
    
    known_operator_tb = ListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'array', 'begin', 'boolean', 'break', 'case', 'char', 'const',
      'do', 'downto', 'else', 'end', 'false', 'file', 'for', 'forward',
      'function', 'goto', 'if', 'integer', 'label', 'nil', 'of', 'otherwise',
      'packed', 'procedure', 'program', 'real', 'record', 'repeat', 'reset',
      'set', 'string', 'then', 'to', 'true', 'until', 'uses', 'value', 'var',
      'while'
      ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      hex_constant_tb,
      octal_constant_tb,
      binary_constant_tb,
      char_constant_tb,
      keyword_tb,
      string_tb,
      known_operator_tb,
      identifier_tb,
      brace_comment_tb,
      paren_star_comment_tb,
      self.unknown_operator_tb
      ]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = tokenizer.tokenize(code)

    # get the first and last meaningful tokens
    first_token = None
    last_token = None

    for token in self.tokens:
      if first_token is None and token.group not in ['whitespace', 'comment', 'newline']:
        first_token = token

      if token.group not in ['whitespace', 'comment', 'newline']:
        last_token = token

    num_known_tokens = self.count_valid_tokens(self.tokens)
    num_invalid_operators = self.count_invalid_operators(self.tokens)
    num_known_operators = self.count_known_operators(self.tokens)

    # program should begin with 'program'
    program_end_confidence = 0.0
    if first_token is not None:
      if first_token.text.lower() == 'program':
        program_end_confidence += 0.75

    # program should end with '.'
    if last_token is not None:
      if last_token.text == '.':
        program_end_confidence += 0.25

    # consider the number of matches for begin/end
    ok, num_begin, num_end = self.check_paired_tokens(self.tokens, 'begin', 'end')
    num_begin_end = num_begin + num_end
    begin_end_confidence = 1.0
    if num_begin_end > 0:
      begin_end_confidence = (num_begin + num_end) / num_begin_end

    #  unknown tokens reduce confidence
    token_confidence = 1.0

    if len(self.tokens) > 0:
      token_confidence = num_known_tokens / len(self.tokens)

    #  unknown operators reduce confidence
    operator_confidence = 1.0
    num_operators = num_known_operators + num_invalid_operators

    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    self.confidences = {
        'program_end': program_end_confidence,
        'begin_end': begin_end_confidence,
        'token': token_confidence,
        'operator': operator_confidence
      }
