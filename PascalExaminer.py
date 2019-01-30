import string
import math
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder,
  PrefixedIntegerTokenBuilder
)
from PascalTokenBuilders import (
  IdentifierTokenBuilder,
  BraceCommentTokenBuilder,
  ParenStarCommentTokenBuilder
)
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
    string_tb = StringTokenBuilder(["'"], False)

    brace_comment_tb = BraceCommentTokenBuilder()
    paren_star_comment_tb = ParenStarCommentTokenBuilder()

    known_operators = [
      '+', '-', '*', '/',
      '=', '<>', '>', '>=', '<', '<=',
      'and', 'or', 'not',
      '&', '|', '~', '<<', '>>',
      ':=', '^', '~', '@',
      '.', ':',
      '..',
      'div', 'mod', 'shl', 'shr', 'in'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', False)

    groupers = ['(', ')', ',', '[', ']']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

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
      groupers_tb,
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
    ok, num_begin, num_end = self.check_paired_tokens(self.tokens, ['begin', 'record', 'case'], ['end'])
    num_begin_end = num_begin + num_end
    # assume no begin or end keywords
    begin_end_confidence = 0.0
    if num_begin_end > 0:
      begin_end_confidence = 1.0
      difference = num_begin - num_end
      # match or -1 for turbo pascal which allows no starting begin
      if difference not in [0, -1]:
        # difference in counts reduces confidence
        reduction = (abs(difference) / num_begin_end) * 0.1
        begin_end_confidence -= reduction

    if not ok:
      begin_end_confidence *= 0.75

    self.calc_token_confidence()
    self.calc_operator_confidence()


    self.confidences['program_end'] = program_end_confidence
    self.confidences['begin_end'] = begin_end_confidence
