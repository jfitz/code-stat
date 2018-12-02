import string
import math
from Examiner import Examiner
from TokenBuilders import *
from PascalTokenBuilders import *
from Tokenizer import Tokenizer

class PascalExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    wtb = WhitespaceTokenBuilder()
    nltb = NewlineTokenBuilder()

    ntb = NumberTokenBuilder()
    itb = IdentifierTokenBuilder()
    stb = StringTokenBuilder()

    bctb = BraceCommentTokenBuilder()
    ctb = CommentTokenBuilder()

    known_operators = [
      '+', '-', '*', '/',
      '=', '<>', '>', '>=', '<', '<=',
      'and', 'or', 'not',
      '&', '|', '~', '<<', '>>',
      ':=', '^', '~',
      '(', ')', ',', '.', ':', ';',
      '[', ']', '..',
      'div', 'mod', 'shl', 'shr', 'in'
      ]
    
    kotb = ListTokenBuilder(known_operators, 'operator', False)

    unknown_operators = set(self.common_operators()) - set(known_operators)
    uotb = ListTokenBuilder(unknown_operators, 'invalid operator', False)

    nltb = NewlineTokenBuilder()
    
    keywords = [
      'and', 'array', 'begin', 'boolean', 'break', 'case', 'char', 'const',
      'do', 'downto', 'else', 'end', 'false', 'file', 'for', 'forward',
      'function', 'goto', 'if', 'integer', 'label', 'nil', 'of', 'otherwise',
      'packed', 'procedure', 'program', 'real', 'record', 'repeat', 'set',
      'string', 'then', 'to', 'true', 'until', 'uses', 'value', 'var', 'while'
      ]

    ktb = ListTokenBuilder(keywords, 'keyword', False)

    tokenbuilders = [wtb, nltb, stb, kotb, uotb, bctb, ctb, ntb, ktb, itb]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    first_token = ''
    last_token = ''
    num_keywords = 0
    found_keywords = {}

    self.tokens = tokenizer.tokenize(code)
    for token in self.tokens:
      token_lower = str(token).lower()
      
      # get the first and last meaningful tokens
      if first_token == '' and not token.whitespace() and not token.comment():
        first_token = token

      if not token.whitespace() and not token.comment():
        last_token = token

      # count keywords
      if token_lower in keywords:
        num_keywords += 1
        found_keywords[token_lower] = True

    num_known_tokens = self.count_valid_tokens(self.tokens)
    num_invalid_operators = self.count_invalid_operators(self.tokens)
    num_known_operators = self.count_known_operators(self.tokens)

    # program should begin with 'program'
    program_end_confidence = 0
    first_token_str = str(first_token)
    if first_token_str.lower() == 'program':
      program_end_confidence += 0.75

    # program should end with '.'
    last_token_str = str(last_token)
    if last_token_str == '.':
      program_end_confidence += 0.25

    # consider the number of matches for begin/end
    ok, num_begin, num_end = self.check_paired_tokens(self.tokens, 'begin', 'end')
    num_begin_end = num_begin + num_end
    begin_end_confidence = 1
    if num_begin_end > 0:
      begin_end_confidence = (num_begin + num_end) / num_begin_end

    # recognized keywords improve confidence
    keyword_confidence = 0
    if num_keywords > 0:
      keyword_confidence = len(found_keywords) / len(keywords)

    #  unknown tokens reduce confidence
    token_confidence = 1
    if len(self.tokens) > 0:
      token_confidence = num_known_tokens / len(self.tokens)

    #  unknown operators reduce confidence
    operator_confidence = 1
    num_operators = num_known_operators + num_invalid_operators
    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    # compute confidence
    self.confidence = program_end_confidence * begin_end_confidence * token_confidence * operator_confidence
    self.confidences = {
      'program_begin': program_end_confidence,
      'program_end': begin_end_confidence,
      'keyword': keyword_confidence,
      'token': token_confidence,
      'operator': operator_confidence
    }
