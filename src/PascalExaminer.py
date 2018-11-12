import string
import math
from Examiner import Examiner
from TokenBuilders import *
from PascalTokenBuilders import *
from Tokenizer import Tokenizer

class PascalExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    lines = code.split('\n')

    num_tokens = 0
    num_known_tokens = 0
    num_operators = 0
    num_known_operators = 0

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
      'and', 'then', 'or', 'else', 'not',
      '&', '|', '~', '<<', '>>',
      ':=', '^', '~',
      '(', ')', ',', '.', ':', ';',
      '[', ']', '..',
      'div', 'mod', 'shl', 'shr', 'in'
      ]
    
    kotb = ListTokenBuilder(known_operators, 'operator')

    unknown_operators = set(self.common_operators()) - set(known_operators)
    uotb = ListTokenBuilder(unknown_operators, 'invalid operator')

    nltb = NewlineTokenBuilder()
    
    keywords = [
      'and', 'begin', 'case', 'div', 'do', 'downto', 'else', 'end', 'for',
      'forward', 'function', 'goto', 'if', 'int', 'nil', 'not', 'of', 'or',
      'otherwise', 'packed', 'procedure', 'program', 'real', 'record',
      'repeat', 'then', 'to', 'until', 'uses', 'value', 'var', 'while'
      ]

    ktb = ListTokenBuilder(keywords, 'keyword')

    tokenbuilders = [wtb, nltb, stb, kotb, uotb, bctb, ctb, nltb, ntb, itb, ktb]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    first_token = ''
    last_token = ''
    num_begin = 0
    num_end = 0
    num_keywords = 0
    found_keywords = {}

    tokens = tokenizer.tokenize(code)
    for token in tokens:
      token_lower = str(token).lower()
      
      num_tokens += 1
      
      if not token.group.startswith('invalid'):
        num_known_tokens += 1
        
      if first_token == '' and not token.whitespace() and not token.comment():
        first_token = token
        
      if not token.whitespace() and not token.comment():
        last_token = token
        
      # count 'begin' and 'end' keywords for matches
      if token_lower == 'begin':
        num_begin += 1
      if token_lower == 'end':
        num_end += 1

      # count operators
      if token.group == 'operator' or token.group == 'invalid operator':
        num_operators += 1
      if token.group == 'operator':
        num_known_operators += 1

      # count keywords
      if token_lower in keywords:
        num_keywords += 1
        found_keywords[token_lower] = True

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
    if num_tokens > 0:
      token_confidence = num_known_tokens / num_tokens

    #  unknown operators reduce confidence
    operator_confidence = 1
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
    self.tokens = tokens
