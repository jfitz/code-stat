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

    ntb = NumberTokenBuilder()
    itb = IdentifierTokenBuilder()
    stb = StringTokenBuilder()

    bctb = BraceCommentTokenBuilder()
    ctb = CommentTokenBuilder()

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '<>', '>', '>=', '<', '<=',
      'and', 'and', 'or', 'not',
      '&', '|', '~', '<<', '>>',
      ':=', '^',
      '(', ')', ',', '.', ':', ';',
      '[', ']', '..'
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

    tokenbuilders = [wtb, ntb, itb, stb, kotb, uotb, bctb, ctb, nltb, ktb]
    
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
    confidence_1 = 0
    first_token_str = str(first_token)
    if first_token_str.lower() == 'program':
      confidence_1 += 0.75

    # program should end with '.'
    last_token_str = str(last_token)
    if last_token_str == '.':
      confidence_1 += 0.25

    # consider the number of matches for begin/end
    num_begin_end = num_begin + num_end
    confidence_2 = 1
    if num_begin_end > 0:
      confidence_2 = (num_begin + num_end) / num_begin_end

    # recognized keywords improve confidence
    confidence_3 = 0
    if num_keywords > 0:
      confidence_3 = len(found_keywords) / len(keywords)

    #  unknown tokens reduce confidence
    confidence_4 = 1
    if num_tokens > 0:
      confidence_4 = num_known_tokens / num_tokens

    #  unknown operators reduce confidence
    confidence_5 = 1
    if num_operators > 0:
      confidence_5 = num_known_operators / num_operators

    # compute confidence
    self.confidence = confidence_1 * confidence_2 * confidence_4 * confidence_5
    self.tokens = tokens
