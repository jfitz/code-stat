import string
from PascalTokenBuilders import *
from Tokenizer import Tokenizer

class PascalExaminer:
  def __init__(self, code):
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
      '+', '-', '*', '/', '^',
      '=', '>', '>=', '<', '<=', '<>',
      '(', ')', ',', ':', ';',
      '[', ']', '..'
      ]
    kotb = ListTokenBuilder(known_operators, 'operator')
    unknown_operators = [
      '++', '--', '**', '->', '<->', '<=>', '%', '~', '@', '&&', '||'
      '&', '|', '\\'
      ]
    uotb = ListTokenBuilder(unknown_operators, 'invalid operators')
    nltb = NewlineTokenBuilder()
    
    tokenbuilders = [wtb, ntb, itb, stb, kotb, uotb, bctb, ctb, nltb]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    first_token = ''
    last_token = ''
    num_begin = 0
    num_end = 0

    confidence_1 = 0
    confidence_2 = 0
    confidence_3 = 0
    
    tokens = tokenizer.tokenize(code)
    for token in tokens:
      num_tokens += 1
      if not token.group.startswith('invalid'):
        num_known_tokens += 1
      if first_token == '' and not token.whitespace() and not token.comment():
        first_token = token
      if not token.whitespace() and not token.comment():
        last_token = token
      # count 'begin' and 'end' keywords for matches
      if str(token) == 'begin':
        num_begin += 1
      if str(token) == 'end':
        num_end += 1

      # count operators
      if token.group == 'operator' or token.group == 'invalid operator':
        num_operators += 1
      if token.group == 'operator':
        num_known_operators += 1

    # program should begin with 'program'
    if str(first_token).lower() == 'program':
      confidence_1 += 0.5

    # program should end with '.'
    if str(last_token) == '.':
      confidence_1 += 0.5

    # consider the number of matches for begin/end
    num_begin_end = num_begin + num_end
    if num_begin_end > 0:
      confidence_2a = num_begin / num_begin_end
      confidence_2b = num_end / num_begin_end
      confidence_2 = confidence_2a + confidence_2b

    #  unknown operators reduce confidence
    if num_tokens > 0:
      confidence_3 = num_known_tokens / num_tokens

    #  unknown operators reduce confidence
    confidence_4 = 0
    if num_operators > 0:
      confidence_4 = num_known_operators / num_operators

    # compute confidence
    self.confidence = confidence_1 * confidence_2 * confidence_3 * confidence_4
