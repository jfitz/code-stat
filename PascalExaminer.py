import string
from Examiner import Examiner
from PascalTokenBuilders import *
from Tokenizer import Tokenizer

class PascalExaminer(Examiner):
  def __init__(self, code):
    lines = code.split('\n')

    num_tokens = 0
    num_known_tokens = 0

    wtb = WhitespaceTokenBuilder()
    ntb = NumberTokenBuilder()
    itb = IdentifierTokenBuilder()
    stb = StringTokenBuilder()
    bctb = BraceCommentTokenBuilder()
    ctb = CommentTokenBuilder()
    operators = [
      '+', '-', '*', '/', '^',
      '=', '>', '>=', '<', '<=', '<>',
      '(', ')', ',', ':', ';',
      '[', ']', '..'
      ]
    otb = ListTokenBuilder(operators)
    nltb = NewlineTokenBuilder()
    
    tokenbuilders = [wtb, ntb, itb, stb, otb, bctb, ctb, nltb]

    
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
    #  unknown operators reduce confidence
    #  unknown identifiers (text of two or more, not FNx) reduce confidence
    for token in tokens:
      num_tokens += 1
      if not token.invalid:
        num_known_tokens += 1
      if first_token == '' and not token.whitespace() and not token.comment():
        first_token = token
      if not token.whitespace() and not token.comment():
        last_token = token
      if str(token) == 'begin':
        num_begin += 1
      if str(token) == 'end':
        num_end += 1

    if str(first_token).lower() == 'program':
      confidence_1 += 0.5

    if str(last_token) == '.':
      confidence_1 += 0.5

    num_begin_end = num_begin + num_end
    if num_begin_end > 0:
      confidence_2a = num_begin / num_begin_end
      confidence_2b = num_end / num_begin_end
      confidence_2 = confidence_2a + confidence_2b

    if num_tokens > 0:
      confidence_3 = num_known_tokens / num_tokens

    # compute confidence
    self.confidence = confidence_1 * confidence_2 * confidence_3

  def confidence(self):
    return self.confidence

  def remove_pascal_comments(self, line):
    result = ''
    in_brace_comment = False
    for c in line:
      if c == '{' and not in_brace_comment:
        in_brace_comment = True
        c = ''

      if not in_brace_comment:
        result += c

      if c == '}':
        in_brace_comment = False

    return result
