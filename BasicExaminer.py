import string
from Examiner import Examiner
from BasicTokenBuilders import *
from Tokenizer import Tokenizer

class BasicExaminer(Examiner):
  def __init__(self, code):
    lines = code.split('\n')

    # Pass 1 - all lines begin with numbers
    num_lines_start_num = 0
    for line in lines:
      if len(line) > 0 and line[0].isdigit():
        num_lines_start_num += 1
    confidence_1 = num_lines_start_num / len(lines)

    # Pass 2 - reasonable tokens
    num_tokens = 0
    num_known_tokens = 0

    wtb = WhitespaceTokenBuilder()
    ntb = NumberTokenBuilder()
    itb = IdentifierTokenBuilder()
    stb = StringTokenBuilder()
    operators = [
      '+', '-', '*', '/', '^',
      '=', '>', '>=', '<', '<=', '<>',
      '(', ')', ',', ':', ';', '&', '#', '\\'
      ]
    operators2 = [
      '==', '->', '!=', '~', '@', '?', '{', '}', '%'
      ]
    otb = ListTokenBuilder(operators)
    tokenbuilders = [wtb, ntb, itb, stb, otb]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    for line in lines:
      line = line.strip()
      seen_rem = False
      #  consider only lines with text
      if len(line) > 0:
        #  simple lexer
        tokens = tokenizer.tokenize(line)
        #  unknown operators reduce confidence
        #  unknown identifiers (text of two or more, not FNx) reduce confidence
        for token in tokens:
          if not seen_rem:
            num_tokens += 1
            if not token.invalid:
              num_known_tokens += 1
            if str(token) == 'REM' or str(token) == 'REMARK':
              seen_rem = True

    confidence_2 = 0
    if num_tokens > 0:
      confidence_2 = num_known_tokens / num_tokens

    # compute confidence
    self.confidence = confidence_1 * confidence_2

  def confidence(self):
    return self.confidence

  def is_variable(self, token):
    if len(token) == 1 and token[0].isalpha():
      return True
    if len(token) == 2 and token[0].isalpha() and token[1].isdigit():
      return True
    return False
