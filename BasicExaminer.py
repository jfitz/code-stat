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
    expected_operators = [
      '+', '-', '*', '/', '^',
      '=', '>', '>=', '<', '<=', '<>',
      '(', ')', ',', ':', ';', '&', '#', '\\'
      ]
    eotb = ListTokenBuilder(expected_operators, 'operator')
    invalid_operators = [
      '==', '->', '!=', '~', '@', '?', '{', '}',
      '%', '<->', '<=>', '++', '--', '::', ':='
      ]
    iotb = ListTokenBuilder(invalid_operators, 'invalid operator')

    expected_keywords = [
      'CHANGE', 'CLOSE', 'DATA', 'DEF', 'DIM', 'END', 'FOR', 'GOSUB',
      'GOTO', 'IF', 'INPUT', 'LET', 'OPEN', 'NEXT', 'PRINT', 'RANDOMIZE',
      'READ', 'REM', 'REMARK', 'RESTORE', 'RETURN', 'STOP'
      ]
    ktb = ListTokenBuilder(expected_keywords, 'keyword')
    
    tokenbuilders = [wtb, ntb, itb, stb, eotb, iotb, ktb]

    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    for line in lines:
      line = line.strip()
      seen_rem = False
      #  consider only lines with text
      if len(line) > 0:
        tokens = tokenizer.tokenize(line)
        for token in tokens:
          if not seen_rem:
            # count all tokens
            num_tokens += 1

            # count known tokens
            if not token.group.startswith('invalid'):
              num_known_tokens += 1
            if str(token) == 'REM' or str(token) == 'REMARK':
              seen_rem = True

    #  unknown operators reduce confidence
    #  unknown identifiers (text of two or more, not FNx) reduce confidence
    confidence_2 = 0
    if num_tokens > 0:
      confidence_2 = num_known_tokens / num_tokens

    # compute confidence
    self.confidence = confidence_1 * confidence_2

  def confidence(self):
    return self.confidence
