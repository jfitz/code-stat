import string
from Examiner import Examiner
from TokenBuilders import *
from BasicTokenBuilders import *
from Tokenizer import Tokenizer

class BasicExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    num_known_tokens = 0
    num_invalid_operators = 0
    num_known_operators = 0

    wtb = WhitespaceTokenBuilder()
    nltb = NewlineTokenBuilder()

    bntb = BasicNumberTokenBuilder()
    vtb = VariableTokenBuilder()
    stb = StringTokenBuilder(['"'])
    rtb = RemarkTokenBuilder()
    lntb = LineNumberTokenBuilder()

    known_operators = [
      '+', '-', '*', '/', '^',
      '=', '>', '>=', '<', '<=', '<>',
      '(', ')', ',', ':', ';', '&', '#', '\\'
      ]
    
    kotb = ListTokenBuilder(known_operators, 'operator', True)

    unknown_operators = set(self.common_operators()) - set(known_operators)
    uotb = ListTokenBuilder(unknown_operators, 'invalid operator', True)

    keywords = [
      'AS', 'CHANGE', 'CLOSE', 'DATA', 'DEF', 'DIM', 'ELSE', 'END', 'ERROR',
      'FILE', 'FOR', 'GOSUB', 'GO', 'GOTO', 'IF', 'INPUT', 'LET', 'ON', 'OPEN',
      'OUTPUT', 'NEXT', 'PRINT', 'RANDOMIZE', 'READ', 'REM', 'REMARK',
      'RESTORE', 'RETURN', 'STEP', 'STOP', 'THEN', 'TO'
      ]

    ktb = ListTokenBuilder(keywords, 'keyword', True)

    functions = [
      'ABS', 'ASC', 'CHR$', 'COS', 'DET', 'INV', 'LEFT$', 'LEN', 'MID$', 'RIGHT$',
      'SGN', 'SIN', 'STR$', 'TRN', 'VAL', 'ZER',
      'FNA', 'FNB', 'FNC', 'FND', 'FNE', 'FNF', 'FNG', 'FNH', 'FNI', 'FNJ',
      'FNK', 'FNL', 'FNM', 'FNN', 'FNO', 'FNP', 'FNQ', 'FNR', 'FNS', 'FNT',
      'FNU', 'FNV', 'FNW', 'FNX', 'FNY', 'FNZ'
    ]

    ftb = ListTokenBuilder(functions, 'function', True)

    tokenbuilders = [wtb, nltb, bntb, lntb, vtb, ftb, stb, kotb, uotb, ktb, rtb]

    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = tokenizer.tokenize(code)

    num_known_tokens = self.count_valid_tokens(self.tokens)
    num_invalid_operators = self.count_invalid_operators(self.tokens)
    num_known_operators = self.count_known_operators(self.tokens)

    # unknown tokens reduce confidence
    token_confidence = 1.0
    if len(self.tokens) > 0:
      token_confidence = num_known_tokens / len(self.tokens)

    #  unknown operators reduce confidence
    operator_confidence = 1.0
    num_operators = num_known_operators + num_invalid_operators
    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    #  unknown identifiers (text of two or more, not FNx) reduce confidence

    # line format
    lines = self.split_tokens(self.tokens)
    num_lines = 0
    num_lines_correct = 0
    for line in lines:
      if len(line) > 0:
        num_lines += 1

        if line[0].group == 'line number':
          num_lines_correct += 1
    
    line_format_confidence = 0

    if num_lines > 0:
      line_format_confidence = num_lines_correct / num_lines

    # compute confidence
    self.confidence = line_format_confidence * token_confidence * operator_confidence
    self.confidences = {
      'line_format': line_format_confidence,
      'token': token_confidence,
      'operator': operator_confidence
      }


  def split_tokens(self, tokens):
    token_groups = []

    token_group = []

    for token in tokens:
      if token.group == 'newline':
        if len(token_group) > 0:
          token_groups.append(token_group)
          token_group = []
      else:
        token_group.append(token)
    
    if len(token_group) > 0:
      token_groups.append(token_group)

    return token_groups
