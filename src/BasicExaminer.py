import string
from Examiner import Examiner
from TokenBuilders import *
from BasicTokenBuilders import *
from Tokenizer import Tokenizer

class BasicExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    lines = code.split('\n')

    # Pass 1 - all lines begin with numbers
    num_lines_start_num = 0
    num_nonblank_lines = 0
    for line in lines:
      line = line.strip()
      if len(line) > 0:
        num_nonblank_lines += 1
        if line[0].isdigit():
          num_lines_start_num += 1

    line_format_confidence = 0.0
    if num_nonblank_lines > 0:
      line_format_confidence = num_lines_start_num / num_nonblank_lines

    # Pass 2 - tokens
    num_tokens = 0
    num_known_tokens = 0
    num_operators = 0
    num_known_operators = 0

    wtb = WhitespaceTokenBuilder()
    nltb = NewlineTokenBuilder()

    ntb = BasicNumberTokenBuilder()
    vtb = VariableTokenBuilder()
    stb = StringTokenBuilder()
    rtb = RemarkTokenBuilder()

    known_operators = [
      '+', '-', '*', '/', '^',
      '=', '>', '>=', '<', '<=', '<>',
      '(', ')', ',', ':', ';', '&', '#', '\\'
      ]
    
    kotb = ListTokenBuilder(known_operators, 'operator')

    unknown_operators = set(self.common_operators()) - set(known_operators)
    uotb = ListTokenBuilder(unknown_operators, 'invalid operator')

    keywords = [
      'CHANGE', 'CLOSE', 'DATA', 'DEF', 'DIM', 'END', 'FOR', 'GOSUB',
      'GOTO', 'IF', 'INPUT', 'LET', 'OPEN', 'NEXT', 'PRINT', 'RANDOMIZE',
      'READ', 'REM', 'REMARK', 'RESTORE', 'RETURN', 'STOP'
      ]

    ktb = ListTokenBuilder(keywords, 'keyword')

    functions = [
      'ABS', 'ASC', 'CHR$', 'COS', 'DET', 'INV', 'LEFT$', 'LEN', 'MID$', 'RIGHT$',
      'SGN', 'SIN', 'STR$', 'TRN', 'VAL', 'ZER',
      'FNA', 'FNB', 'FNC', 'FND', 'FNE', 'FNF', 'FNG', 'FNH', 'FNI', 'FNJ',
      'FNK', 'FNL', 'FNM', 'FNN', 'FNO', 'FNP', 'FNQ', 'FNR', 'FNS', 'FNT',
      'FNU', 'FNV', 'FNW', 'FNX', 'FNY', 'FNZ'
    ]

    ftb = ListTokenBuilder(functions, 'function')

    tokenbuilders = [wtb, nltb, ntb, vtb, ftb, stb, kotb, uotb, ktb, rtb]

    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = []
    for line in lines:
      line = line.strip()
      
      #  consider only lines with text
      if len(line) > 0:
        tokens = tokenizer.tokenize(line)
        tokens.append(Token('\n', 'newline'))
        self.tokens += tokens
        
        for token in tokens:
          # count all tokens
          num_tokens += 1

          # count known tokens
          if not token.group.startswith('invalid'):
            num_known_tokens += 1

          # count operators
          if token.group == 'operator' or token.group == 'invalid operator':
            num_operators += 1
          if token.group == 'operator':
            num_known_operators += 1

    # unknown tokens reduce confidence
    token_confidence = 1.0
    if num_tokens > 0:
      token_confidence = num_known_tokens / num_tokens

    #  unknown operators reduce confidence
    operator_confidence = 1.0
    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    #  unknown identifiers (text of two or more, not FNx) reduce confidence

    # compute confidence
    self.confidence = line_format_confidence * token_confidence * operator_confidence
    self.confidences = {
      'line_format': line_format_confidence,
      'token': token_confidence,
      'operator': operator_confidence
      }
