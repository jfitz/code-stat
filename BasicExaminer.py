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

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    basic_number_tb = BasicNumberTokenBuilder()
    variable_tb = VariableTokenBuilder()
    string_tb = StringTokenBuilder(['"'])
    remark_tb = RemarkTokenBuilder()
    line_number_tb = LineNumberTokenBuilder()

    known_operators = [
      '+', '-', '*', '/', '^',
      '=', '>', '>=', '<', '<=', '<>',
      '(', ')', ',', ':', ';', '&', '#', '\\'
      ]
    
    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    unknown_operators = set(self.common_operators()) - set(known_operators)
    unknown_operator_tb = ListTokenBuilder(unknown_operators, 'invalid operator', True)

    keywords = [
      'AS', 'CHANGE', 'CLOSE', 'DATA', 'DEF', 'DIM', 'ELSE', 'END', 'ERROR',
      'FILE', 'FOR', 'GOSUB', 'GO', 'GOTO', 'IF', 'INPUT', 'LET', 'MAT', 'NEXT', 
      'ON', 'OPEN', 'OUTPUT', 'PRINT', 'RANDOMIZE', 'READ', 'REM', 'REMARK',
      'RESTORE', 'RETURN', 'STEP', 'STOP', 'THEN', 'TO', 'USING'
      ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    functions = [
      'ABS', 'ASC', 'ATN', 'CHR$', 'COS', 'EXP', 'LEFT', 'LEFT$',
      'LEN', 'MID', 'MID$', 'RIGHT', 'RIGHT$',
      'SGN', 'SIN', 'STR$', 'TAN', 'VAL',
      'DET', 'INV', 'TRN', 'ZER',
      'FNA', 'FNB', 'FNC', 'FND', 'FNE', 'FNF', 'FNG', 'FNH', 'FNI', 'FNJ',
      'FNK', 'FNL', 'FNM', 'FNN', 'FNO', 'FNP', 'FNQ', 'FNR', 'FNS', 'FNT',
      'FNU', 'FNV', 'FNW', 'FNX', 'FNY', 'FNZ'
    ]

    function_tb = ListTokenBuilder(functions, 'function', True)

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      integer_tb,
      basic_number_tb,
      line_number_tb,
      variable_tb,
      function_tb,
      string_tb,
      known_operator_tb,
      unknown_operator_tb,
      keyword_tb,
      remark_tb
    ]

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
