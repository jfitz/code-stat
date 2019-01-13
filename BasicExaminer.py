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
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False)
    integer_suffix_tb = BasicSuffixedIntegerTokenBuilder('%')
    long_suffix_tb = BasicSuffixedIntegerTokenBuilder('&')
    single_suffix_tb = BasicSuffixedRealTokenBuilder(False, False, '!')
    double_suffix_tb = BasicSuffixedRealTokenBuilder(False, False, '#')
    hex_constant_tb = PrefixedIntegerTokenBuilder('&H', '0123456789ABCDEFabcdef_')
    octal_constant_tb = PrefixedIntegerTokenBuilder('&O', '01234567_')
    binary_constant_tb = PrefixedIntegerTokenBuilder('&B', '01_')
    variable_tb = BasicVariableTokenBuilder('%#!$&')
    string_tb = StringTokenBuilder(['"'])
    remark_tb = RemarkTokenBuilder()
    line_number_tb = LineNumberTokenBuilder()

    stmt_separator_tb = ListTokenBuilder([':'], 'statement separator', False)

    known_operators = [
      '+', '-', '*', '/', '^',
      '=', '>', '>=', '<', '<=', '<>',
      '(', ')', ',', ';', '#', '\\'
      ]
    
    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'AS', 'CHANGE', 'CLOSE', 'DATA', 'DEF', 'DIM', 'ELSE', 'END', 'ERROR',
      'FILE', 'FOR', 'GOSUB', 'GO', 'GOTO', 'IF', 'INPUT', 'LET', 'MAT', 'NEXT', 
      'ON', 'ONERR', 'OPEN', 'OUTPUT', 'PEEK', 'POKE', 'PRINT',
      'RANDOMIZE', 'READ', 'REM', 'REMARK',
      'RESTORE', 'RETURN', 'STEP', 'STOP', 'THEN', 'TO', 'USING'
      ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    functions = [
      'ASC', 'CHR$', 'STR$', 'TAB',
      'ATN', 'COS', 'SIN', 'TAN',
      'ABS', 'EXP', 'INT', 'LOG', 'RND', 'SGN', 'SQR',
      'LEFT', 'LEFT$', 'LEN', 'MID', 'MID$', 'RIGHT', 'RIGHT$', 'VAL',
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
      integer_exponent_tb,
      single_suffix_tb,
      double_suffix_tb,
      integer_suffix_tb,
      long_suffix_tb,
      real_tb,
      real_exponent_tb,
      hex_constant_tb,
      octal_constant_tb,
      binary_constant_tb,
      line_number_tb,
      variable_tb,
      function_tb,
      string_tb,
      stmt_separator_tb,
      known_operator_tb,
      self.unknown_operator_tb,
      keyword_tb,
      remark_tb
    ]

    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = tokenizer.tokenize(code)

    found_keywords = self.find_keywords(self.tokens)
    found_identifiers = self.find_identifiers(self.tokens)

    num_known_tokens = self.count_valid_tokens(self.tokens)
    num_invalid_operators = self.count_invalid_operators(self.tokens)
    num_known_operators = self.count_known_operators(self.tokens)

    # recognized keywords improve confidence
    self.keyword_confidence = 0.0

    if len(found_keywords) > 0:
      self.keyword_confidence = 1.0

    if len(found_identifiers) > 0:
      self.keyword_confidence = len(found_keywords) / len(found_identifiers)

    # unknown tokens reduce confidence
    self.token_confidence = 1.0

    if len(self.tokens) > 0:
      self.token_confidence = num_known_tokens / len(self.tokens)

    #  unknown operators reduce confidence
    self.operator_confidence = 1.0
    num_operators = num_known_operators + num_invalid_operators

    if num_operators > 0:
      self.operator_confidence = num_known_operators / num_operators

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
    
    self.line_format_confidence = 0

    if num_lines > 0:
      self.line_format_confidence = num_lines_correct / num_lines


  def confidence(self):
    return self.line_format_confidence *  self.token_confidence * self.operator_confidence


  def confidences(self):
    return {
      'line_format': self.line_format_confidence,
      'token': self.token_confidence,
      'keyword': self.keyword_confidence,
      'operator': self.operator_confidence
    }
