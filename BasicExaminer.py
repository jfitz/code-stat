import string
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder,
  PrefixedIntegerTokenBuilder,
  LeadCommentTokenBuilder
)
from BasicTokenBuilders import (
  BasicSuffixedIntegerTokenBuilder,
  BasicSuffixedRealTokenBuilder,
  BasicVariableTokenBuilder,
  RemarkTokenBuilder,
  LineNumberTokenBuilder
)
from Tokenizer import Tokenizer

class BasicExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    integer_suffix_tb = BasicSuffixedIntegerTokenBuilder('%')
    long_suffix_tb = BasicSuffixedIntegerTokenBuilder('&')
    single_suffix_tb = BasicSuffixedRealTokenBuilder(False, False, '!')
    double_suffix_tb = BasicSuffixedRealTokenBuilder(False, False, '#')
    hex_constant_tb = PrefixedIntegerTokenBuilder('&H', True, '0123456789ABCDEFabcdef_')
    octal_constant_tb = PrefixedIntegerTokenBuilder('&O', True, '01234567_')
    binary_constant_tb = PrefixedIntegerTokenBuilder('&B', True, '01_')
    variable_tb = BasicVariableTokenBuilder('%#!$&')
    string_tb = StringTokenBuilder(['"'], True, False)
    remark_tb = RemarkTokenBuilder()
    comment_tb = LeadCommentTokenBuilder("'")
    line_number_tb = LineNumberTokenBuilder()

    stmt_separator_tb = ListTokenBuilder([':'], 'statement separator', False)

    known_operators = [
      '+', '-', '*', '/', '^',
      '=', '>', '>=', '<', '<=', '<>',
      '#', '\\', '#'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-', '#'
    ]

    groupers = ['(', ')', ',', ';']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

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

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      stmt_separator_tb,
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
      string_tb,
      known_operator_tb,
      groupers_tb,
      keyword_tb,
      function_tb,
      variable_tb,
      remark_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # do not check for two operands in a row
    self.calc_line_format_confidence()


  def calc_line_format_confidence(self):
    lines = self.split_tokens(self.tokens)
    num_lines = 0
    num_lines_correct = 0

    for line in lines:
      if len(line) > 0:
        num_lines += 1

        if line[0].group == 'line number':
          num_lines_correct += 1
    
    line_format_confidence = 0.0

    if num_lines > 0:
      line_format_confidence = num_lines_correct / num_lines

    self.confidences['line_format'] = line_format_confidence
