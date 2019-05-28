import string
from Examiner import Examiner
from Token import Token
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
  BasicSuffixed2IntegerTokenBuilder,
  BasicSuffixedRealTokenBuilder,
  BasicVariableTokenBuilder,
  RemarkTokenBuilder
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
    integer_suffix_tb = BasicSuffixedIntegerTokenBuilder(['%', '&', 'S', 'I', 'L', 'F', 'D', 'R'])
    integer_suffix_us_tb = BasicSuffixed2IntegerTokenBuilder('US')
    integer_suffix_ui_tb = BasicSuffixed2IntegerTokenBuilder('UI')
    integer_suffix_ul_tb = BasicSuffixed2IntegerTokenBuilder('UL')
    float_suffix_tb = BasicSuffixedRealTokenBuilder(False, False, ['!', '#', 'F', 'D', 'R'])
    hex_constant_tb = PrefixedIntegerTokenBuilder('&H', True, '0123456789ABCDEFabcdef_')
    octal_constant_tb = PrefixedIntegerTokenBuilder('&O', True, '01234567_')
    binary_constant_tb = PrefixedIntegerTokenBuilder('&B', True, '01_')
    variable_tb = BasicVariableTokenBuilder('%#!$&')
    string_tb = StringTokenBuilder(['"'], True, False)
    remark_tb = RemarkTokenBuilder()
    comment_tb = LeadCommentTokenBuilder("'")

    stmt_separator_tb = ListTokenBuilder([':'], 'statement separator', False)

    known_operators = [
      '+', '-', '*', '/', '^',
      '=', '>', '>=', '<', '<=', '<>',
      '#', '\\', '#', 'AND', 'OR', 'NOT'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-', '#', 'NOT'
    ]

    groupers = ['(', ')', ',', ';']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'AS', 'CHANGE', 'CLOSE', 'DATA', 'DEF', 'DIM', 'ELSE', 'END',
      'ERROR', 'ERRNO', 'ERRLN',
      'FILE', 'FOR', 'GOSUB', 'GO', 'GOTO', 'IF', 'INPUT', 'LET', 'LINE',
      'MAT', 'NEXT', 
      'ON', 'ONERR', 'OPEN', 'OUTPUT', 'PEEK', 'POKE', 'PRINT',
      'RANDOMIZE', 'READ', 'REM', 'REMARK',
      'RESTORE', 'RETURN', 'STEP', 'STOP', 'THEN', 'TO', 'USING'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    functions = [
      'ASC', 'CHR', 'CHR$', 'STR$', 'TAB', 'POS',
      'ATN', 'COS', 'SIN', 'TAN',
      'ABS', 'EXP', 'INT', 'LOG', 'RND', 'SGN', 'SQR',
      'INSTR', 'LEFT', 'LEFT$', 'LEN', 'MID', 'MID$', 'RIGHT', 'RIGHT$', 'VAL',
      'CON', 'DET', 'IDN', 'INV', 'TRN', 'ZER',
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
      float_suffix_tb,
      integer_suffix_tb,
      integer_suffix_us_tb,
      integer_suffix_ui_tb,
      integer_suffix_ul_tb,
      real_tb,
      real_exponent_tb,
      hex_constant_tb,
      octal_constant_tb,
      binary_constant_tb,
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

    self.ConvertNumbersToLineNumbers()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence()
    self.calc_operand_confidence()
    # self.calc_value_value_confidence()
    # self.calc_value_value_different_confidence()
    self.calc_keyword_confidence()
    self.calc_line_format_confidence()
    self.calc_statistics()


  def ConvertNumbersToLineNumbers(self):
    prev_token = Token('\n', 'newline')

    for token in self.tokens:
      if token.group == 'number' and\
        prev_token.group == 'newline':
        token.group = 'line number'

      if token.group not in ['whitespace', 'comment']:
        prev_token = token


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

    self.confidences['line format'] = line_format_confidence
