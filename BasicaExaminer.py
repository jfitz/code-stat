import string
from Examiner import Examiner
from Token import Token
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IntegerTokenBuilder,
  SuffixedIntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  SuffixedRealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  PrefixedIntegerTokenBuilder,
  LeadCommentTokenBuilder
)
from BasicTokenBuilders import (
  BasicLongVariableTokenBuilder,
  RemarkTokenBuilder
)
from Tokenizer import Tokenizer

class BasicaExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(False)
    real_tb = RealTokenBuilder(False, False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    integer_suffix_tb = SuffixedIntegerTokenBuilder(['%', '&', 'S', 'I', 'L', 'F', 'D', 'R', 'US', 'UI', 'UL'], None)
    float_suffix_tb = SuffixedRealTokenBuilder(False, False, ['!', '#', 'F', 'D', 'R'], '_')
    hex_constant_tb = PrefixedIntegerTokenBuilder('&H', True, '0123456789ABCDEFabcdef_')
    octal_constant_tb = PrefixedIntegerTokenBuilder('&O', True, '01234567_')
    binary_constant_tb = PrefixedIntegerTokenBuilder('&B', True, '01_')
    variable_tb = BasicLongVariableTokenBuilder('%#!$&')
    string_tb = StringTokenBuilder(['"'], True, False)
    remark_tb = RemarkTokenBuilder()
    comment_tb = LeadCommentTokenBuilder("'")

    stmt_separator_tb = SingleCharacterTokenBuilder(':', 'statement separator')

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
    group_ends = [')']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'AS', 'CHANGE', 'CLOSE', 'DATA', 'DEF', 'DIM', 'ELSE', 'END',
      'ERROR', 'ERRNO', 'ERRLN',
      'FILE', 'FOR', 'GOSUB', 'GO', 'GOTO', 'IF', 'INPUT', 'LET', 'LINE',
      'MAT', 'NEXT', 
      'ON', 'ONERR', 'OPEN', 'OUTPUT', 'PEEK', 'POKE', 'PRINT',
      'RANDOMIZE', 'READ', 'REM', 'REMARK',
      'RESTORE', 'RETURN', 'STEP', 'STOP', 'THEN', 'TO', 'USING',
      'SYSTEM', 'SCREEN', 'WIDTH', 'PAINT', 'CIRCLE', 'COLOR',
      'WHILE', 'WEND', 'KEY', 'ON', 'OFF', 'PSET', 'LOCATE',
      'LSET', 'RSET', 'GET', 'PUT', 'FILES'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

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

    function_tb = ListTokenBuilder(functions, 'function', False)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      float_suffix_tb,
      integer_suffix_tb,
      real_tb,
      real_exponent_tb,
      hex_constant_tb,
      octal_constant_tb,
      binary_constant_tb,
      keyword_tb,
      known_operator_tb,
      function_tb,
      variable_tb,
      groupers_tb,
      string_tb,
      remark_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.convert_numbers_to_line_numbers()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_line_format_confidence()
    self.calc_statistics()


  def convert_numbers_to_line_numbers(self):
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
