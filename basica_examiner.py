import string

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StuffedQuoteStringTokenBuilder,
  IntegerTokenBuilder,
  SuffixedIntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  SuffixedRealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  PrefixedIntegerTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from basic_token_builders import (
  BasicLongVariableTokenBuilder,
  RemarkTokenBuilder,
  LongUserFunctionTokenBuilder
)
from examiner import Examiner

class BasicaExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StuffedQuoteStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    SuffixedIntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    SuffixedRealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    BasicLongVariableTokenBuilder.__escape_z__()
    RemarkTokenBuilder.__escape_z__()
    LongUserFunctionTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(False)
    real_tb = RealTokenBuilder(False, False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    integer_suffix_tb = SuffixedIntegerTokenBuilder(['%', '&', 'S', 'I', 'L', 'F', 'D', 'R', 'US', 'UI', 'UL'], True, None)
    float_suffix_tb = SuffixedRealTokenBuilder(False, False, ['!', '#', 'F', 'D', 'R'], True, '_')
    hex_constant_tb = PrefixedIntegerTokenBuilder('&H', True, '0123456789ABCDEFabcdef_')
    octal_constant_tb = PrefixedIntegerTokenBuilder('&O', True, '01234567_')
    binary_constant_tb = PrefixedIntegerTokenBuilder('&B', True, '01_')
    variable_tb = BasicLongVariableTokenBuilder('%#!$&')
    quotes = ['"']
    string_tb = StuffedQuoteStringTokenBuilder(quotes, False)
    remark_tb = RemarkTokenBuilder()
    comment_tb = LeadToEndOfLineTokenBuilder("'", True, 'comment')
    comment2_tb = LeadToEndOfLineTokenBuilder("’", True, 'comment')

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
    group_starts = ['(', ',']
    group_ends = [')']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'AS',
      'BASE',
      'CALL', 'CHAIN', 'CHANGE', 'CIRCLE', 'CLEAR', 'CLS', 'CLOSE', 'COLOR', 'COMMON',
      'DATA', 'DEF', 'DEFDBL', 'DEFINT', 'DEFSNG', 'DEFSTR', 'DIM',
      'ELSE', 'END', 'ERASE', 'ERROR', 'ERRNO', 'ERRLN',
      'FIELD', 'FILE', 'FILES', 'FOR',
      'GET', 'GOSUB', 'GO', 'GOTO',
      'IF', 'INPUT',
      'KEY', 'KILL',
      'LET', 'LINE', 'LOCATE', 'LSET',
      'NEXT',
      'OFF', 'ON', 'ONERR', 'OPEN', 'OUTPUT', 'OPTION',
      'PAINT', 'POKE', 'PRINT', 'PSET', 'PUT',
      'RANDOMIZE', 'READ', 'REM', 'REMARK', 'RESET', 'RESTORE', 'RESUME', 'RETURN', 'RSET',
      'SCREEN', 'SET', 'STEP', 'STOP', 'SWAP', 'SYSTEM',
      'THEN', 'TO',
      'USING',
      'WAIT', 'WHILE', 'WEND', 'WIDTH', 'WRITE'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    functions = [
      'ABS', 'ASC', 'ATN',
      'CDBL', 'CHR$', 'CINT', 'COS', 'CSNG', 'CVI', 'CVD', 'CVS',
      'DATE$',
      'EOF', 'EXP',
      'FIX', 'FRE',
      'HEX$',
      'INKEY', 'INP', 'INPUT$', 'INSTR', 'INT',
      'LEFT$', 'LEN', 'LOC', 'LOF', 'LOG', 'LPOS',
      'MID$', 'MKI$', 'MKD$', 'MKS$',
      'OCT$',
      'PEEK', 'POS',
      'RIGHT$',
      'SGN', 'SIN', 'SPACE$', 'SPC', 'SQR', 'STR$', 'STRING$',
      'TAB', 'TAN', 'TIME$',
      'USR',
      'VAL', 'VARPTR'
    ]

    function_tb = ListTokenBuilder(functions, 'function', False)

    user_function_tb = LongUserFunctionTokenBuilder('%#!$&')

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
      user_function_tb,
      variable_tb,
      groupers_tb,
      string_tb,
      remark_tb,
      comment_tb,
      comment2_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    self.convert_numbers_to_line_numbers()

    tokens = self.source_tokens()

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence(tokens)
    self.calc_operator_3_confidence(tokens, group_ends)
    self.calc_operator_4_confidence(tokens, group_starts)
    operand_types = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_confidence(tokens, operand_types)
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
    lines = self.split_tokens_into_lines(self.tokens)
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
