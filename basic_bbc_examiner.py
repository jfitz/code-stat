import string

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  NullTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StuffedQuoteStringTokenBuilder,
  IntegerTokenBuilder,
  SuffixedIntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  SuffixedRealTokenBuilder,
  RealExponentTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  PrefixedIntegerTokenBuilder
)
from basic_token_builders import (
  BasicLongVariableTokenBuilder,
  RemarkTokenBuilder,
  LongUserFunctionTokenBuilder,
  LongProcNameTokenBuilder
)
from examiner import Examiner

class BasicBbcExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    NullTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StuffedQuoteStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    SuffixedIntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    SuffixedRealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    BasicLongVariableTokenBuilder.__escape_z__()
    RemarkTokenBuilder.__escape_z__()
    LongUserFunctionTokenBuilder.__escape_z__()
    LongProcNameTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()
    self.newlines_important = 'always'

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder(False)
    real_tb = RealTokenBuilder(False, False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', '_')
    double_exponent_tb = NullTokenBuilder()
    integer_suffix_tb = SuffixedIntegerTokenBuilder(['%', '&', 'S', 'I', 'L', 'F', 'D', 'R', 'US', 'UI', 'UL'], True, '_')
    float_suffix_tb = SuffixedRealTokenBuilder(False, False, ['!', '#', 'F', 'D', 'R'], True, '_')

    double_exponent_tb = RealExponentTokenBuilder(False, False, 'D', '_')
    integer_suffix_tb = SuffixedIntegerTokenBuilder(['%'], False, '_')
    float_suffix_tb = SuffixedRealTokenBuilder(False, False, ['!', '#'], True, '_')

    hex_constant_tb = PrefixedIntegerTokenBuilder('&H', True, '0123456789ABCDEFabcdef_')
    octal_constant_tb = PrefixedIntegerTokenBuilder('&O', True, '01234567_')
    binary_constant_tb = PrefixedIntegerTokenBuilder('&B', True, '01_')

    operand_types.append('number')

    variable_tb = BasicLongVariableTokenBuilder('%#!$&')

    operand_types.append('variable')

    quotes = ['"']
    string_tb = StuffedQuoteStringTokenBuilder(quotes, False)
    operand_types.append('string')

    remark_tb = RemarkTokenBuilder()

    known_operators = [
      '*', '**', '*|', '#', '%', '~',
      '>>', '?', '!', ']', '|', '$', '$$',
      '>>>', '<<<',
      '=', '>', '>=', '<', '<=', '<>',
      '+', '-', '/',
      'AND', 'DIV', 'MOD', 'OR',
      'AND=', 'DIV=', 'MOD=', 'OR=',
      'NOT', '.'
    ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '?', '!', ']', '|', '$', '$$', 'NOT', '+', '-'
    ]

    groupers = ['(', ')', ',', ';', ':']
    group_starts = ['(']
    group_mids = [',', ';']
    group_ends = [')']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'BGET#', 'BPUT#',
      'CALL', 'CASE', 'CHAIN', 'CIRCLE', 'CLEAR', 'CLG', 'CLOSE#', 'CLS',
      'COLOR', 'COLOUR',
      'DATA', 'DEF', 'DIM', 'DRAW',
      'EDIT', 'ELLIPSE', 'ELSE', 'END', 'ENDCASE', 'ENDIF', 'ENDWHILE',
      'ENDPROC', 'ENVELOPE', 'ERROR', 'EVAL',
      'FILL', 'FOR',
      'GCOL', 'GOSUB', 'GOTO',
      'IF', 'INPUT', 'INPUT#',
      'LET', 'LINE', 'LOCAL',
      'MODE', 'MOUSE', 'MOVE',
      'NEXT',
      'OF', 'OPENIN', 'OPENOUT', 'OPENUP', 'OPT', 'ORIGIN', 'OSCLI',
      'OTHERWISE',
      'PAGE', 'PLOT', 'POINT', 'PRINT', 'PRINT#', 'PROC', 'PTR#',
      'QUIT',
      'RECTANGLE', 'READ', 'READ#', 'REM', 'REPEAT', 'REPORT', 'RESTORE',
      'RETURN', 'RUN',
      'SOUND', 'STEP', 'STOP', 'SUM', 'SWAP', 'SYS',
      'THEN', 'TIME', 'TIME$', 'TO', 'TRACE',
      'UNTIL', 'USR',
      'VDU',
      'WHEN', 'WHILE', 'WIDTH'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    values = [
      'ERL', 'ERR', 'FALSE', 'TRUE', 'OFF', 'ON', 'PI'
    ]

    values_tb = CaseInsensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    functions = [
      'ABS',  'ASC', 'ATN', 'ACS', 'ADVAL', 'ASN',
      'CHR$', 'COS', 'COUNT',
      'DEG',
      'EXP', 'EOR', 'EOF#', 'EXT#',
      'GET', 'GET$',
      'HIMEM',
      'INKEY', 'INKEY$', 'INSTR', 'INT',
      'LEFT$', 'LEN', 'LN', 'LOG', 'LOMEM',
      'MID$',
      'POS',
      'RAD', 'RIGHT$', 'RND',
      'SGN', 'SIN', 'SPC', 'SQR', 'STR$', 'STRING$',
      'TAB', 'TAN', 'TINT', 'TOP',
      'VAL', 'VPOS'
    ]

    function_tb = CaseInsensitiveListTokenBuilder(functions, 'common function', True)
    user_function_tb = LongUserFunctionTokenBuilder('%#!$&')
    proc_name_tb = LongProcNameTokenBuilder('%#!$&')

    operand_types.append('common function')
    operand_types.append('function')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      integer_tb,
      integer_exponent_tb,
      float_suffix_tb,
      integer_suffix_tb,
      real_tb,
      real_exponent_tb,
      double_exponent_tb,
      hex_constant_tb,
      octal_constant_tb,
      binary_constant_tb,
      keyword_tb,
      known_operator_tb,
      function_tb,
      user_function_tb,
      proc_name_tb,
      values_tb,
      variable_tb,
      groupers_tb,
      string_tb,
      remark_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    self.tokens = tokens

    self.calc_statistics()

    tokens = self.source_tokens()

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = [['\n', '='], ['\r\n', '=']]
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'string', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    # self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_length_confidence(code, self.max_expected_line)


  @staticmethod
  def convert_numbers_to_line_numbers(tokens):
    prev_token = Token('\n', 'newline', False)

    kwds = ['goto', 'gosub', 'then']

    for token in tokens:
      if token.group == 'number' and prev_token.group == 'newline':
        token.group = 'line number'

      if token.group == 'number' and \
        prev_token.group == 'keyword' and prev_token.text.lower() in kwds:
        token.group = 'line number'

      if token.group not in ['whitespace', 'comment']:
        prev_token = token

    return tokens


  @staticmethod
  def convert_values_to_functions(tokens, values):
    prev_token = Token('\n', 'newline', False)

    for token in tokens:
      if token.group == 'group' and token.text == '(' and \
        prev_token.group == 'value' and prev_token.text in values:
        prev_token.group = 'function'

      if token.group not in ['whitespace', 'comment']:
        prev_token = token

    return tokens
