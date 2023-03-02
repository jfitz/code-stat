import string

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StuffedQuoteStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  PrefixedIntegerTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from basic_token_builders import (
  RemarkTokenBuilder
)
from cbasic_token_builders import (
  CBasicVariableTokenBuilder,
  CBasicLabelTokenBuilder,
  CBasicSuffixedIntegerTokenBuilder,
  CBasicLineContinuationTokenBuilder
)
from examiner import Examiner

class CBasicExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StuffedQuoteStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    RemarkTokenBuilder.__escape_z__()
    CBasicVariableTokenBuilder.__escape_z__()
    CBasicLabelTokenBuilder.__escape_z__()
    CBasicSuffixedIntegerTokenBuilder.__escape_z__()
    CBasicLineContinuationTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    line_continuation_tb = CBasicLineContinuationTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    hex_constant_tb = CBasicSuffixedIntegerTokenBuilder('0123456789ABCDEF', 'H')
    binary_constant_tb = CBasicSuffixedIntegerTokenBuilder('01', 'B')
    operand_types.append('number')

    variable_tb = CBasicVariableTokenBuilder('%$')
    operand_types.append('variable')

    quotes = ['"']
    string_tb = StuffedQuoteStringTokenBuilder(quotes, False)
    operand_types.append('string')

    remark_tb = RemarkTokenBuilder()
    comment_tb = LeadToEndOfLineTokenBuilder("'", False, 'comment')

    stmt_separator_tb = SingleCharacterTokenBuilder(':', 'statement separator', False)

    known_operators = [
      '+', '-', '*', '/', '^',
      '=', '>', '>=', '<', '<=', '<>',
      '#', 'NOT',
      'AND', 'EQ', 'GE', 'GT', 'LE', 'LT', 'NE', 'OR', 'XOR',
      '?'
    ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-', '#', 'NOT',
      '?'
    ]

    groupers = ['(', ')', ',', ';']
    group_starts = ['(', ',']
    group_mids = [',']
    group_ends = [')']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'AS', 'BUFF', 'CALL',  'CHAIN', 'CLOSE', 'COMMON', 'CONSOLE', 'CREATE',
      'DATA', 'DEF', 'DELETE', 'DIM', 'ELSE', 'END', 'FEND',
      'FILE', 'FOR', 'GOSUB', 'GO', 'GOTO', 'IF', 'INITIALIZE',
      'INPUT', 'INTEGER', 'LET', 'LINE', 'LPRINTER', 'NEXT', 
      'ON', 'OPEN', 'OUT', 'POKE', 'PRINT',
      'RANDOMIZE', 'READ', 'REM', 'REMARK', 'RENAME',
      'RESTORE', 'RETURN', 'SAVEMEM', 'STEP', 'STOP', 'SUB',
      'THEN', 'TO', 'USING', 'WEND', 'WHILE', 'WIDTH',
      'GRAPHIC', 'MAT', 'FILL', 'MAT', 'MARKER', 'PLOT', 'CHARACTER',
      'HEIGHT', 'SET', 'ASK', 'COLOR', 'COUNT', 'JUSTIFY', 'LINE', 'STYLE',
      'TYPE', 'TEXT', 'ANGLE', 'BOUNDS', 'DEVICE', 'VIEWPORT', 'WINDOW',
      'BEAM', 'CLEAR', 'CLIP', 'POSITION'
    ]

    keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    label_tb = CBasicLabelTokenBuilder(keywords)

    functions = [
      'ASC', 'CHR$', 'STR$', 'TAB', 'COMMAND$', 'CONCHAR%', 'CONSTAT%',
      'ATN', 'COS', 'SIN', 'TAN',
      'ABS', 'EXP', 'INT', 'FLOAT', 'LOG', 'RND', 'SGN', 'SQR',
      'LEFT$', 'LEN', 'MID$', 'RIGHT$', 'MATCH', 'VAL',
      'FRE', 'INP', 'INT%', 'PEEK', 'POS', 'TAB',
      'RECL', 'RECS', 'SADD', 'SIZE', 'UCASE$', 'VARPTR'
    ]

    function_tb = CaseInsensitiveListTokenBuilder(functions, 'common function', True)
    operand_types.append('common function')
    operand_types.append('function')

    directives = [
      '%LIST', '%NOLIST',
      '%PAGE', '%EJECT',
      '%INCLUDE', '%CHAIN'
    ]

    directive_tb = CaseInsensitiveListTokenBuilder(directives, 'directive', False)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      hex_constant_tb,
      binary_constant_tb,
      keyword_tb,
      known_operator_tb,
      function_tb,
      variable_tb,
      label_tb,
      groupers_tb,
      string_tb,
      remark_tb,
      comment_tb,
      directive_tb,
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
    self.convert_numbers_to_line_numbers()
    # self.convert_identifiers_to_functions()
    # self.convert_functions_to_common_functions(functions)

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_line_continuation_confidence(tokens)

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'string', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    self.calc_line_format_confidence()
    self.calc_line_length_confidence(code, self.max_expected_line)


  def convert_numbers_to_line_numbers(self):
    prev_token = Token('\n', 'newline', False)

    prev_line_continuation = False
    line_continuation = False

    for token in self.tokens:
      if token.group == 'number' and\
        prev_token.group == 'newline' and\
        not prev_line_continuation:
        token.group = 'line number'

      if token.group == 'line continuation':
        line_continuation = True

      if token.group not in ['whitespace', 'comment']:
        prev_token = token

      if token.group == 'newline':
        prev_line_continuation = line_continuation
        line_continuation = False


  # line numbers reduce confidence
  def calc_line_format_confidence(self):
    lines = self.split_tokens_into_lines(self.tokens)
    num_lines = 0
    num_lines_correct = 0

    for line in lines:
      if len(line) > 0:
        num_lines += 1

        if line[0].group == 'line number':
          num_lines_correct += 1
    
    percentage_line_with_numbers = 0.0

    if num_lines > 0:
      percentage_line_with_numbers = num_lines_correct / num_lines

    line_format_confidence = 1.0

    if percentage_line_with_numbers > 0.5:
      line_format_confidence = 1.0 - (percentage_line_with_numbers * 0.1)

    self.confidences['line format'] = line_format_confidence
