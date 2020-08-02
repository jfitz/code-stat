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
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  PrefixedIntegerTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from basic_token_builders import (
  BasicVariableTokenBuilder,
  RemarkTokenBuilder,
  UserFunctionTokenBuilder
)
from examiner import Examiner

class BasicExaminer(Examiner):
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
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    BasicVariableTokenBuilder.__escape_z__()
    RemarkTokenBuilder.__escape_z__()
    UserFunctionTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder(False)
    real_tb = RealTokenBuilder(False, False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', '_')
    integer_suffix_tb = SuffixedIntegerTokenBuilder(['%', '&', 'S', 'I', 'L', 'F', 'D', 'R', 'US', 'UI', 'UL'], True, '_')
    float_suffix_tb = SuffixedRealTokenBuilder(False, False, ['!', '#', 'F', 'D', 'R'], True, '_')
    hex_constant_tb = PrefixedIntegerTokenBuilder('&H', True, '0123456789ABCDEFabcdef_')
    octal_constant_tb = PrefixedIntegerTokenBuilder('&O', True, '01234567_')
    binary_constant_tb = PrefixedIntegerTokenBuilder('&B', True, '01_')
    operand_types.append('number')

    variable_tb = BasicVariableTokenBuilder('%#!$&')
    operand_types.append('variable')

    quotes = ['"']
    string_tb = StuffedQuoteStringTokenBuilder(quotes, False)
    operand_types.append('string')

    remark_tb = RemarkTokenBuilder()
    comment_tb = LeadToEndOfLineTokenBuilder("'", False, 'comment')
    comment2_tb = LeadToEndOfLineTokenBuilder("’", False, 'comment')

    stmt_separator_tb = SingleCharacterTokenBuilder(':', 'statement separator', False)

    known_operators = [
      '+', '-', '*', '/', '^',
      '=', '>', '>=', '<', '<=', '<>',
      '#', '\\', '#', 'AND', 'OR', 'NOT'
    ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-', '#', 'NOT'
    ]

    groupers = ['(', ')', ',', ';']
    group_starts = ['(']
    group_mids = [',', ';']
    group_ends = [')']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'AS',
      'CHANGE', 'CLOSE',
      'DATA', 'DEF', 'DIM',
      'ELSE', 'END', 'ERROR', 'ERRNO', 'ERRLN',
      'FILE', 'FOR',
      'GOSUB', 'GO', 'GOTO',
      'IF', 'INPUT',
      'LET', 'LINE',
      'MAT',
      'NEXT', 
      'ON', 'ONERR', 'OPEN', 'OUTPUT',
      'POKE', 'PRINT',
      'RANDOMIZE', 'READ', 'REM', 'REMARK', 'RESTORE', 'RETURN',
      'STEP', 'STOP',
      'THEN', 'TO',
      'USING'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    functions = [
      'ABS',
      'ASC',
      'ATN',
      'CHR', 'CHR$', 'CON', 'COS',
      'DET',
      'EXP',
      'IDN', 'INSTR', 'INT', 'INV',
      'LEFT', 'LEFT$', 'LEN', 'LOG',
      'MID', 'MID$',
      'POS',
      'RIGHT', 'RIGHT$', 'RND',
      'SGN', 'SIN', 'SQR', 'STR$',
      'TAB', 'TAN', 'TRN',
      'VAL',
      'ZER'
    ]

    function_tb = CaseInsensitiveListTokenBuilder(functions, 'function', True)
    operand_types.append('function')

    user_function_tb = UserFunctionTokenBuilder('%#!$&')

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

    operand_types = ['number', 'string', 'symbol', 'identifier', 'variable', 'function']

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = BasicExaminer.convert_numbers_to_line_numbers(tokens)
    self.tokens = tokens

    self.calc_statistics()

    tokens = self.source_tokens()

    self.calc_statistics()
    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator'])
    if num_operators > 0:
      self.calc_operator_confidence()
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, allow_pairs)
      self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'string', 'variable', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    self.calc_line_format_confidence()


  @staticmethod
  def convert_numbers_to_line_numbers(tokens):
    prev_token = Token('\n', 'newline', False)

    for token in tokens:
      if token.group == 'number' and prev_token.group == 'newline':
        token.group = 'line number'

      if token.group not in ['whitespace', 'comment', 'line description']:
        prev_token = token

    return tokens


  # check each line begins with a line number
  def calc_line_format_confidence(self):
    lines = self.split_tokens_into_lines(self.tokens)
    num_lines = 0
    num_lines_correct = 0

    for line in lines:
      if len(line) > 0:
        num_lines += 1

        if line[0].group == 'line number':
          num_lines_correct += 1
        elif len(line) > 1 and \
          line[0].group == 'whitespace' and line[1].group == 'line number':
          num_lines_correct += 1

    line_format_confidence = 0.0

    if num_lines > 0:
      line_format_confidence = num_lines_correct / num_lines

    self.confidences['line format'] = line_format_confidence
