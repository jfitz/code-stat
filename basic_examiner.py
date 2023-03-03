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
  LeadToEndOfLineTokenBuilder,
  NullTokenBuilder
)
from basic_token_builders import (
  BasicVariableTokenBuilder,
  BasicLongVariableTokenBuilder,
  RemarkTokenBuilder,
  UserFunctionTokenBuilder,
  LongUserFunctionTokenBuilder,
  HardwareFunctionTokenBuilder
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
    NullTokenBuilder.__escape_z__()
    BasicVariableTokenBuilder.__escape_z__()
    BasicLongVariableTokenBuilder.__escape_z__()
    RemarkTokenBuilder.__escape_z__()
    UserFunctionTokenBuilder.__escape_z__()
    LongUserFunctionTokenBuilder.__escape_z__()
    HardwareFunctionTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, version):
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

    integer_suffix_tb = SuffixedIntegerTokenBuilder(
      ['%', '&', 'S', 'I', 'L', 'F', 'D', 'R', 'US', 'UI', 'UL'], True, '_')

    float_suffix_tb = SuffixedRealTokenBuilder(False, False, ['!', '#', 'F', 'D', 'R'], True, '_')

    if version in ['basic-80', 'basica', 'gw-basic']:
      double_exponent_tb = RealExponentTokenBuilder(False, False, 'D', '_')
      integer_suffix_tb = SuffixedIntegerTokenBuilder(['!', '%'], False, '_')
      float_suffix_tb = SuffixedRealTokenBuilder(False, False, ['!', '#'], True, '_')

    hex_constant_tb = PrefixedIntegerTokenBuilder('&H', True, '0123456789ABCDEFabcdef_')
    octal_constant_tb = PrefixedIntegerTokenBuilder('&O', True, '01234567_')
    binary_constant_tb = PrefixedIntegerTokenBuilder('&B', True, '01_')

    operand_types.append('number')

    variable_tb = BasicVariableTokenBuilder('%#!$&')

    if version in ['basic-80', 'basica', 'gw-basic']:
      variable_tb = BasicLongVariableTokenBuilder('%#!$&')

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
      '#', '\\', 'AND', 'OR', 'NOT'
    ]

    known_operators_ms = [
      '=>', '=<',
      'IMP', 'EQV', 'XOR', 'MOD'
    ]

    if version in ['basic-80', 'basica', 'gw-basic']:
      known_operators += known_operators_ms

    if version in ['basic']:
      known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)
    else:
      known_operator_tb = CaseInsensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-', '#', 'NOT'
    ]

    groupers = ['(', ')', ',', ';']
    group_starts = ['(']
    group_mids = [',', ';']
    group_ends = [')']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'CLOSE', 'CHAIN',
      'DATA', 'DEF', 'DIM',
      'ELSE', 'END', 'ERROR',
      'FILE', 'FOR',
      'GOSUB', 'GOTO',
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

    keywords_plain = [
      'AS',
      'GO'
    ]

    keywords_1965 = [
      'ARR', 'MAT',
      'BREAK',
      'CONTINUE',
      'FILES', 'FORGET',
      'OPTION', 'BASE',
      'PLOT',
      'WRITE'
    ]

    keywords_1973 = [
      'CHAIN', 'CHANGE',
      'ENDFN', 'ERROR',
      'FNEND',
      'RANDOMIZE', 'RESUME'
    ]

    keywords_1978 = [
      'CHAIN',
      'ENDFN', 'ERROR',
      'FNEND',
      'RANDOMIZE', 'RESUME'
    ]

    keywords_ms = [
      # 'AS',  ## promoted from variable after FIELD
      # 'BASE',  ## promoted from variable after OPTION
      'CALL', 'CLEAR', 'CLS', 'COMMON',
      'DEFDBL', 'DEFINT', 'DEFSNG', 'DEFSTR',
      'ELSE', 'END', 'ERASE', 'ERRLN', 'ERRNO', 'ERROR',
      'FIELD', 'FILES',
      'GET',
      'KILL',
      'LOAD', 'LPRINT', 'LSET',
      'MERGE',
      'NULL',
      'ONERR', 'OPTION', 'OUT',
      'PUT',
      'RESET', 'RESUME', 'RETURN', 'RSET', 'RUN',
      'SET', 'SWAP', 'SYSTEM',
      'TRON', 'TROFF',
      'WAIT', 'WHILE', 'WEND', 'WIDTH', 'WRITE'
    ]

    keywords_plus = [
      'CHANGE'
    ]

    if version in ['']:
      keywords += keywords_plain

    if version in ['basic-80', 'basica', 'gw-basic']:
      keywords += keywords_ms

    if version in ['basic-1965', 'basic-1973', 'basic-1978']:
      keywords += keywords_1965

    if version in ['basic-1973']:
      keywords += keywords_1973

    if version in ['basic-1978']:
      keywords += keywords_1978

    keywords_basica = [
      'COLOR', 'KEY', 'LOCATE', 'PAINT', 'PLAY', 'PSET',
      'SCREEN', 'SOUND'
    ]

    if version in ['basica', 'gw-basic']:
      keywords += keywords_basica

    if version in ['basic']:
      keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)
    else:
      keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    values = ['OFF', 'ON']

    values_tb = CaseInsensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    functions = [
      'ABS', 'ASC', 'ATN',
      'CHR', 'CHR$', 'CON', 'COS',
      'DET',
      'ERL', 'ERR', 'EXP',
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

    functions_ms = [
      'CDBL', 'CINT', 'CSNG', 'CVI', 'CVD', 'CVS',
      'DATE$',
      'EOF',
      'FIX', 'FRE',
      'HEX$',
      'INKEY', 'INP', 'INPUT$', 'INSTR',
      'LOC', 'LOF', 'LPOS',
      'MKI$', 'MKD$', 'MKS$',
      'OCT$',
      'PEEK',
      'SPACE$', 'SPC', 'STRING$',
      'TIME$',
      'USR',
      'VARPTR'
    ]

    functions_1965 = [
      'ARCCOS', 'ARCSIN', 'ARCTAN', 'AVG',
      'BROKEN',
      'CON1', 'CON2', 'COT', 'CSC',
      'DEG',
      'FIX', 'FRA',
      'MAXA', 'MAXM', 'MEDIAN', 'MINA', 'MINM', 'MOD',
      'NCOL', 'NELEM', 'NROW',
      'PROD',
      'RAD', 'RND1', 'RND2', 'REV1', 'ROUND',
      'SORT1', 'SORT2', 'SUM',
      'UNIQ1',
      'ZER1', 'ZER2'
    ]
 
    functions_1973 = [
      'ASC%', 'ASCII', 'ASCII%', 'CHR$', 'CON1%', 'CON1$', 'CON2%', 'CON2$',
      'ERL', 'ERR',
      'FIX%', 'FRAC', 'INSTR', 'INSTR%', 'INT%',
      'LEFT', 'LEN', 'LOWER$',
      'MAXA%', 'MAXA$', 'MAXM%', 'MAXM$', 'MEDIAN%', 'MEDIAN$', 'MINA%', 'MINA$',
      'MINM%', 'MINM$',
      'NCOL%', 'NELEM%', 'NROW%',
      'NUM', 'NUM$',
      'PACK$', 'PROD%',
      'RAD', 'RIGHT', 'RND%', 'RND$', 'RND1%', 'RND1$', 'RND2%', 'RND2$',
      'REV1%', 'REV1$',
      'SGN%', 'SORT1%', 'SORT1$', 'SORT2%', 'SORT2$', 'SPACE$', 'SPC', 'SPC$',
      'SPLIT1$', 'SUM%', 'STR$', 'STRING$',
      'TAB', 'TIME',
      'UNIQ1%', 'UNIQ1$', 'UNPACK', 'UNPACK%', 'UPPER$',
      'ZER1%', 'ZER1$', 'ZER2%', 'ZER2$'
    ]

    if version in ['basic-80', 'basica', 'gw-basic']:
      functions += functions_ms

    if version in ['basic-1965', 'basic-1973', 'basic-1978']:
      functions += functions_1965

    if version in ['basic-1973', 'basic-1978']:
      functions += functions_1973

    if version in ['basic']:
      function_tb = CaseSensitiveListTokenBuilder(functions, 'common function', True)
    else:
      function_tb = CaseInsensitiveListTokenBuilder(functions, 'common function', True)

    user_function_tb = UserFunctionTokenBuilder('%#!$&')
    hardware_function_tb = NullTokenBuilder()

    if version in ['basic-80', 'basica', 'gw-basic']:
      user_function_tb = LongUserFunctionTokenBuilder('%#!$&')
      hardware_function_tb = HardwareFunctionTokenBuilder()

    operand_types.append('common function')
    operand_types.append('function')

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
      double_exponent_tb,
      hex_constant_tb,
      octal_constant_tb,
      binary_constant_tb,
      keyword_tb,
      known_operator_tb,
      function_tb,
      user_function_tb,
      hardware_function_tb,
      values_tb,
      variable_tb,
      groupers_tb,
      string_tb,
      remark_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = BasicExaminer.convert_numbers_to_line_numbers(tokens)

    if version in ['basic-80']:
      tokens = BasicExaminer.extract_keywords_from_identifiers(tokens, keywords, known_operators)

    if version in ['basic-80', 'basica', 'gw-basic']:
      tokens = BasicExaminer.convert_as_to_keyword(tokens)
      tokens = BasicExaminer.convert_base_to_keyword(tokens)
      tokens = BasicExaminer.convert_operators_to_values(tokens)

    self.tokens = tokens
    # self.convert_identifiers_to_functions()
    # self.convert_functions_to_common_functions(functions)

    self.calc_statistics()

    tokens = self.source_tokens()

    self.calc_statistics()
    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)

      allow_pairs = []
      if version in ['basic-80', 'basica', 'gw-basic']:
        allow_pairs = [',', ',']

      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    if version not in ['basic-80', 'basica', 'gw-basic']:
      self.calc_group_confidence(tokens, group_mids)

    if version not in ['basic-80', 'basica', 'gw-basic']:
      operand_types_2 = ['number', 'string', 'variable', 'symbol']
      self.calc_operand_n_confidence(tokens, operand_types_2, 2)
      self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    self.calc_line_format_confidence()
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
  def extract_keywords(text, words):
    new_texts = []
    new_text = ''

    while len(text) > 0:
      found = False

      for word in words:
        if text.startswith(word):
          if len(new_text) > 0:
            new_texts.append(new_text)
            new_text = ''

          new_texts.append(word)
          # clip out keyword
          text = text[len(word):]
          found = True
          break

      if not found:
        new_text += text[0]
        text = text[1:]

    if len(new_text) > 0:
      new_texts.append(new_text)

    return new_texts


  @staticmethod
  def extract_lead_keywords(text, words):
    new_texts = []
    new_text = text + ''

    found = True

    while found:
      found = False

      for word in words:
        if new_text.startswith(word):
          new_texts.append(word)
          # clip out keyword
          new_text = new_text[len(word):]
          found = True
          break

    if len(new_text) > 0:
      new_texts.append(new_text)

    return new_texts


  @staticmethod
  def extract_keywords_from_identifiers(tokens, keywords, operators):
    new_tokens = []

    words = keywords

    for token in tokens:
      if token.group == 'variable':
        new_texts = BasicExaminer.extract_keywords(token.text, words)

        for new_text in new_texts:
          if new_text in keywords:
            new_token = Token(new_text, 'keyword', False)
          elif new_text in operators:
            new_token = Token(new_text, 'operator', False)
          else:
            if new_text.isdigit():
              new_token = Token(new_text, 'number', True)
            else:
              new_token = Token(new_text, 'variable', True)

          new_tokens.append(new_token)
      else:
        new_tokens.append(token)

    return new_tokens


  @staticmethod
  def convert_as_to_keyword(tokens):
    seen_field = False

    for token in tokens:
      if token.group == 'newline':
        seen_field = False

      if token.group == 'keyword' and token.text.lower() == 'field':
        seen_field = True

      if token.group == 'variable' and token.text.lower() == 'as' and seen_field:
        token.group = 'keyword'

    return tokens


  @staticmethod
  def convert_base_to_keyword(tokens):
    prev_token = Token('\n', 'newline', False)

    for token in tokens:
      if token.group == 'variable' and token.text.lower() == 'base' and \
        prev_token.group == 'keyword' and prev_token.text.lower() == 'option':
        token.group = 'keyword'

      if token.group not in ['whitespace', 'comment']:
        prev_token = token

    return tokens


  # Convert operators to values, because in certain statements these
  # operators are used without values
  @staticmethod
  def convert_operators_to_values(tokens):
    prev_token = Token('\n', 'newline', False)
    seen_put = False

    for token in tokens:
      if token.group == 'newline':
        seen_put = False

      if token.group == 'keyword' and token.text.lower() == 'put':
        seen_put = True

      # TODO: check not in open parens
      if token.group == 'operator' and \
        token.text.lower() in ['and', 'or', 'xor'] and \
        seen_put and \
        prev_token.text == ',':
        token.group = 'value'

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
