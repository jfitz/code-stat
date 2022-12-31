from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  IdentifierTokenBuilder,
  StuffedQuoteStringTokenBuilder,
  PrefixedStringTokenBuilder,
  SingleCharacterTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  LeadToEndOfLineTokenBuilder,
  NullTokenBuilder
)
from fortran_token_builders import (
  FortranIdentifierTokenBuilder,
  FormatSpecifierTokenBuilder,
  HollerithStringTokenBuilder,
  UserDefinedOperatorTokenBuilder,
  KindIntegerTokenBuilder,
  KindRealTokenBuilder
)
from jcl_token_builders import (
  JCLTokenBuilder
)
from examiner import Examiner


class FortranExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, year, tab_size, format):
    super().__init__()
    self.newlines_important = 'always'

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    double_exponent_tb = RealExponentTokenBuilder(False, False, 'D', None)

    jcl_tb = JCLTokenBuilder()

    invalid_token_builder = InvalidTokenBuilder()

    # FORTRAN-66 should be upper case only
    # FORTRAN-77 may be upper or lower case
    case_significant = year in ['66', '1966']

    operand_types = []

    if year in ['66', '1966', '77', '1977']:
      kind_integer_tb = NullTokenBuilder()
      kind_real_tb = NullTokenBuilder()
    else:
      kind_integer_tb = KindIntegerTokenBuilder()
      kind_real_tb = KindRealTokenBuilder()

    operand_types.append('number')

    if year in ['66', '1966']:
      identifier_tb = FortranIdentifierTokenBuilder()
    elif year in ['77', '1977']:
      leads = ''
      extras = ''
      identifier_tb = IdentifierTokenBuilder(leads, extras)
    else:
      leads = '_'
      extras = '_'
      identifier_tb = IdentifierTokenBuilder(leads, extras)

    operand_types.append('identifier')

    if year in ['66', '1966', '77', '1977']:
      bang_comment_tb = NullTokenBuilder()
    else:
      bang_comment_tb = LeadToEndOfLineTokenBuilder('!', False, 'comment')

    quotes = ['"', "'"]

    if year in ['66', '1966', '77', '1977']:
      if year in ['66', '1966']:
        hollerith_tb = HollerithStringTokenBuilder()
        string_tb = NullTokenBuilder()
      else:
        hollerith_tb = NullTokenBuilder()
        string_tb = StuffedQuoteStringTokenBuilder(quotes, False)

      binary_string_tb = NullTokenBuilder()
      octal_string_tb = NullTokenBuilder()
      hex_string_tb = NullTokenBuilder()
    else:
      hollerith_tb = NullTokenBuilder()
      string_tb = StuffedQuoteStringTokenBuilder(quotes, False)
      binary_string_tb = PrefixedStringTokenBuilder('B', False, quotes)
      octal_string_tb = PrefixedStringTokenBuilder('O', False, quotes)
      hex_string_tb = PrefixedStringTokenBuilder('Z', False, quotes)

    operand_types.append('string')

    if year in ['66', '1966', '77', '1977']:
      format_tb = FormatSpecifierTokenBuilder()
    else:
      format_tb = NullTokenBuilder()

    if year in ['66', '1966', '77', '1977']:
      known_operators = [
        '=', '+', '-', '*', '/', '**',
        '.EQ.', '.GT.', '.GE.', '.LT.', '.LE.', '.NE.',
        '.AND.', '.OR.', '.NOT.'
      ]
    else:
      known_operators = [
        '=', '+', '-', '*', '/', '**',
        '==', '>', '>=', '<', '<=', '/=',
        '.EQ.', '.NE.', '.LT.', '.LE.', '.GT.', '.GE.',
        '.AND.', '.OR.', '.NOT.', '.EQV.', '.NEQV.',
        ':', '::', '=>', '%'
      ]

    if case_significant:
      known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)
    else:
      known_operator_tb = CaseInsensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-', '.NOT.'
    ]

    if year in ['66', '1966', '77', '1977']:
      user_operator_tb = NullTokenBuilder()
      continuation_tb = NullTokenBuilder()
      stmt_separator_tb = NullTokenBuilder()
    else:
      user_operator_tb = UserDefinedOperatorTokenBuilder()
      continuation_tb = SingleCharacterTokenBuilder('&', 'line continuation', False)
      stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator', False)

    if year in ['66', '1966', '77', '1977']:
      groupers = ['(', ')', ',']
      # group_starts = ['(', ',']
      group_mids = [',']
      # group_ends = [')']
    else:
      groupers = ['(', ')', ',', '[', ']']
      # group_starts = ['(', '[', ',', '{']
      group_mids = [',']
      # group_ends = [')', ']', '}']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'IF', 'GO', 'TO', 'GOTO', 'GO TO', 'ASSIGN',
      'READ', 'WRITE', 'BACKSPACE', 'REWIND', 'ENDFILE', 'FORMAT',
      'DO', 'CONTINUE',
      'SUBROUTINE', 'FUNCTION', 'BLOCK DATA',
      'COMMON', 'DIMENSION', 'EQUIVALENCE',
      'DATA', 'EXTERNAL',
      'CALL', 'RETURN', 'PAUSE', 'STOP', 'END'
    ]

    keywords_77 = [
      'THEN', 'ELSE', 'ENDIF', 'END IF',
      'PRINT', 'PROGRAM',
      'IMPLICIT', 'SAVE',
      'INQUIRE', 'INTRINSIC', 'PARAMETER'
    ]

    keywords_90 = [
      'DATA', 'EXTERNAL',
      'allocate', 'case', 'contains', 'cycle', 'deallocate',
      'elsewhere', 'exit', 'include', 'interface', 'intent', 'kind', 'module',
      'namelist', 'nullify', 'only', 'operator', 'optional', 'pointer',
      'private', 'procedure', 'public', 'recursive', 'result', 'select',
      'sequence', 'target', 'type', 'use', 'while', 'where',
      'enddo', 'end do', 'none'
    ]

    keywords_95 = [
      'FORALL', 'PURE', 'ELEMENTAL'
    ]

    keywords_2003 = [
      'abstract', 'allocatable', 'associate', 'bind', 'class', 'enum', 'end enum',
      'import', 'protected', 'select type', 'type guard', 'value', 'wait'
    ]

    keywords_2008 = [
      'block', 'contiguous'
    ]

    if year in ['77', '1977', '90', '1990', '95', '2003', '2008']:
      keywords += keywords_77

    if year in ['90', '1990', '95', '2003', '2008']:
      keywords += keywords_90

    if year in ['95', '2003', '2008']:
      keywords += keywords_95

    if year in ['2003', '2008']:
      keywords += keywords_2003

    if year in ['2008']:
      keywords += keywords_2008

    if case_significant:
      keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)
    else:
      keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    if year in ['66', '1966']:
      types = [
        'INTEGER', 'REAL', 'COMPLEX', 'DOUBLE PRECISION', 'DOUBLEPRECISION',
        'LOGICAL'
      ]
    else:
      types = [
        'INTEGER', 'REAL', 'COMPLEX', 'DOUBLE PRECISION', 'DOUBLEPRECISION',
        'DOUBLE', 'LOGICAL', 'CHARACTER'
      ]

    if case_significant:
      types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    else:
      types_tb = CaseInsensitiveListTokenBuilder(types, 'type', True)

    operand_types.append('type')

    # tokenize as free-format
    tokenbuilders_free = [
      newline_tb,
      whitespace_tb,
      continuation_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      kind_integer_tb,
      real_tb,
      real_exponent_tb,
      double_exponent_tb,
      kind_real_tb,
      keyword_tb,
      types_tb,
      known_operator_tb,
      user_operator_tb,
      groupers_tb,
      identifier_tb,
      string_tb,
      binary_string_tb,
      octal_string_tb,
      hex_string_tb,
      bang_comment_tb,
      jcl_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer_free = Tokenizer(tokenbuilders_free)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens_free = tokenizer_free.tokenize(ascii_code)

    tokens_free = Examiner.combine_adjacent_identical_tokens(tokens_free, 'invalid operator')
    tokens_free = Examiner.combine_adjacent_identical_tokens(tokens_free, 'invalid')
    tokens_free = Examiner.combine_identifier_colon(tokens_free, ['newline'], [], ['whitespace', 'comment', 'line identification'])
    self.tokens = tokens_free

    self.convert_identifiers_after_goto_to_labels()
    self.convert_numbers_to_line_numbers()
    self.convert_stars_to_io_channels()

    self.calc_statistics()
    self.statistics['format'] = 'free'
    statistics_free = self.statistics.copy()

    tokens = self.source_tokens()

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      # self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      # self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()
    self.calc_line_length_confidence(code, self.max_expected_line)

    confidences_free = self.confidences.copy()
    self.confidences = {}
    errors_free = self.errors.copy()
    self.errors = []

    # tokenize as fixed-format
    tokenbuilders_fixed = [
      newline_tb,
      whitespace_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      double_exponent_tb,
      keyword_tb,
      types_tb,
      format_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      bang_comment_tb,
      hollerith_tb,
      string_tb,
      jcl_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer_fixed = Tokenizer(tokenbuilders_fixed)

    # code = self.TrimCtrlZText(code)
    # ascii_code = self.convert_to_ascii(code)
    tokens_fixed = self.tokenize_code(ascii_code, tab_size, tokenizer_fixed)

    tokens_fixed = Examiner.combine_adjacent_identical_tokens(tokens_fixed, 'invalid operator')
    tokens_fixed = Examiner.combine_adjacent_identical_tokens(tokens_fixed, 'invalid')
    tokens_fixed = Examiner.combine_adjacent_identical_tokens(tokens_fixed, 'whitespace')

    self.tokens = tokens_fixed

    self.convert_identifiers_after_goto_to_labels()
    self.convert_numbers_to_line_numbers()
    self.convert_stars_to_io_channels()

    self.calc_statistics()
    self.statistics['format'] = 'fixed'
    statistics_fixed = self.statistics.copy()

    tokens = self.source_tokens()

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      # self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      # self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()
    self.calc_line_length_confidence(code, self.max_expected_line)
    self.calc_line_description_confidence()

    confidences_fixed = self.confidences.copy()
    self.confidences = {}
    errors_fixed = self.errors.copy()
    self.errors = []

    # compute confidence for free-format and fixed-format
    confidence_free = 1.0
    if len(confidences_free) == 0:
      confidence_free = 0.0
    else:
      for key in confidences_free:
        factor = confidences_free[key]
        confidence_free *= factor

    confidence_fixed = 1.0
    if len(confidences_fixed) == 0:
      confidence_fixed = 0.0
    else:
      for key in confidences_fixed:
        factor = confidences_fixed[key]
        confidence_fixed *= factor

    if format == 'better':
      # select the better of free-format and spaced-format
      if confidence_fixed > confidence_free:
        self.tokens = tokens_fixed
        self.statistics = statistics_fixed
        self.confidences = confidences_fixed
        self.errors = errors_fixed
      else:
        self.tokens = tokens_free
        self.statistics = statistics_free
        self.confidences = confidences_free
        self.errors = errors_free

    if format == 'fixed':
      # select the fixed-format values
      self.tokens = tokens_fixed
      self.statistics = statistics_fixed
      self.confidences = confidences_fixed
      self.errors = errors_fixed

    if format == 'free':
      # select the free-format values
      self.tokens = tokens_free
      self.statistics = statistics_free
      self.confidences = confidences_free
      self.errors = errors_free


  def convert_numbers_to_line_numbers(self):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'number' and\
        prev_token.group == 'newline':
        token.group = 'line number'

      if token.group not in ['whitespace', 'comment', 'line identification']:
        prev_token = token


  def convert_stars_to_io_channels(self):
    prev_tokens = [
      Token('\n', 'newline', False),
      Token('\n', 'newline', False),
      Token('\n', 'newline', False),
      Token('\n', 'newline', False)
    ]

    for token in self.tokens:
      if token.group == 'operator' and token.text == '*' and\
        prev_tokens[-1].group == 'keyword' and\
        prev_tokens[-1].text.lower() in ['read', 'write', 'print']:
        token.group = 'number'

      if token.group == 'operator' and token.text == '*' and\
        prev_tokens[-1].group == 'group' and\
        prev_tokens[-1].text == '(' and\
        prev_tokens[-2].group == 'keyword' and\
        prev_tokens[-2].text.lower() in ['read', 'write', 'print']:
        token.group = 'number'

      if token.group == 'operator' and token.text == '*' and\
        prev_tokens[-1].group == 'group' and\
        prev_tokens[-1].text == ',' and\
        prev_tokens[-2].group == 'number' and\
        prev_tokens[-3].group == 'group' and\
        prev_tokens[-3].text == '(' and\
        prev_tokens[-4].group == 'keyword' and\
        prev_tokens[-4].text.lower() in ['read', 'write', 'print']:
        token.group = 'number'

      if token.group not in ['whitespace', 'comment', 'line identification']:
        prev_tokens.append(token)
        prev_tokens.pop(0)


  def unwrapped_code(self, lines):
    unwrapped_lines = ''

    buffer = None
    for line in lines:
      # rtrim
      line = line.rstrip()

      # if continuation (not comment, longer than 6, not space in column 5)
      if len(line) > 5 and line[0] != 'C' and line[5] != ' ':
        # drop leading columns
        line = line[6:]
        # append to buffer
        if buffer is None:
          buffer = line
        else:
          buffer += line
      else:
        if buffer is not None:
          unwrapped_lines += buffer
          unwrapped_lines += '\n'
        buffer = line
    if len(buffer) > 0:
      unwrapped_lines += buffer
      unwrapped_lines += '\n'

    return unwrapped_lines


  def tokenize_line_number(self, line_number):
    tokens = []

    if len(line_number) > 0:
      if line_number.isspace():
        tokens.append(Token(line_number, 'whitespace', False))
      else:
        if line_number.isdigit():
          tokens.append(Token(line_number, 'line number', True))
        else:
          ln_2 = line_number.lstrip()
          ln_3 = ln_2.rstrip()

          front_space = ''
          front_count = len(line_number) - len(ln_2)
          if front_count > 0:
            front_space = ' ' * front_count
            tokens.append(Token(front_space, 'whitespace', False))

          if ln_3.strip().isdigit():
            tokens.append(Token(ln_3, 'line number', False))
          else:
            tokens.append(Token(line_number, 'invalid', False))

          back_space = ''
          back_count = len(ln_2) - len(ln_3)
          if back_count > 0:
            back_space = ' ' * back_count
            tokens.append(Token(back_space, 'whitespace', False))

    return tokens


  def tokenize_line(self, line, tokenizer):
    # break apart the line based on fixed format
    tokens = []

    # The fixed-format FORTRAN line format is:
    # 1: space or C or *
    # 2-6: line number or blank
    # 7: continuation character
    # 8-72: program text
    # 73-: identification, traditionally sequence number (ignored)

    if line.startswith(('//', '/*')):
      tokens.append(Token(line, 'jcl', False))
    else:
      line_indicator = line[0:1]
      line_number = line[1:5]
      line_continuation = line[5:6]
      line_text = line[6:72]
      line_identification = line[72:]

      # tokenize the line indicator
      if line_indicator in ['C', '*']:
        tokens.append(Token(line, 'comment', False))
      else:
        if len(line_indicator) > 0 and line_indicator != ' ':
          tokens.append(Token(line, 'invalid', False))
        else:
          tokens += self.tokenize_line_number(line_number)

          # tokenize line continuation character
          if len(line_continuation) > 0:
            if line_continuation.isspace():
              tokens.append(Token(line_continuation, 'whitespace', False))
            else:
              tokens.append(Token(line_continuation, 'line continuation', False))

          # tokenize the code
          tokens += tokenizer.tokenize(line_text)

          # tokenize the line identification
          if len(line_identification) > 0:
            tokens.append(Token(line_identification, 'line identification', False))

    tokens.append(Token('\n', 'newline', False))

    return tokens


  def tokenize_code(self, code, tab_size, tokenizer):
    lines = code.split('\n')

    tokens = []

    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = Examiner.tabs_to_spaces(line, tab_size)

      line_tokens = self.tokenize_line(line, tokenizer)
      tokens += line_tokens

    return tokens
