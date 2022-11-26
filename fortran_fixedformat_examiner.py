import string

from codestat_exception import CodeStatException
from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  CaseSensitiveListTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  IdentifierTokenBuilder,
  StuffedQuoteStringTokenBuilder
)
from fortran_token_builders import (
  FortranIdentifierTokenBuilder,
  FormatSpecifierTokenBuilder,
  HollerithStringTokenBuilder
)
from fortran_examiner import FortranExaminer
from examiner import Examiner

class FortranFixedFormatExaminer(FortranExaminer):
  @staticmethod
  def __escape_z__():
    CaseSensitiveListTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    StuffedQuoteStringTokenBuilder.__escape_z__()
    FortranIdentifierTokenBuilder.__escape_z__()
    FormatSpecifierTokenBuilder.__escape_z__()
    HollerithStringTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, year, tab_size):
    super().__init__()
    self.max_expected_line = 80

    if year is not None and year not in ['66', '1966', '77', '1977']:
      raise CodeStatException('Unknown year for language')

    # FORTRAN-66 should be upper case only
    # FORTRAN-77 may be upper or lower case
    case_significant = year in ['66', '1966']

    operand_types = []

    if case_significant:
      identifier_tb = FortranIdentifierTokenBuilder()
    else:
      leads = ''
      extras = ''
      identifier_tb = IdentifierTokenBuilder(leads, extras)

    operand_types.append('identifier')

    hollerith_tb = HollerithStringTokenBuilder()
    string_tb = StuffedQuoteStringTokenBuilder(["'", '"'], False)
    operand_types.append('string')

    format_tb = FormatSpecifierTokenBuilder()

    known_operators = [
      '=', '+', '-', '*', '/', '**',
      '.EQ.', '.GT.', '.GE.', '.LT.', '.LE.', '.NE.',
      '.AND.', '.OR.', '.NOT.'
    ]

    if case_significant:
      known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)
    else:
      known_operator_tb = CaseInsensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-'
    ]

    groupers = ['(', ')', ',']
    # group_starts = ['(', ',']
    group_mids = [',']
    # group_ends = [')']

    groupers_tb = CaseSensitiveListTokenBuilder(groupers, 'group', False)

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

    if year in ['77', '1977']:
      keywords += keywords_77

    if case_significant:
      keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)
    else:
      keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    types = [
      'INTEGER', 'REAL', 'COMPLEX', 'DOUBLE PRECISION', 'LOGICAL'
    ]

    types_77 = [
      'CHARACTER'
    ]

    if year in ['77', '1977']:
      types += types_77

    if case_significant:
      types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    else:
      types_tb = CaseInsensitiveListTokenBuilder(types, 'type', True)

    operand_types.append('type')

    tokenbuilders1 = [
      self.newline_tb,
      self.whitespace_tb,
      self.integer_tb,
      self.integer_exponent_tb,
      self.real_tb,
      self.real_exponent_tb,
      self.double_exponent_tb,
      keyword_tb,
      types_tb,
      format_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      self.jcl_tb
    ]

    tokenbuilders2 = [
      self.unknown_operator_tb,
      self.invalid_token_builder
    ]

    if year in ['66', '1966']:
      tokenbuilders = tokenbuilders1 + [hollerith_tb] + tokenbuilders2

    if year in ['77', '1977']:
      tokenbuilders = tokenbuilders1 + [string_tb] + tokenbuilders2

    tokenizer = Tokenizer(tokenbuilders)


    ascii_code = self.convert_to_ascii(code)
    tokens = self.tokenize_code(ascii_code, tab_size, tokenizer)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'whitespace')

    self.convert_numbers_to_lineNumbers()
    self.convert_stars_to_io_channels()

    self.calc_statistics()

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
