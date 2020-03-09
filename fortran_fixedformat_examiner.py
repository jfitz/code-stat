import string

from codestat_exception import CodeStatException
from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  ListTokenBuilder,
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
    ListTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    StuffedQuoteStringTokenBuilder.__escape_z__()
    FortranIdentifierTokenBuilder.__escape_z__()
    FormatSpecifierTokenBuilder.__escape_z__()
    HollerithStringTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, year, tab_size, wide):
    super().__init__()

    if year is not None and year not in ['66', '1966', '77', '1977']:
      raise CodeStatException('Unknown year for language')

    # FORTRAN-66 should be upper case only
    # FORTRAN-77 may be upper or lower case
    case_significant = year in ['66', '1966']

    if case_significant:
      identifier_tb = FortranIdentifierTokenBuilder()
    else:
      leads = ''
      extras = ''
      suffixes = ''
      identifier_tb = IdentifierTokenBuilder(leads, extras, suffixes)

    hollerith_tb = HollerithStringTokenBuilder()
    string_tb = StuffedQuoteStringTokenBuilder(["'", '"'], False)
    format_tb = FormatSpecifierTokenBuilder()

    known_operators = [
      '=', '+', '-', '*', '/', '**',
      '.EQ.', '.GT.', '.GE.', '.LT.', '.LE.', '.NE.',
      '.AND.', '.OR.', '.NOT.'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', case_significant)

    self.unary_operators = [
      '+', '-'
    ]

    groupers = ['(', ')', ',']
    # group_starts = ['(', ',']
    # group_ends = [')']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

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

    keyword_tb = ListTokenBuilder(keywords, 'keyword', case_significant)

    types = [
      'INTEGER', 'REAL', 'COMPLEX', 'DOUBLE PRECISION', 'LOGICAL'
    ]

    types_77 = [
      'CHARACTER'
    ]

    if year in ['77', '1977']:
      types += types_77

    types_tb = ListTokenBuilder(types, 'type', case_significant)

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

    tokens = self.tokenize_code(code, tab_size, tokenizer, wide)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'whitespace')

    self.convert_numbers_to_lineNumbers()
    self.convert_stars_to_io_channels()

    tokens = self.source_tokens()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence(tokens)
    # self.calc_operator_3_confidence(tokens, group_ends)
    # self.calc_operator_4_confidence(tokens, group_starts)
    operand_types = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    if not wide:
      self.calc_line_length_confidence(code, 80)
    self.calc_statistics()


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
        tokens.append(Token(line_number, 'whitespace'))
      else:
        if line_number.isdigit():
          tokens.append(Token(line_number, 'line number'))
        else:
          ln_2 = line_number.lstrip()
          ln_3 = ln_2.rstrip()

        front_space = ''
        front_count = len(line_number) - len(ln_2)
        if front_count > 0:
          front_space = ' ' * front_count
          tokens.append(Token(front_space, 'whitespace'))

        if ln_3.strip().isdigit():
          tokens.append(Token(ln_3, 'line number'))
        else:
          tokens.append(Token(line_number, 'invalid'))

        back_space = ''
        back_count = len(ln_2) - len(ln_3)
        if back_count > 0:
          back_space = ' ' * back_count
          tokens.append(Token(back_space, 'whitespace'))

    return tokens


  def tokenize_line(self, line, tokenizer, wide):
    # break apart the line based on fixed format
    tokens = []

    # The fixed-format FORTRAN line format is:
    # 1: space or C or *
    # 2-6: line number or blank
    # 7: continuation character
    # 8-72: program text
    # 73-: identification, traditionally sequence number (ignored)

    if line.startswith(('//', '/*')):
      tokens.append(Token(line, 'jcl'))
    else:
      line_indicator = line[0:1]
      line_number = line[1:5]
      line_continuation = line[5:6]
      if wide:
        line_text = line[6:]
        line_identification = ''
      else:
        line_text = line[6:72]
        line_identification = line[72:]

      # tokenize the line indicator
      if line_indicator in ['C', '*']:
        tokens.append(Token(line, 'comment'))
      else:
        if len(line_indicator) > 0 and line_indicator != ' ':
          tokens.append(Token(line, 'invalid'))
        else:
          tokens += self.tokenize_line_number(line_number)

          # tokenize line continuation character
          if len(line_continuation) > 0:
            if line_continuation.isspace():
              tokens.append(Token(line_continuation, 'whitespace'))
            else:
              tokens.append(Token(line_continuation, 'line continuation'))

          # tokenize the code
          tokens += tokenizer.tokenize(line_text)

          # tokenize the line identification
          if len(line_identification) > 0:
            tokens.append(Token(line_identification, 'line identification'))

    tokens.append(Token('\n', 'newline'))

    return tokens


  def tokenize_code(self, code, tab_size, tokenizer, wide):
    lines = code.split('\n')

    tokens = []

    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = self.tabs_to_spaces(line, tab_size)

      line_tokens = self.tokenize_line(line, tokenizer, wide)
      tokens += line_tokens

    return tokens
