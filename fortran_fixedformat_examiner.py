import string

from codestat_exception import CodeStatException
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
  ListTokenBuilder
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
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StuffedQuoteStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    FortranIdentifierTokenBuilder.__escape_z__()
    FormatSpecifierTokenBuilder.__escape_z__()
    HollerithStringTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, year, tab_size, wide):
    super().__init__()

    if year is not None and year not in ['66', '1966', '77', '1977']:
      raise CodeStatException('Unknown year for language')

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    double_exponent_tb = RealExponentTokenBuilder(False, False, 'D', None)
    identifier_tb = FortranIdentifierTokenBuilder()
    hollerith_tb = HollerithStringTokenBuilder()
    string_tb = StuffedQuoteStringTokenBuilder(["'", '"'], False)
    format_tb = FormatSpecifierTokenBuilder()

    known_operators = [
      '=', '+', '-', '*', '/', '**',
      '.EQ.', '.GT.', '.GE.', '.LT.', '.LE.', '.NE.',
      '.AND.', '.OR.', '.NOT.'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-'
    ]

    groupers = ['(', ')', ',']
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

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'INTEGER', 'REAL', 'COMPLEX', 'DOUBLE PRECISION', 'LOGICAL'
    ]

    types_77 = [
      'CHARACTER'
    ]

    if year in ['77', '1977']:
      types += types_77

    types_tb = ListTokenBuilder(types, 'type', False)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders1 = [
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
      identifier_tb
    ]

    tokenbuilders2 = [
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    if year in ['66', '1966']:
      tokenbuilders = tokenbuilders1 + [hollerith_tb] + tokenbuilders2

    if year in ['77', '1977']:
      tokenbuilders = tokenbuilders1 + [string_tb] + tokenbuilders2

    tokenizer = Tokenizer(tokenbuilders)

    tokens = self.tokenize_code(code, tab_size, tokenizer, wide)
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'whitespace')

    self.convert_numbers_to_lineNumbers()
    self.convert_stars_to_io_channels()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_confidence(operand_types)
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
