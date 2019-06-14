import string
from CodeStatException import CodeStatException
from Token import Token
from Examiner import Examiner
from FortranExaminer import FortranExaminer
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder
)
from FortranTokenBuilders import (
  FortranIdentifierTokenBuilder,
  FormatSpecifierTokenBuilder,
  HollerithStringTokenBuilder
)
from Tokenizer import Tokenizer

class FortranFixedFormatExaminer(FortranExaminer):
  def __init__(self, code, year, tab_size, wide):
    super().__init__()

    if year is not None and year not in ['66', '1966', '77', '1977']:
      raise CodeStatException('Unknown year for language')

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    double_exponent_tb = RealExponentTokenBuilder(False, False, 'D')
    identifier_tb = FortranIdentifierTokenBuilder()
    hollerith_tb = HollerithStringTokenBuilder()
    string_tb = StringTokenBuilder(["'", '"'], True, False)
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
    group_ends = [')']

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

    self.tokens = self.tokenize_code(code, tab_size, tokenizer, wide)
    self.tokens = self.combine_adjacent_whitespace(self.tokens)

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


def unwrap_fortran_lines(lines):
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
