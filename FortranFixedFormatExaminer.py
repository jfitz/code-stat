import string
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

    if year == '77':
      keywords += keywords_77

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'INTEGER', 'REAL', 'COMPLEX', 'DOUBLE PRECISION', 'LOGICAL'
    ]

    types_77 = [
      'CHARACTER'
    ]

    if year == '77':
      types += types_77

    types_tb = ListTokenBuilder(types, 'type', False)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
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
      hollerith_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    if year == '77':
      tokenbuilders = [
        whitespace_tb,
        newline_tb,
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
        string_tb,
        self.unknown_operator_tb,
        invalid_token_builder
      ]

    tokenizer = Tokenizer(tokenbuilders)

    self.tokens = self.TokenizeCode(code, tab_size, tokenizer, wide)
    self.tokens = self.combineAdjacentWhitespace(self.tokens)

    self.ConvertNumbersToLineNumbers()
    self.ConvertStarsToIOChannels()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # self.calc_operator_3_confidence()
    self.calc_operand_confidence()
    # self.calc_value_value_confidence()
    # self.calc_value_value_different_confidence()
    self.calc_keyword_confidence()
    if not wide:
      self.calc_line_length_confidence(code, 80)
    self.calc_statistics()
