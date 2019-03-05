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
  LineNumberTokenBuilder,
  FormatSpecifierTokenBuilder
)
from Tokenizer import Tokenizer

class Fortran77Examiner(FortranExaminer):
  def __init__(self, code, tab_size, wide):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    double_exponent_tb = RealExponentTokenBuilder(False, False, 'D')
    identifier_tb = FortranIdentifierTokenBuilder()
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
      'IF', 'THEN', 'ELSE', 'ENDIF', 'END IF', 'GO', 'TO', 'GOTO', 'GO TO',
      'ASSIGN',
      'READ', 'WRITE', 'BACKSPACE', 'REWIND', 'ENDFILE', 'FORMAT', 'PRINT',
      'DO', 'CONTINUE',
      'PROGRAM', 'SUBROUTINE', 'FUNCTION', 'BLOCK DATA',
      'INTEGER', 'REAL', 'COMPLEX', 'DOUBLE PRECISION', 'LOGICAL', 'CHARACTER',
      'IMPLICIT', 'SAVE',
      'COMMON', 'DIMENSION', 'EQUIVALENCE', 'DATA', 'EXTERNAL',
      'CALL', 'RETURN', 'PAUSE', 'STOP', 'END',
      'INQUIRE', 'INTRINSIC', 'PARAMETER'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

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

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operand_confidence()
    self.calc_keyword_confidence()
    if not wide:
      self.calc_line_length_confidence(code, 80)
