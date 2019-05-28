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
  IdentifierTokenBuilder,
  ListTokenBuilder,
  LeadCommentTokenBuilder
)
from FortranTokenBuilders import (
  UserDefinedOperatorTokenBuilder,
  KindIntegerTokenBuilder,
  KindRealTokenBuilder
)
from Tokenizer import Tokenizer

class FortranFreeFormatExaminer(FortranExaminer):
  def __init__(self, code, year):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    kind_integer_tb = KindIntegerTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    double_exponent_tb = RealExponentTokenBuilder(False, False, 'D')
    kind_real_tb = KindRealTokenBuilder()
    identifier_tb = IdentifierTokenBuilder()

    bang_comment_tb = LeadCommentTokenBuilder('!')
    string_tb = StringTokenBuilder(["'", '"'], True, False)

    known_operators = [
      '=', '+', '-', '*', '/', '**',
      '==', '>', '>=', '<', '<=', '/=',
      '.EQ.', '.NE.', '.LT.', '.LE.', '.GT.', '.GE.',
      '.AND.', '.OR.', '.NOT.', '.EQV.', '.NEQV.',
      ':', '::', '=>', '%'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-'
    ]

    user_operator_tb = UserDefinedOperatorTokenBuilder()

    continuation_tb = ListTokenBuilder(['&'], 'line continuation', False)

    stmt_separator_tb = ListTokenBuilder([';'], 'statement separator', False)

    groupers = ['(', ')', ',', '[', ']']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'IF', 'THEN', 'ELSE', 'ENDIF', 'END IF', 'GO', 'TO', 'GOTO', 'GO TO',
      'READ', 'WRITE', 'BACKSPACE', 'REWIND', 'ENDFILE', 'FORMAT', 'PRINT',
      'DO', 'CONTINUE',
      'PROGRAM', 'SUBROUTINE', 'FUNCTION', 'BLOCK DATA',
      'IMPLICIT', 'SAVE',
      'COMMON', 'DIMENSION', 'EQUIVALENCE', 'DATA', 'EXTERNAL',
      'CALL', 'RETURN', 'STOP', 'END',
      'INQUIRE', 'INTRINSIC', 'PARAMETER',
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

    if year in ['95', '2003', '2008']:
      keywords += keywords_95

    if year in ['2003', '2008']:
      keywords += keywords_2003

    if year in ['2008']:
      keywords += keywords_2008

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    types = [
      'INTEGER', 'REAL', 'COMPLEX', 'DOUBLE PRECISION', 'DOUBLEPRECISION',
      'DOUBLE', 'PRECISION', 'LOGICAL', 'CHARACTER'
    ]

    types_tb = ListTokenBuilder(types, 'type', False)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
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
      continuation_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      user_operator_tb,
      string_tb,
      bang_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.ConvertNumbersToLineNumbers()
    self.ConvertStarsToIOChannels()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # self.calc_operator_3_confidence()
    operand_types = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_statistics()
