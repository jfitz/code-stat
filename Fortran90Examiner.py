import string
from Token import Token
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder,
  LeadCommentTokenBuilder
)
from Fortran90TokenBuilders import (
  IdentifierTokenBuilder
)
from Tokenizer import Tokenizer

class Fortran90Examiner(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False)
    identifier_tb = IdentifierTokenBuilder()

    bang_comment_tb = LeadCommentTokenBuilder('!')
    string_tb = StringTokenBuilder(["'", '"'], True)

    known_operators = [
      '=', '+', '-', '*', '/', '**',
      '==', '>', '>=', '<', '<=', '/=',
      '.EQ.', '.NE.', '.LT.', '.LE.', '.GT.', '.GE.',
      '.AND.', '.OR.', '.NOT.', '.EQV.', '.NEQV.',
      ':', '::'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', False)

    continuation_tb = ListTokenBuilder(['&'], 'line continuation', False)

    stmt_separator_tb = ListTokenBuilder([';'], 'statement separator', False)

    groupers = ['(', ')', ',', '[', ']']

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
      'INQUIRE', 'INTRINSIC', 'PARAMETER',
      'allocatable', 'allocate', 'case', 'contains', 'cycle', 'deallocate',
      'elsewhere', 'exit', 'include', 'interface', 'intent', 'module',
      'namelist', 'nullify', 'only', 'operator', 'optional', 'pointer',
      'private', 'procedure', 'public', 'recursive', 'result', 'select',
      'sequence', 'target', 'use', 'while', 'where',
      'enddo', 'none'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      continuation_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      string_tb,
      bang_comment_tb,
      self.unknown_operator_tb
    ]

    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()

    # two operands in a row is not FORTRAN
    tokens = self.drop_whitespace(self.tokens)
    tokens = self.drop_comments(tokens)

    operands = ['number', 'string', 'identifier']

    two_operand_count = 0
    prev_token = Token('\n', 'newline')
    for token in tokens:
      if token.group in operands and prev_token.group in operands:
        two_operand_count += 1

      prev_token = token

    operand_confidence = 1.0
    if len(tokens) > 0:
      operand_confidence = 1.0 - (two_operand_count / len(tokens))

    self.confidences['operand'] = operand_confidence
