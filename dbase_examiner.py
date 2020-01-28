import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
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
  SingleCharacterTokenBuilder
)
from cobol_token_builders import AsteriskCommentTokenBuilder
from dbase_token_builders import DbaseFilenameTokenBuilder
from examiner import Examiner

class DbaseExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    AsteriskCommentTokenBuilder.__escape_z__()
    DbaseFilenameTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    real_tb = RealTokenBuilder(False, False, "'")
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False, False)

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

    comment_tb = AsteriskCommentTokenBuilder()

    known_operators = [
      '+', '-', '*', '/', '**', '^',
      '=', '<>', '#', '>', '>=', '<', '<=',
      '$',
      '.NOT.', '.AND.', '.OR.'
    ]

    self.unary_operators = [
      '+', '-',
      '.NOT.'
    ]

    self.postfix_operators = []

    groupers = ['(', ')', ',']
    group_ends = [')']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'APPEND',
      'CASE',
      'DO',
      'EJECT', 'ENDDO', 'ERASE',
      'FORMAT',
      'GET', 'GO',
      'PACK', 'PICTURE',
      'READ', 'REPLACE', 'RETURN',
      'SAY', 'SET', 'SKIP', 'STORE',
      'TALK', 'TO',
      'USE',
      'WHILE', 'WITH',
      '@'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    values = [
      'OFF', 'ON', 'TOP', 'BOTTOM', 'EOF', 'BLANK'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    functions = [
      'ALLTRIM',
      'CHR', 'CTOD',
      'DATE', 'DATETIME', 'DAY', 'DELETED', 'DESCEND', 'DTOC', 'DTOS',
      'IIF',
      'LEFT', 'LTRIM',
      'MONTH',
      'PAGENO',
      'RECCOUNT', 'RECNO', 'RIGHT',
      'STOD', 'STR', 'SUBSTR',
      'TIME', 'TRIM',
      'UPPER',
      'VAL',
      'YEAR'
    ]

    function_tb = ListTokenBuilder(functions, 'function', True)

    filename_tb = DbaseFilenameTokenBuilder()

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      function_tb,
      identifier_tb,
      string_tb,
      filename_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_statistics()
