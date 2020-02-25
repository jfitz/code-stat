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
  ListTokenBuilder,
  SingleCharacterTokenBuilder
)
from cobol_token_builders import AsteriskCommentTokenBuilder
from dbase_token_builders import (
  DbaseDeletedFunctionTokenBuilder,
  DbaseIdentifierTokenBuilder,
  DbaseFilenameTokenBuilder,
  KeywordCommentTokenBuilder
)
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
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    AsteriskCommentTokenBuilder.__escape_z__()
    DbaseDeletedFunctionTokenBuilder.__escape_z__()
    DbaseIdentifierTokenBuilder.__escape_z__()
    DbaseFilenameTokenBuilder.__escape_z__()
    KeywordCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    ctrlz_tb = SingleCharacterTokenBuilder([''], 'EOF')

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    real_tb = RealTokenBuilder(False, False, "'")
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")
    identifier_tb = DbaseIdentifierTokenBuilder()
    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, False)

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

    comment_tb = AsteriskCommentTokenBuilder()

    known_operators = [
      '+', '-', '*', '/', '**', '^',
      '=', '<>', '#', '>', '>=', '<', '<=',
      '$',
      '.NOT.', '.AND.', '.OR.',
      '&', '$', '#', '!'
    ]

    self.unary_operators = [
      '+', '-',
      '.NOT.',
      '&', '$', '#', '!'
    ]

    self.postfix_operators = []

    groupers = ['(', ')', ',']
    group_ends = [')']

    groupers_tb = ListTokenBuilder(groupers, 'group', True)

    deleted_indicator_tb = DbaseDeletedFunctionTokenBuilder()

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'ACCEPT', 'APPEND',
      'CASE', 'CLEAR', 'COPY',
      'DO',
      'EJECT', 'ELSE', 'ENDCASE', 'ENDDO', 'ENDIF', 'ENDWHILE', 'ERASE',
      'FOR', 'FORMAT', 'FORM',
      'IF',
      'GET', 'GO',
      'LOCATE',
      'PACK', 'PICTURE', 'PICT',
      'OTHERWISE',
      'READ', 'RELEASE', 'REPLACE', 'RETURN',
      'SAVE', 'SAY', 'SELECT', 'SELE', 'SET', 'SKIP', 'STORE', 'SUM',
      'TALK', 'TO',
      'USE',
      'WAIT', 'WHILE', 'WITH',
      '@', '?'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    keyword_comments = [
      'ELSE', 'ENDCASE', 'ENDDO', 'ENDIF', 'ENDWHILE',
      'NOTE', 'REMARK'
    ]

    keyword_comment_tb = KeywordCommentTokenBuilder(keyword_comments, False)

    values = [
      'ALL', 'BLANK', 'BOTTOM', 'EOF', 'OFF', 'ON', 'TOP',
      'PRIMARY', 'PRIM', 'SECONDARY', 'SECO',
      '.T.', '.F.'
    ]

    values_tb = ListTokenBuilder(values, 'value', False)

    functions = [
      'ALLTRIM',
      'CHR', 'CTOD',
      'DATE', 'DATETIME', 'DAY', 'DELETED', 'DESCEND', 'DESC', 'DTOC', 'DTOS',
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

    function_tb = ListTokenBuilder(functions, 'function', False)

    filename_tb = DbaseFilenameTokenBuilder()

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      terminators_tb,
      ctrlz_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      keyword_comment_tb,
      values_tb,
      groupers_tb,
      deleted_indicator_tb,
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
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    # self.calc_eof_confidence()
    self.calc_statistics()


  # any tokens after an EOF reduce confidence
  def calc_eof_confidence(self):
    num_tokens = len(self.tokens)

    seen_eof = False
    num_tokens_after_eof = 0

    for token in self.tokens:
      if token.group == 'EOF':
        seen_eof = True

      if seen_eof and token.group != 'EOF':
        num_tokens_after_eof += 1
  
    if num_tokens > 0:
      self.confidences['EOF'] = 1.0 - num_tokens_after_eof / num_tokens
