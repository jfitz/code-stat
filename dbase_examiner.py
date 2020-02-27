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
  DbaseSpecialFunctionTokenBuilder,
  DbaseIdentifierTokenBuilder,
  DbaseFilenameTokenBuilder,
  KeywordCommentTokenBuilder,
  TextBlockTokenBuilder
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
    DbaseSpecialFunctionTokenBuilder.__escape_z__()
    DbaseIdentifierTokenBuilder.__escape_z__()
    DbaseFilenameTokenBuilder.__escape_z__()
    KeywordCommentTokenBuilder.__escape_z__()
    TextBlockTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, version):
    super().__init__()
    self.newlines_important = 'always'

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    ctrlz_tb = SingleCharacterTokenBuilder([''], 'EOF')

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    real_tb = RealTokenBuilder(False, False, "'")
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")

    if version == 'ii':
      extra_chars = [':', '_', "'"]

    if version == 'iii':
      extra_chars = ['_']

    identifier_tb = DbaseIdentifierTokenBuilder(extra_chars)

    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, False)

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

    comment_tb = AsteriskCommentTokenBuilder()
    text_comment_tb = TextBlockTokenBuilder('TEXT', 'ENDTEXT')

    if version == 'ii':
      known_operators = [
        '+', '-', '*', '/', '**', '^',
        '=', '<>', '#', '>', '>=', '<', '<=',
        '$',
        '.NOT.', '.AND.', '.OR.',
        '&', '$', '#', '!'
      ]

    if version == 'iii':
      known_operators = [
        '+', '-', '*', '/', '**', '^',
        '=', '<>', '#', '>', '>=', '<', '<=',
        '$',
        '.NOT.', '.AND.', '.OR.',
        '&', '$', '#', '!'
      ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', False)

    if version == 'ii':
      self.unary_operators = [
        '+', '-',
        '.NOT.',
        '&', '$', '#', '!'
      ]

    if version == 'iii':
      self.unary_operators = [
        '+', '-',
        '.NOT.',
        '&'
      ]

    self.postfix_operators = []

    special_chars = []

    if version == 'ii':
      special_chars = ['*', '#']

    special_function_tb = DbaseSpecialFunctionTokenBuilder(special_chars)

    groupers = ['(', ')', ',']
    group_ends = [')']

    groupers_tb = ListTokenBuilder(groupers, 'group', True)

    if version == 'ii':
      keywords = [
        'ACCEPT', 'ACCE', 'APPEND', 'APPE',
        'CASE', 'CLEAR', 'CLEA', 'COPY', 'CREATE', 'CREA',
        'DISPLAY', 'DISP', 'DO',
        'EJECT', 'EJEC', 'ELSE', 'ENDCASE', 'ENDC', 'ENDDO', 'ENDD',
        'ENDIF', 'ENDI', 'ENDWHILE', 'ENDW', 'ERASE', 'ERAS',
        'FOR', 'FORMAT', 'FORM',
        'GET', 'GO',
        'IF',
        'LIKE', 'LOCATE', 'LOCA',
        'OTHERWISE', 'OTHE',
        'PACK', 'PICTURE', 'PICT',
        'READ', 'RELEASE', 'RELE', 'REPLACE', 'REPL', 'RETURN', 'RETU',
        'SAVE', 'SAY', 'SELECT', 'SELE', 'SET', 'SKIP', 'STORE', 'STOR', 'SUM',
        'TALK', 'TO',
        'USE', 'USING', 'USIN',
        'WAIT', 'WHILE', 'WHIL', 'WITH',
        '@', '?', '??'
      ]

    if version == 'iii':
      keywords = [
        'ACCEPT', 'APPEND', 'ASSIST', 'AVERAGE',
        'BROWSE',
        'CALL', 'CANCEL', 'CASE', 'CHANGE', 'CLEAR', 'CLOSE', 'CONTINUE',
        'COPY', 'COUNT', 'CREATE',
        'DELETE', 'DIR', 'DISPLAY', 'DISP', 'DO',
        'EDIT', 'ELSE', 'ENDCASE', 'ENDDO', 'ENDIF', 'ENDWHILE', 'ERASE', 'EXIT',
        'EXPORT',
        'FIND', 'FOR', 'FROM',
        'GET', 'GOTO',
        'HELP',
        'IF', 'IMPORT', 'INDEX', 'INPUT', 'INSERT',
        'JOIN',
        'LABEL', 'LIKE', 'LIST', 'LOAD', 'LOCATE', 'LOOP',
        'MODIFY',
        'OTHERWISE', 'OTHE',
        'PACK', 'PARAMETERS', 'PICTURE', 'PRIVATE', 'PROCEDURE', 'PUBLIC',
        'QUIT',
        'READ', 'RECALL', 'RELEASE', 'REPLACE', 'RESUME', 'RETURN', 'RETRY',
        'RUN',
        'SAVE', 'SAY', 'SELECT', 'SELE', 'SEEK', 'SET', 'SKIP', 'SORT',
        'STORE', 'SUM', 'SUSPEND',
        'TO', 'TOTAL', 'TYPE',
        'UPDATE', 'USE',
        'WHILE', 'WITH',
        'ZAP',
        '@', '?', '??',
        'ALTERNATE',
        'BELL',
        'CARRY', 'CATALOG', 'CENTURY', 'COLOR', 'CONFIRM', 'CONSOLE',
        'DATE', 'AMERICAN', 'ANSI', 'BRITISH', 'ITALIAN', 'FRENCH', 'GERMAN',
        'DATABASES', 'DEBUG', 'DECIMALS', 'DEFAULT', 'DELETED',
        'DELIMITERS', 'DEVICE', 'DOHISTORY',
        'ECHO', 'ESCAPE', 'EXACT',
        'FIELDS', 'FILTER', 'FIXED', 'FORMAT', 'FUNCTION',
        'HEADING', 'HELP', 'HISTORY',
        'INDEX', 'INTENSITY',
        'MARGIN', 'MEMO', 'WIDTH', 'MENUS', 'MESSAGE',
        'ODOMETER', 'ORDER',
        'PATH', 'PRINTER',
        'RELATION',
        'SAFETY', 'STATUS', 'STEP',
        'TALK', 'TITLE', 'TYPEAHEAD',
        'UNIQUE', 'VIEW', 'STRUCTURE', 'MEMORY', 'LABEL', 'QUERY', 'REPORT',
        'GETS', 'LOCK', 'FREEZE', 'NOFOLLOW', 'NOMENU'
      ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    keyword_comments = []
    if version == 'ii':
      keyword_comments = [
        'ELSE', 'ENDCASE', 'ENDC', 'ENDDO', 'ENDD', 'ENDIF', 'ENDI', 'ENDWHILE', 'ENDW',
        'NOTE', 'REMARK', 'REMA'
      ]

    keyword_comment_tb = KeywordCommentTokenBuilder(keyword_comments, False)

    if version == 'ii':
      values = [
        'ALL', 'BLANK', 'BLAN', 'BOTTOM', 'BOTT', 'EOF', 'OFF', 'ON', 'TOP',
        'PRIMARY', 'PRIM', 'SECONDARY', 'SECO',
        '.T.', '.F.'
      ]

    if version == 'iii':
      values = [
        'ALL', 'BLANK', 'BLAN', 'BOTTOM', 'BOTT', 'EOF', 'OFF', 'ON', 'TOP',
        '.T.', '.F.'
      ]

    values_tb = ListTokenBuilder(values, 'value', False)

    if version == 'ii':
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

    if version == 'iii':
      functions = [
        'ABS', 'ASC', 'AT',
        'BOF',
        'CDOW', 'CHR', 'CMONTH', 'COL', 'CTOD',
        'DATE', 'DAY', 'DBF', 'DELETED', 'DISKSPACE', 'DOW', 'DTOC',
        'EOF', 'ERROR', 'EXP',
        'FILE', 'FKMAX', 'FKLABEL', 'FIELD', 'FOUND',
        'GETENV',
        'IIF', 'INKEY', 'INT', 'ISALPHA', 'ISCOLOR', 'ISLOWER', 'ISUPPER',
        'LEFT', 'LEN', 'LOG', 'LOWER', 'LTRIM', 'LUPDATE',
        'MAX', 'MESSAGE', 'MIN', 'MOD', 'MONTH',
        'NDX',
        'OS',
        'PCOL', 'PROW',
        'READKEY', 'RECCOUNT', 'RECNO', 'RECSIZE', 'REPLICATE', 'RIGHT',
        'RTRIM', 'ROUND', 'ROW',
        'TIME', 'TYPE',
        'SPACE', 'STUFF', 'SQRT', 'STR', 'SUBSTR',
        'TRANSFORM', 'TRIM',
        'UPPER',
        'VAL', 'VERSION',
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
      special_function_tb,
      comment_tb,         # before operators, to catch single asterisk on line
      known_operator_tb,
      function_tb,
      identifier_tb,
      string_tb,
      filename_tb,
      text_comment_tb,
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
