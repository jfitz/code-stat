import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  IdentifierTokenBuilder,
  EscapedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from cobol_token_builders import AsteriskCommentTokenBuilder
from dbase_token_builders import (
  DbaseSpecialFunctionTokenBuilder,
  DbaseFilenameTokenBuilder,
  WildCardIdentifierTokenBuilder,
  BracketedStringTokenBuilder,
  SetCaseInsensitiveListTokenBuilder,
  KeywordCommentTokenBuilder,
  KeywordComment2TokenBuilder,
  TextBlockTokenBuilder
)
from examiner import Examiner

class DbaseExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    EscapedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    AsteriskCommentTokenBuilder.__escape_z__()
    DbaseSpecialFunctionTokenBuilder.__escape_z__()
    DbaseFilenameTokenBuilder.__escape_z__()
    WildCardIdentifierTokenBuilder.__escape_z__()
    BracketedStringTokenBuilder.__escape_z__()
    SetCaseInsensitiveListTokenBuilder.__escape_z__()
    KeywordCommentTokenBuilder.__escape_z__()
    KeywordComment2TokenBuilder.__escape_z__()
    TextBlockTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, version):
    super().__init__()
    ctrlz_char = ''
    code = self.TrimCtrlZText(code, ctrlz_char)

    self.newlines_important = 'always'

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    real_tb = RealTokenBuilder(False, False, "'")
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")
    operand_types.append('number')

    leads = '_'
    extras = '_'

    if version == 'ii':
      extras = ":_'-"

    identifier_tb = IdentifierTokenBuilder(leads, extras)
    wild_card_identifier_tb = WildCardIdentifierTokenBuilder('*?', '*?:')
    operand_types.append('identifier')

    quotes = ['"', "'", "’"]
    string_tb = EscapedStringTokenBuilder(quotes, 0)
    bracket_string_tb = BracketedStringTokenBuilder()
    text_string_tb = TextBlockTokenBuilder('TEXT', 'ENDTEXT')
    operand_types.append('string')

    line_continuation_tb = SingleCharacterTokenBuilder(';', 'line continuation', False)

    comment_tb = AsteriskCommentTokenBuilder()
    line_comment_tb = LeadToEndOfLineTokenBuilder('&&', True, 'comment')

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
        '=', '<>', '#', '>', '>=', '<', '<=', '!=',
        '$', '->',
        '.NOT.', '.AND.', '.OR.',
        '&', '$', '#', '!', '.'
      ]

    known_operator_tb = CaseInsensitiveListTokenBuilder(known_operators, 'operator', False)

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
        '&', '$', '#', '!'
      ]

    self.postfix_operators = []

    special_chars = []

    if version == 'ii':
      special_chars = ['*', '#']

    previous = ['if', 'case', 'while', 'store', '(', '.and.', '.or', '.not.']
    special_function_tb = DbaseSpecialFunctionTokenBuilder(special_chars, previous)

    groupers = ['(', ')', ',']
    group_starts = ['(']
    group_mids = [',']
    group_ends = [')']

    if version == 'iii':
      groupers = ['(', ')', ',', '[', ']']
      group_starts = ['(', '[']
      group_ends = [')', ']']

    groupers_tb = CaseSensitiveListTokenBuilder(groupers, 'group', False)

    if version == 'ii':
      keywords = [
        'ACCEPT', 'ACCE', 'APPEND', 'APPE',
        'CASE', 'CLEAR', 'CLEA', 'COPY', 'COUNT', 'CREATE', 'CREA',
        'DELETE', 'DELE', 'DISPLAY', 'DISP', 'DO',
        'EJECT', 'EJEC', 'ELSE', 'ENDCASE', 'ENDC', 'ENDDO', 'ENDD',
        'ENDIF', 'ENDI', 'ENDWHILE', 'ENDW', 'ERASE', 'ERAS',
        'FIND', 'FOR', 'FORMAT', 'FORM',
        'GET', 'GO', 'GOTO',
        'IF', 'INDEX',
        'LIKE', 'LOCATE', 'LOCA', 'LOOP',
        'OTHERWISE', 'OTHE',
        'PACK', 'PICTURE', 'PICT',
        'READ', 'RECALL', 'RECA', 'RELEASE', 'RELE', 'REPLACE', 'REPL',
        'REPORT', 'REPO', 'RESTORE', 'REST', 'RETURN', 'RETU',
        'SAVE', 'SAY', 'SELECT', 'SELE', 'SET', 'SKIP', 'SORT',
        'STORE', 'STOR', 'SUM',
        'TO',
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
        'EDIT', 'ELSE', 'ELSEIF', 'ENDCASE', 'ENDC', 'ENDDO', 'ENDD',
        'ENDIF', 'ENDI', 'ENDWHILE', 'ENDW', 'ERASE', 'EXIT',
        'EXPORT',
        'FIND', 'FOR', 'FROM', 'FUNCTION', 'FUNC',
        'GET', 'GO', 'GOTO',
        'HELP',
        'IF', 'IMPORT', 'INDEX', 'INPUT', 'INSERT',
        'JOIN',
        'KEYBOARD', 'KEYB',
        'LABEL', 'LIKE', 'LIST', 'LOAD', 'LOCATE', 'LOOP',
        'MODIFY',
        'NEXT',
        'OTHERWISE', 'OTHE',
        'PACK', 'PARAMETERS', 'PARA', 'PICTURE', 'PICT', 'PRIVATE', 'PRIV',
        'PROCEDURE', 'PROC', 'PUBLIC',
        'QUIT',
        'READ', 'RECALL', 'RELEASE', 'REPLACE', 'REPORT', 'RESTORE',
        'RESUME', 'RETURN', 'RETU', 'RETRY', 'RUN',
        'SAVE', 'SAY', 'SELECT', 'SELE', 'SEEK', 'SET', 'SKIP', 'SORT',
        'STORE', 'SUM', 'SUSPEND',
        'TO', 'TOTAL', 'TYPE',
        'UPDATE', 'USE',
        'WHILE', 'WITH',
        'ZAP',
        '@', '?', '??'
      ]

    keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    # keyword comments are keywords that also start a comment
    # any text after the keyword is a comment
    keyword_comments = []
    if version == 'ii':
      keyword_comments = [
        'ELSE', 'ENDCASE', 'ENDC', 'ENDDO', 'ENDD', 'ENDIF', 'ENDI', 'ENDWHILE', 'ENDW',
        'NOTE', 'REMARK', 'REMA'
      ]

    keyword_comment_tb = KeywordCommentTokenBuilder(keyword_comments, False)

    keyword_comment2_tb = KeywordComment2TokenBuilder(['DO', 'CASE'], False)

    values = []

    if version == 'ii':
      values = [
        'ALL', 'BLANK', 'BLAN', 'BOTTOM', 'BOTT', 'EOF', 'OFF', 'ON', 'TOP',
        '.T.', '.F.'
      ]

    if version == 'iii':
      values = [
        'ALL', 'BLANK', 'BLAN', 'BOTTOM', 'BOTT', 'EOF', 'OFF', 'ON', 'TOP',
        '.T.', '.F.',
        'AMERICAN', 'ANSI', 'BRITISH', 'ITALIAN', 'FRENCH', 'GERMAN',
      ]

    values_tb = CaseInsensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    set_keywords = []

    if version == 'ii':
      set_keywords = [
        'ALTERNATE',
        'BELL',
        'COLON', 'COLOR', 'CONSOLE', 'CONS',
        'DELIMITERS',
        'INTENSITY',
        'PRIMARY', 'PRIM', 'PRINT',
        'SECONDARY', 'SECO',
        'TALK'
        ]

    if version == 'iii':
      set_keywords = [
        'ALTERNATE',
        'BELL',
        'CARRY', 'CATALOG', 'CENTURY', 'COLOR', 'CONFIRM', 'CONSOLE',
        'DATE', 'DATABASES', 'DEBUG', 'DECIMALS', 'DEFAULT', 'DELETED',
        'DELIMITERS', 'DEVICE', 'DOHISTORY',
        'ECHO', 'ESCAPE', 'EXACT',
        'FIELDS', 'FILTER', 'FIXED', 'FORMAT', 'FUNCTION',
        'HEADING', 'HELP', 'HISTORY',
        'INTENSITY',
        'MARGIN', 'MEMO', 'WIDTH', 'MENUS', 'MESSAGE',
        'ODOMETER', 'ORDER',
        'PATH', 'PRIMARY', 'PRIM', 'PRINTER',
        'RELATION',
        'SAFETY', 'SECONDARY', 'SECO', 'STATUS', 'STEP',
        'TALK', 'TITLE', 'TYPEAHEAD',
        'UNIQUE', 'VIEW',
        'STRUCTURE', 'MEMORY', 'LABEL', 'QUERY', 'REPORT',
        'GETS', 'LOCK', 'FREEZE', 'NOFOLLOW', 'NOMENU'
      ]

    set_keyword_tb = SetCaseInsensitiveListTokenBuilder(set_keywords, 'keyword', False)

    functions = []
  
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

    function_tb = CaseInsensitiveListTokenBuilder(functions, 'function', True)
    operand_types.append('function')

    filename_tb = DbaseFilenameTokenBuilder()

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders1 = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      set_keyword_tb,
      keyword_comment_tb, # after keyword, because these are longer
      keyword_comment2_tb,
      values_tb,
      groupers_tb,
      special_function_tb,
      comment_tb,         # before operators, to catch single asterisk on line
      known_operator_tb,
      function_tb,
      filename_tb,        # before identifier
      identifier_tb,
      string_tb,
      text_string_tb
    ]

    tokenbuilders_ii = [
      bracket_string_tb
    ]

    tokenbuilders_iii = [
      bracket_string_tb,
      line_comment_tb
    ]

    tokenbuilders2 = [
      wild_card_identifier_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenbuilders = tokenbuilders1

    if version == 'ii':
      tokenbuilders += tokenbuilders_ii

    if version == 'iii':
      tokenbuilders += tokenbuilders_iii

    tokenbuilders += tokenbuilders2

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    self.tokens = tokens

    if version == 'iii':
      self.convert_keywords_to_identifiers()

    self.convert_dollar_to_value()

    self.calc_statistics()

    tokens = self.source_tokens()

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'number', 'function', 'value', 'string', 'filename']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)

    self.calc_keyword_confidence()

    if version == 'ii':
      self.calc_line_format_confidence_ii()

    self.calc_line_length_confidence(code, self.max_expected_line)


  def calc_line_format_confidence_ii(self):
    # remove tokens we don't care about
    drop_types = ['whitespace', 'comment', 'EOF']
    tokens = Examiner.drop_tokens(self.tokens, drop_types)

    # join continued lines
    tokens = self.join_continued_lines(tokens)

    # split tokens by lines
    lines = self.split_tokens_into_lines(tokens)

    # check that each line either blank or starts with a keyword
    num_lines = len(lines)
    num_lines_correct = 0

    for line in lines:
      if len(line) > 0:
        if line[0].group == 'keyword':
          num_lines_correct += 1
        else:
          self.errors.append({
            'TYPE': 'LINE FORMAT',
            'FIRST': line[0].group,
            'SECOND': line[0].text
          })
      else:
        num_lines_correct += 1

    line_format_confidence = 1.0
    if num_lines > 0:
      line_format_confidence = num_lines_correct / num_lines

    self.confidences['line format'] = line_format_confidence

    return tokens


  # convert certain keywords to identifiers
  def convert_keywords_to_identifiers(self):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      # keyword '='
      #  => change keyword to identifier
      if token.group == 'operator' and token.text == '=' and \
        prev_token.group == 'keyword':
        prev_token.group = 'identifier'
        prev_token.is_operand = True

      # operator keyword
      #  => change keyword to identifier
      if prev_token.group == 'operator' and token.group == 'keyword':
        token.group = 'identifier'
        token.is_operand = True

      # keyword ['FUNCTION', 'IF', 'ELSEIF'] keyword
      #  => change second keyword to identifier
      lead_keywords = ['function', 'func', 'if', 'elseif', 'elsi']

      if prev_token.group == 'keyword' and \
        prev_token.text.lower() in lead_keywords and \
        token.group == 'keyword':
        token.group = 'identifier'
        token.is_operand = True

      if token.group not in ['whitespace', 'comment', 'newline', 'line description']:
        prev_token = token


  # convert dollar signs after '@' to value
  def convert_dollar_to_value(self):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'operator' and token.text == '$' and \
        prev_token.group == 'keyword' and prev_token.text == '@':
        token.group = 'value'
        token.is_operand = True

      if token.group not in ['whitespace', 'comment', 'newline', 'line description']:
        prev_token = token
