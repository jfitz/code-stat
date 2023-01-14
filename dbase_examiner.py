import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  NullTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  IdentifierTokenBuilder,
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
  DbaseStringTokenBuilder,
  DbaseFilenameTokenBuilder,
  WildCardIdentifierTokenBuilder,
  BracketedStringTokenBuilder,
  SetCaseInsensitiveListTokenBuilder,
  KeywordCommentTokenBuilder,
  KeywordDoCaseCommentTokenBuilder,
  TextBlockTokenBuilder
)
from examiner import Examiner

class DbaseExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    NullTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    AsteriskCommentTokenBuilder.__escape_z__()
    DbaseStringTokenBuilder.__escape_z__()
    DbaseFilenameTokenBuilder.__escape_z__()
    WildCardIdentifierTokenBuilder.__escape_z__()
    BracketedStringTokenBuilder.__escape_z__()
    SetCaseInsensitiveListTokenBuilder.__escape_z__()
    KeywordCommentTokenBuilder.__escape_z__()
    KeywordDoCaseCommentTokenBuilder.__escape_z__()
    TextBlockTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, version, format):
    super().__init__()

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

    quotes = ['"', "'"]
    string_tb = DbaseStringTokenBuilder(quotes)
    bracket_string_tb = BracketedStringTokenBuilder()
    text_string_tb = TextBlockTokenBuilder('TEXT', 'ENDTEXT')
    operand_types.append('string')

    line_continuation_tb = SingleCharacterTokenBuilder(';', 'line continuation', False)

    comment_tb = AsteriskCommentTokenBuilder()

    if version == 'ii':
      line_comment_tb = NullTokenBuilder()
    else:
      line_comment_tb = LeadToEndOfLineTokenBuilder('&&', True, 'comment')

    if version == 'ii':
      known_operators = [
        '#',
        '+', '-', '*', '/', '**', '^',
        '=', '<>', '>', '>=', '<', '<=',
        '$',
        '.NOT.', '.AND.', '.OR.',
        '&', 'P.', 'S.'
      ]

    if version == 'iii':
      known_operators = [
        '#',
        '+', '-', '*', '/', '**', '^',
        '=', '<>', '>', '>=', '<', '<=', '!=',
        '$', '!', '->',
        '.NOT.', '.AND.', '.OR.',
        '&', '.'
      ]

    known_operator_tb = CaseInsensitiveListTokenBuilder(known_operators, 'operator', False)

    if version == 'ii':
      self.unary_operators = [
        '+', '-',
        '.NOT.',
        '&', '$', '!',
        'P.', 'S.'
      ]

    if version == 'iii':
      self.unary_operators = [
        '+', '-',
        '.NOT.',
        '&', '$', '!'
      ]

    self.postfix_operators = []

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
        'BROWSE', 'BROW',
        'CANCEL', 'CANC', 'CASE', 'CHANGE', 'CHAN', 'CLEAR', 'CLEA',
        'COMMAND', 'CONTINUE', 'COPY', 'COUNT', 'COUN', 'CREATE', 'CREA',
        'DELETE', 'DELE', 'DISPLAY', 'DISP', 'DO',
        'EDIT', 'EJECT', 'EJEC', 'ELSE', 'ENDCASE', 'ENDC', 'ENDDO', 'ENDD',
        'ENDIF', 'ENDI', 'ENDWHILE', 'ENDW', 'ERASE', 'ERAS',
        'FILE', 'FIND', 'FOR', 'FORMAT', 'FORM',
        'GET', 'GO', 'GOTO',
        'IF', 'INDEX', 'INDE', 'INPUT', 'INPU',
        'INSERT', 'INSE',
        'JOIN',
        'LIKE', 'LIST', 'LOCATE', 'LOCA', 'LOOP',
        'MEMORY', 'MEMO', 'MODIFY', 'MODI',
        'OTHERWISE', 'OTHE',
        'PACK', 'PICTURE', 'PICT',
        'QUIT',
        'READ', 'RECALL', 'RECA', 'RELEASE', 'RELE', 'RENAME', 'RENA',
        'REPLACE', 'REPL', 'REPORT', 'REPO', 'RESTORE', 'REST',
        'RETURN', 'RETU',
        'SAVE', 'SAY', 'SELECT', 'SELE', 'SET', 'SKIP', 'SORT',
        'STORE', 'STOR', 'STRUCTURE', 'SUM',
        'TO', 'TOTAL', 'TOTA', 'TYPE',
        'UPDATE', 'UPDA', 'USE', 'USING', 'USIN',
        'WAIT', 'WHILE', 'WHIL', 'WITH',
        '@', '?', '??'
      ]

    if version == 'iii':
      keywords = [
        'ACCEPT', 'ACCE', 'APPEND', 'APPE', 'ASSIST', 'ASSI', 'AVERAGE', 'AVER',
        'BROWSE', 'BROW',
        'CALL', 'CANCEL', 'CANC', 'CASE', 'CHANGE', 'CHAN', 'CLEAR', 'CLEA',
        'CLOSE', 'CLOS', 'COMMAND,' 'CONTINUE', 'CONT',
        'COPY', 'COUNT', 'COUN', 'CREATE', 'CREA',
        'DELETE', 'DELE', 'DIR', 'DISPLAY', 'DISP', 'DO',
        'EDIT', 'ELSE', 'ELSEIF', 'ENDCASE', 'ENDC', 'ENDDO', 'ENDD',
        'ENDIF', 'ENDI', 'ENDWHILE', 'ENDW', 'ERASE', 'ERAS', 'EXIT',
        'EXPORT', 'EXPO',
        'FILE', 'FIND', 'FOR', 'FROM', 'FUNCTION', 'FUNC',
        'GET', 'GO', 'GOTO',
        'HELP',
        'IF', 'IMPORT', 'IMPO', 'INDEX', 'INDE', 'INPUT', 'INPU',
        'INSERT', 'INSE',
        'JOIN',
        'KEYBOARD', 'KEYB',
        'LABEL', 'LABE', 'LIKE', 'LIST', 'LOAD', 'LOCATE', 'LOCA', 'LOOP',
        'MEMORY', 'MEMO', 'MODIFY', 'MODI',
        'NEXT',
        'OTHERWISE', 'OTHE',
        'PACK', 'PARAMETERS', 'PARA', 'PICTURE', 'PICT', 'PRIVATE', 'PRIV',
        'PROCEDURE', 'PROC', 'PUBLIC', 'PUBL',
        'QUIT',
        'READ', 'RECALL', 'RECA', 'RELEASE', 'RELE', 'RENAME', 'RENA',
        'REPLACE', 'REPL', 'REPORT', 'REPO', 'RESTORE', 'REST',
        'RESUME', 'RESU', 'RETURN', 'RETU',
        'RETRY', 'RETR', 'RUN',
        'SAVE', 'SAY', 'SELECT', 'SELE', 'SEEK', 'SET', 'SKIP', 'SORT',
        'STORE', 'STOR', 'STRUCTURE', 'SUM', 'SUSPEND', 'SUSP',
        'TO', 'TOTAL', 'TOTA', 'TYPE',
        'UPDATE', 'UPDA', 'USE', 'USING', 'USIN',
        'WHILE', 'WHIL', 'WITH',
        'ZAP',
        '@', '?', '??'
      ]

    keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    # keyword comments are keywords that also start a comment
    # any text after the keyword is a comment
    keyword_do_case_comment_tb = KeywordDoCaseCommentTokenBuilder(False)

    # dbase II allowed comments after certain keywords
    # later versions allowed comments after fewer
    if version == 'ii':
      keyword_comments = [
        'ELSE', 'ENDCASE', 'ENDC', 'ENDDO', 'ENDD', 'ENDIF', 'ENDI',
        'ENDWHILE', 'ENDW', 'NOTE', 'REMARK', 'REMA'
      ]
    else:
      keyword_comments = ['NOTE', 'REMARK', 'REMA']

    keyword_comment_ii = KeywordCommentTokenBuilder(keyword_comments, False)

    values = []

    if version == 'ii':
      values = [
        'ALL', 'BLANK', 'BLAN', 'BOTTOM', 'BOTT', 'EOF', 'OFF', 'ON', 'TOP',
        '.T.', '.F.', '.Y.', '.N.','T', 'F', 'Y', 'N'
      ]

    if version == 'iii':
      values = [
        'ALL', 'BLANK', 'BLAN', 'BOTTOM', 'BOTT', 'EOF', 'OFF', 'ON', 'TOP',
        '.T.', '.F.', '.Y.', '.N.','T', 'F', 'Y', 'N',
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
        '!', '#', '$',
        'ALLTRIM',
        'CHR', 'CTOD',
        'DATE', 'DATETIME', 'DAY', 'DELETED', 'DESCEND', 'DESC', 'DTOC', 'DTOS',
        'INT', 'IIF',
        'LEFT', 'LEN', 'LTRIM',
        'MONTH',
        'PAGENO', 'PEEK',
        'RECCOUNT', 'RECNO', 'RIGHT',
        'STOD', 'STR', 'SUBSTR',
        'TEST', 'TIME', 'TRIM', 'TYPE',
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
        'PCOL', 'PEEK', 'PROW',
        'READKEY', 'RECCOUNT', 'RECNO', 'RECSIZE', 'REPLICATE', 'RIGHT',
        'RTRIM', 'ROUND', 'ROW',
        'SPACE', 'STUFF', 'SQRT', 'STR', 'SUBSTR',
        'TEST', 'TIME', 'TRANSFORM', 'TRIM', 'TYPE',
        'UPPER',
        'VAL', 'VERSION',
        'YEAR'
      ]

    function_tb = CaseInsensitiveListTokenBuilder(functions, 'function', True)
    operand_types.append('function')

    filename_tb = DbaseFilenameTokenBuilder()

    invalid_token_builder = InvalidTokenBuilder()

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      set_keyword_tb,
      keyword_comment_ii, # after keyword, because these are longer
      keyword_do_case_comment_tb,
      values_tb,
      groupers_tb,
      comment_tb,         # before operators, to catch single asterisk on line
      known_operator_tb,
      function_tb,
      filename_tb,        # before identifier
      identifier_tb,
      string_tb,
      text_string_tb,
      bracket_string_tb,
      line_comment_tb,
      wild_card_identifier_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    self.tokens = tokens
    tokens_prg = tokens.copy()
    tokens_fmt = tokens.copy()
    tokens_frm = tokens.copy()

    self.convert_specials_to_functions(group_starts, group_mids)
    self.convert_keywords_to_functions()
    self.convert_keywords_to_identifiers()
    self.convert_dollar_to_value()

    self.calc_statistics()
    statistics_prg = self.statistics.copy()
    statistics_prg['format'] = 'program'
    statistics_fmt = self.statistics.copy()
    statistics_fmt['format'] = 'screen'
    statistics_frm = self.statistics.copy()
    statistics_frm['format'] = 'report'

    tokens = self.source_tokens()

    if format in ['better', 'program']:
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

      confidences_prg = self.confidences.copy()
      self.confidences = {}
      errors_prg = self.errors.copy()
      self.errors = []

      if len(confidences_prg) == 0:
        confidence_prg = 0.0
      else:
        confidence_prg = 1.0

        for key in confidences_prg:
          factor = confidences_prg[key]
          confidence_prg *= factor

      if format == 'program':
        self.tokens = tokens_prg
        self.statistics = statistics_prg
        self.confidences = confidences_prg
        self.errors = errors_prg

    if format in ['better', 'screen']:
      self.calc_token_confidence()
      self.calc_fmt_line_confidence()

      confidences_fmt = self.confidences.copy()
      self.confidences = {}
      errors_fmt = self.errors.copy()
      self.errors = []

      if len(confidences_fmt) == 0:
        confidence_fmt = 0.0
      else:
        confidence_fmt = 1.0

        for key in confidences_fmt:
          factor = confidences_fmt[key]
          confidence_fmt *= factor

      if format == 'screen':
        self.tokens = tokens_fmt
        self.statistics = statistics_fmt
        self.confidences = confidences_fmt
        self.errors = errors_fmt

    if format in ['better', 'report']:
      self.calc_token_confidence()
      self.calc_yes_no_line_confidence()

      confidences_frm = self.confidences.copy()
      self.confidences = {}
      errors_frm = self.errors.copy()
      self.errors = []

      if len(confidences_frm) == 0:
        confidence_frm = 0.0
      else:
        confidence_frm = 1.0

        for key in confidences_frm:
          factor = confidences_frm[key]
          confidence_frm *= factor

      if format == 'report':
        self.tokens = tokens_frm
        self.statistics = statistics_frm
        self.confidences = confidences_frm
        self.errors = errors_frm

    if format == 'better':
      # select the better of free-format and spaced-format
      if confidence_prg > confidence_fmt and confidence_prg > confidence_frm:
        self.tokens = tokens_prg
        self.statistics = statistics_prg
        self.confidences = confidences_prg
        self.errors = errors_prg
      elif confidence_fmt > confidence_frm:
        self.tokens = tokens_fmt
        self.statistics = statistics_fmt
        self.confidences = confidences_fmt
        self.errors = errors_fmt
      else:
        self.tokens = tokens_frm
        self.statistics = statistics_frm
        self.confidences = confidences_frm
        self.errors = errors_frm


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


  # convert star and hash to function
  def convert_specials_to_functions(self, group_starts, group_mids):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      x1 = prev_token.group in ['keyword', 'operator']

      x2 = prev_token.group == 'group' and \
        (prev_token.text in group_starts or prev_token.text in group_mids)

      if token.group == 'operator' and token.text in '*#' and \
        (x1 or x2):
        token.group = 'function'
        token.is_operand = True

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token


  # convert keywords before parens to functions
  def convert_keywords_to_functions(self):
    prev_prev_token = Token('\n', 'newline', False)
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'group' and token.text == '(' and \
        prev_token.group == 'keyword' and \
        prev_prev_token.group != 'newline':
        prev_token.group = 'function'
        prev_token.is_operand = True

      if token.group not in ['whitespace', 'comment']:
        prev_prev_token = prev_token
        prev_token = token


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
      if token.group == 'keyword' and \
        prev_token.group == 'operator':
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

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token


  # convert dollar signs after '@' to value
  def convert_dollar_to_value(self):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'operator' and token.text == '$' and \
        prev_token.group == 'keyword' and prev_token.text == '@':
        token.group = 'value'
        token.is_operand = True

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token


  def calc_fmt_line_confidence(self):
    # remove tokens we don't care about
    drop_types = ['whitespace', 'comment']
    tokens = Examiner.drop_tokens(self.tokens, drop_types)

    # split tokens by lines
    lines = self.split_tokens_into_lines(tokens)

    # check that lines begin with '@'
    num_screen_lines = 0

    for line in lines:
      if len(line) > 0 and line[0].text == '@':
        num_screen_lines += 1

    line_screen_confidence = 0.0
    if len(lines) > 0:
      line_screen_confidence = num_screen_lines / len(lines)

    self.confidences['line screen format'] = line_screen_confidence

  def calc_yes_no_line_confidence(self):
    # remove tokens we don't care about
    drop_types = ['whitespace']
    tokens = Examiner.drop_tokens(self.tokens, drop_types)

    # split tokens by lines
    lines = self.split_tokens_into_lines(tokens)

    # check that at least 3 lines contain single y/n value and nothing else
    num_y_n_lines = 0

    for line in lines:
      if len(line) == 1 and line[0].group == 'value':
        num_y_n_lines += 1

    line_y_n_confidence = 0.0
    if num_y_n_lines > 2:
      line_y_n_confidence = 1.0

    self.confidences['line yn format'] = line_y_n_confidence
