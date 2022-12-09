import string

from codestat_exception import CodeStatException
from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  NullTokenBuilder,
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StuffedQuoteStringTokenBuilder,
  PrefixedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadToEndOfLineTokenBuilder,
  BlockTokenBuilder
)
from cobol_token_builders import (
  CobolIdentifierTokenBuilder,
  PictureTokenBuilder,
  CRPictureTokenBuilder,
  CobolPreprocessorTokenBuilder,
  AsteriskCommentTokenBuilder
)
from cobol_examiner import CobolExaminer
from examiner import Examiner

class CobolFixedFormatExaminer(CobolExaminer):
  @staticmethod
  def __escape_z__():
    NullTokenBuilder.__escape_z__()
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StuffedQuoteStringTokenBuilder.__escape_z__()
    PrefixedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    BlockTokenBuilder.__escape_z__()
    CobolIdentifierTokenBuilder.__escape_z__()
    PictureTokenBuilder.__escape_z__()
    CRPictureTokenBuilder.__escape_z__()
    CobolPreprocessorTokenBuilder.__escape_z__()
    AsteriskCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, year, extension, tab_size):
    super().__init__()

    if year is not None and year not in ['68', '1968', '74', '1974', '85', '1985', '2002', '2014']:
      raise CodeStatException('Unknown year for language')

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, True, None)
    real_exponent_tb = RealExponentTokenBuilder(False, True, 'E', None)
    operand_types.append('number')

    identifier_tb = CobolIdentifierTokenBuilder()
    operand_types.append('identifier')

    quotes = ['"', "'"]
    string_tb = StuffedQuoteStringTokenBuilder(quotes, True)
    n_string_tb = NullTokenBuilder()
    nx_string_tb = NullTokenBuilder()
    operand_types.append('string')

    picture_tb = PictureTokenBuilder()
    cr_picture_tb = CRPictureTokenBuilder()
    operand_types.append('picture')

    inline_comment_tb = NullTokenBuilder()
    star_comment_tb = NullTokenBuilder()

    terminators_tb = SingleCharacterTokenBuilder('.', 'statement terminator', False)

    if year in ['68', '1968', '74', '1974', '85', '1985']:
      known_operators = [
        'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
        '+', '-', '*', '/', '**',
        '=', '<>', '>', '>=', '<', '<=',
        'AND', 'OR', 'NOT',
        ':'
      ]
    else:
      known_operators = [
        'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
        '+', '-', '*', '/', '**',
        '=', '<>', '>', '>=', '<', '<=',
        'AND', 'OR', 'NOT',
        'B-AND', 'B-NOT', 'B-OR', 'B-XOR',
        ':'
      ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-', 'NOT'
    ]

    groupers = ['(', ')', ',']
    group_starts = ['(']
    group_mids = [',']
    # group_ends = [')']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'ACCEPT', 'ACCESS', 'ADD', 'ADDRESS', 'ADVANCING', 'AFTER', 'ALL',
      'ALPHABETIC', 'ALPHABETIC-LOWER', 'ALPHABETIC-UPPER',
      'ALPHANUMERIC', 'ALPHANUMERIC-EDITED', 'ALTER', 'ALTERNATE', 'AND',
      'APPLY', 'ARE', 'AREA', 'AREAS', 'ASCENDING', 'ASSIGN', 'AT', 'AUTHOR',
      'BEFORE', 'BLOCK', 'BY',
      'CALL', 'CANCEL', 'CD', 'CF', 'CH', 'CHARACTER', 'CHARACTERS',
      'CLOCK-UNITS', 'CLOSE', 'COBOL', 'CODE', 'COLUMN', 'COMMA',
      'COMMUNICATION', 'COMP', 'COMPUTATIONAL', 'COMPUTE', 'CONFIGURATION',
      'CONTAINS', 'CONTROL', 'CONTROLS', 'COPY', 'CORR', 'CORRESPONDING',
      'COUNT', 'CURRENCY',
      'DATA', 'DATE', 'DATE-COMPILED', 'DATE-WRITTEN',
      'DE', 'DEBUG-CONTENTS', 'DEBUG-ITEM', 'DEBUG-LINE', 'DEBUG-NAME',
      'DEBUG-SUB-1', 'DEBUG-SUB-2', 'DEBUG-SUB-3',
      'DECIMAL-POINT', 'DECLARATIVES', 'DELIMITED', 'DELIMITER', 'DEPENDING',
      'DESCENDING', 'DESTINATION', 'DETAIL', 'DISABLE', 'DISPLAY',
      'DIVIDE', 'DIVISION', 'DOWN',
      'EGI', 'ELSE', 'EMI', 'ENABLE', 'END', 'ENTER', 'ENVIRONMENT', 'EQUAL',
      'ERROR', 'ESI', 'EVERY', 'EXIT', 'EXTEND',
      'FD', 'FILE', 'FILE-CONTROL', 'FILLER', 'FINAL', 'FIRST', 'FOOTING', 'FOR',
      'FROM',
      'GENERATE', 'GIVING', 'GLOBAL', 'GO', 'GOBACK', 'GREATER', 'GROUP',
      'HEADING', 'HIGH-VALUE', 'HIGH-VALUES',
      'I-O', 'I-O-CONTROL',
      'IDENTIFICATION', 'IF', 'IN', 'INDEX', 'INDEXED', 'INDICATE', 'INITIAL',
      'INITIATE', 'INPUT', 'INPUT-OUTPUT', 'INSTALLATION', 'INTO', 'INVALID',
      'IS',
      'JUST', 'JUSTIFIED',
      'KEY',
      'LABEL', 'LAST', 'LEADING', 'LEFT', 'LENGTH', 'LESS', 'LIMIT', 'LIMITS',
      'LINE', 'LINE-COUNTER', 'LINES', 'LINKAGE',
      'LOCK', 'LOW-VALUE', 'LOW-VALUES',
      'MEMORY', 'MERGE', 'MESSAGE', 'MODE', 'MODULES', 'MOVE', 'MULTIPLE',
      'MULTIPLY',
      'NEGATIVE', 'NEXT', 'NO', 'NOT', 'NUMBER', 'NUMERIC', 'NUMERIC-EDITED',
      'OBJECT-COMPUTER', 'OCCURS', 'OF', 'OMITTED',
      'OPEN', 'OPTIONAL', 'OR', 'OUTPUT', 'OVERFLOW',
      'PAGE', 'PAGE-COUNTER', 'PERFORM', 'PF', 'PH', 'PIC', 'PICTURE',
      'PLUS', 'POINTER', 'POSITION', 'POSITIVE', 'PROCEDURE', 'PROCEED',
      'PROGRAM', 'PROGRAM-ID',
      'QUEUE', 'QUOTE', 'QUOTES',
      'RANDOM', 'RD', 'READ', 'RECEIVE', 'RECORD', 'RECORDS', 'REDEFINES',
      'REEL', 'REFERENCE', 'RELATIVE', 'RELEASE', 'REMAINDER',
      'RENAMES', 'REPLACE', 'REPLACING', 'REPORT', 'REPORTING', 'REPORTS',
      'RERUN', 'RESERVE', 'RESET', 'RETURN', 'REVERSED', 'REWIND', 'REWRITE',
      'RF', 'RH', 'RIGHT', 'ROUNDED', 'RUN',
      'SAME', 'SD', 'SEARCH', 'SECTION', 'SECURITY', 'SEGMENT', 'SEGMENT-LIMIT',
      'SELECT', 'SEND', 'SENTENCE', 'SEQUENCE', 'SEQUENTIAL', 'SET', 'SIGN', 'SIZE',
      'SORT', 'SOURCE', 'SOURCE-COMPUTER', 'SPECIAL-NAMES', 'STANDARD', 'STATUS',
      'STOP', 'STRING','SUB-QUEUE-1', 'SUB-QUEUE-2', 'SUB-QUEUE-3', 'SUBTRACT',
      'SUM', 'SUPPRESS', 'SYMBOLIC', 'SYNC', 'SYNCHRONIZED',
      'TABLE', 'TALLY', 'TAPE', 'TERMINAL', 'TERMINATE', 'TEST', 'TEXT', 'THAN',
      'THEN', 'THROUGH', 'THRU', 'TIME', 'TIMES', 'TITLE', 'TO', 'TYPE',
      'UNIT', 'UNSTRING', 'UNTIL', 'UP', 'UPON', 'USAGE', 'USE', 'USING',
      'VALUE', 'VALUES', 'VARYING',
      'WHEN',
      'WITH', 'WORDS', 'WORKING-STORAGE', 'WRITE'
    ]

    keywords_68_only = [
      'ACTUAL',
      'FILE-LIMITS',
      'NOMINAL',
      'PROCESSING',
      'NOTE',
      'REMARKS',
      'SEEK',
      'TODAY'
    ]

    keywords_74 = [
      'ALSO',
      'BOTTOM',
      'CODE-SET', 'COLLATING', 'COMMON',
      'DAY', 'DELETE', 'DEBUGGING', 'DUPLICATES', 'DYNAMIC',
      'END-OF-PAGE', 'EOP', 'EXCEPTION',
      'INSPECT',
      'LINAGE', 'LINAGE-COUNTER',
      'NATIVE',
      'ORGANIZATION',
      'PACKED-DECIMAL', 'PADDING', 'PRINTING', 'PROCEDURES',
      'REFERENCES', 'REMOVAL',
      'SEPARATE', 'SORT-MERGE', 'STANDARD-1', 'STANDARD-2', 'START',
      'TALLYING', 'TOP', 'TRAILING'
    ]

    keywords_85 = [
      'ALPHABET', 'ANY',
      'BINARY',
      'CONTENT', 'CONTINUE', 'CONVERTING',
      'DAY-OF-WEEK',
      'END-ADD', 'END-CALL', 'END-COMPUTE', 'END-DELETE', 'END-DIVIDE',
      'END-EVALUATE', 'END-IF', 'END-MULTIPLY', 'END-PERFORM', 'END-READ',
      'END-RECEIVE', 'END-RETURN', 'END-REWRITE', 'END-SEARCH', 'END-START',
      'END-STRING', 'END-SUBTRACT', 'END-UNSTRING', 'END-WRITE',
      'EVALUATE', 'EXTERNAL',
      'INITIALIZE',
      'ORDER', 'OTHER',
      'PURGE'
    ]

    keywords_2002 = [
      'ACTIVE-CLASS', 'ALIGNED', 'ALLOCATE', 'ANYCASE', 'ARITHMETIC', 'AUTO',
      'AUTOMATIC',
      'BACKGROUND-COLOR', 'BASED', 'BASIS', 'BINARY-CHAR', 'BINARY-DOUBLE',
      'BINARY-LONG', 'BINARY-SHORT', 'BIT', 'BLINK', 'BOOLEAN',
      'CENTER', 'CLASS', 'CLASS-ID', 'CLASSIFICATION', 'COLUMNS', 'COM-REG',
      'CONDITION', 'CONSTANT', 'CRT', 'CURSOR', 'CYCLE',
      'DATA-POINTER', 'DBCS', 'DEBUGGING', 'DETAIL', 'DISPLAY', 'DISPLAY-1',
      'DISPLAY-OF',
      'EC', 'EGCS', 'EJECT', 'END-INVOKE', 'ENDING', 'ENTRY-CONVENTION', 'ENTRY-FIELD',
      'EO', 'EOS', 'ERASE', 'EXCEPTION-OBJECT', 'EXCLUSIVE', 'EXPANDS', 'EXTERN',
      'FACTORY', 'FLOAT-EXTENDED', 'FLOAT-LONG', 'FLOAT-SHORT',
      'FOREGROUND-COLOR', 'FOREVER', 'FORMAT', 'FREE', 'FUNCTION', 'FUNCTION-ID',
      'GET', 'GROUP-USAGE',
      'HIGHLIGHT',
      'IGNORING', 'IMPLEMENTS', 'INHERITS', 'INITIALIZED', 'INSERT', 'INTERFACE',
      'INTERFACE-ID', 'INTRINSIC', 'INVOKE',
      'KANJI',
      'LC_ALL', 'LC_COLLATE', 'LC_CTYPE', 'LC_MESSAGES', 'LC_MONEY', 'LC_NUMERIC',
      'LC_TIME', 'LOCAL-STORAGE', 'LOCALE', 'LOWLIGHT',
      'MANUAL', 'METACLASS', 'METHOD', 'METHOD-ID', 'MINUS', 'MORE-LABELS', 
      'NATIONAL', 'NATIONAL-EDITED', 'NATIONAL-OF', 'NATIVE_BINARY', 'NESTED',
      'NEW', 'NONE', 'NORMAL', 
      'OBJECT', 'OBJECT-REFERENCE', 'ONLY', 'OPTIONS', 'OVERRIDE',
      'PHYSICAL', 'PRESENT', 'PREVIOUS', 'PROCEDURE-POINTER',
      'PROCESSING', 'PROGRAM-POINTER', 'PROPERTY', 'PROTOTYPE',
      'RAISE', 'RAISING', 'READY', 'RECURSIVE', 'REFERENCES', 'RELATION', 'RELOAD',
      'REPOSITORY', 'REQUIRED', 'RETURN-CODE', 'RETURNING', 'ROUNDING',
      'SCREEN', 'SECONDS', 'SERVICE', 'SHARING', 'SHIFT-IN', 'SHIFT-OUT',
      'SIGNED','SKIP1', 'SKIP2', 'SKIP3','SORT-CONTROL', 'SORT-CORE-SIZE',
      'SORT-FILE-SIZE','SORT-MESSAGE', 'SORT-MODE-SIZE', 'SORT-RETURN',
      'SOURCES', 'STATEMENT', 'STEP', 'STRONG', 'SYMBOL', 'SYSTEM-DEFAULT',
      'TRACE', 'TYPEDEF',
      'UCS-4', 'UNDERLINE', 'UNIVERSAL', 'UNLOCK', 'UNSIGNED', 'USER-DEFAULT',
      'UTF-16', 'UTF-8',
      'VAL-STATUS', 'VALID', 'VALIDATE', 'VALIDATE-STATUS',
      'WHEN-COMPILED', 'WRITE-ONLY',
      'YYYYDDD', 'YYYYMMDD',
     ]

    keywords_2014 = [
      'AWAY-FROM-ZERO', 'NEAREST-AWAY-FROM-ZERO', 'NEAREST-EVEN',
      'NEAREST-TOWARD-ZERO', 'TOWARD-GREATER', 'TOWARD-LESSER',
      'CAPACITY',
      'FLOAT-BINARY-128', 'FLOAT-BINARY-32', 'FLOAT-BINARY-64',
      'FLOAT-DECIMAL-16', 'FLOAT-DECIMAL-34',
      'FLOAT-INFINITY', 'FLOAT-NOT-A-NUMBER',
      'FUNCTION-POINTER',
      'INTERMEDIATE',
      'PHYSICAL', 'PREFIXED', 'PROHIBITED',
      'SHORT', 'STANDARD-BINARY', 'STANDARD-DECIMAL',
      'TRUNCATION'
    ]

    keywords_ibm = [
      'ABSENT', 'ID', 'PASSWORD', 'UNBOUNDED'
    ]

    keywords_gnu = [
      'ARGUMENT-NUMBER', 'ARGUMENT-VALUE',
      'ASCII',
      'BINARY-C-LONG', 'BINARY-SEQUENTIAL',
      'CARD-PUNCH', 'CARD-READER', 'CASSETTE', 'CHAIN', 'CHAINING', 'COLOR',
      'COMMAND-LINE', 'COMMIT',
      'COMP-1', 'COMP-2', 'COMP-3', 'COMP-4', 'COMP-5', 'COMP-6', 'COMP-X',
      'COMPUTATIONAL-1', 'COMPUTATIONAL-2', 'COMPUTATIONAL-3',
      'COMPUTATIONAL-4', 'COMPUTATIONAL-5', 'COMPUTATIONAL-6',
      'COMPUTATIONAL-X',
      'CONVERSION', 'CRT-UNDER',
      'DISC', 'DISK',
      'EBCDIC', 'ECHO', 'END-CHAIN', 'ENTRY', 'ENVIRONMENT-NAME', 'ENVIRONMENT-VALUE',
      'ESCAPE',
      'F', 'FILE-ID', 'FIXED', 'FLOAT-DECIMAL-7',
      'ID', 'IGNORE',
      'KEPT', 'KEYBOARD',
      'LEFT-JUSTIFY', 'LEFTLINE', 'LINE-SEQUENTIAL', 'LOWER',
      'MAGNETIC-TAPE',
      'NAME', 'NO-ECHO', 'NOTHING',
      'OVERLINE',
      'PRINT', 'PRINTER', 'PRINTER-1', 'PROCEDURE-POINTER', 'PROCEDURES',
      'PROMPT', 'PROTECTED',
      'RECORDING', 'REVERSE', 'RIGHT-JUSTIFY', 'ROLLBACK',
      'S', 'SCROLL', 'SIGNED-INT', 'SIGNED-LONG', 'SIGNED-SHORT', 'SPACE-FILL',
      'STATIC', 'STDCALL', 'SYSTEM-OFFSET',
      'TAB', 'TIME-OUT', 'TRAILING-SIGN',
      'U', 'UNSIGNED-INT', 'UNSIGNED-LONG', 'UNSIGNED-SHORT', 'UPDATE', 'UPPER',
      'USER',
      'V', 'VARIABLE',
      'WAIT', 'WRAP',
      'ZERO-FILL'
    ]

    keywords_acu = [
      '3-D',
      'ACTION', 'ACTIVE-X', 'ADJUSTABLE-COLUMNS', 'ALIGNMENT', 'AUTO-DECIMAL',
      'AUTO-SPIN',
      'BACKGROUND-HIGH', 'BACKGROUND-LOW', 'BACKGROUND-STANDARD', 'BAR',
      'BITMAP', 'BITMAP-END', 'BITMAP-HANDLE', 'BITMAP-NUMBER', 'BITMAP-START',
      'BITMAP-TRAILING', 'BITMAP-TRANSPARENT-COLOR', 'BITMAP-WIDTH',
      'BOX', 'BOXED', 'BUSY', 'BUTTONS',
      'CALENDAR-FONT', 'CANCEL-BUTTON',
      'CELL', 'CELL-COLOR', 'CELL-DATA', 'CELL-FONT', 'CELL-PROTECTION',
      'CENTERED-HEADING', 'CENTURY-DATE', 'CHECK-BOX', 'CLEAR-SELECTION',
      'CLINE', 'CLINES', 'COLORS',
      'COLUMN-COLOR', 'COLUMN-DIVIDERS', 'COLUMN-FONT', 'COLUMN-HEADINGS',
      'COLUMN-PROTECTION', 'COMBO-BOX', 'COPY-SELECTION', 'CSIZE',
      'CURSOR-COL', 'CURSOR-COLOR', 'CURSOR-FRAME-WIDTH', 'CURSOR-ROW',
      'CURSOR-X', 'CURSOR-Y', 'CUSTOM-PRINT-TEMPLATE',
      'DASHED', 'DATA-COLUMNS', 'DATA-TYPES', 'DATE-ENTRY', 'DEFAULT-BUTTON',
      'DEFAULT-FONT', 'DESTROY', 'DISPLAY-COLUMNS', 'DISPLAY-FORMAT',
      'DOTDASH', 'DOTTED', 'DOUBLE', 'DRAG-COLOR', 'DROP-DOWN', 'DROP-LIST',
      'END-COLOR', 'END-MODIFY', 'ENGRAVED', 'ENSURE-VISIBLE',
      'ENTRY-FIELD', 'ENTRY-REASON', 'ESCAPE-BUTTON',
      'EVENT', 'EVENT-LIST', 'EXCEPTION-VALUE', 'EXPAND', 'EXTERNAL-FORM',
      'FILE-NAME', 'FILE-POS', 'FILL-COLOR', 'FILL-COLOR-2', 'FILL-PERCENT',
      'FINISH-REASON', 'FIXED-FONT', 'FIXED-WIDTH', 'FLAT', 'FLAT-BUTTONS',
      'FLOAT', 'FLOATING', 'FONT', 'FRAME', 'FRAMED', 'FULL-HEIGHT',
      'GRID', 'GO-BACK', 'GO-FORWARD', 'GO-HOME', 'GO-SEARCH',
      'GRAPHICAL', 'GRID', 'GROUP-VALUE',
      'HANDLE', 'HAS-CHILDREN', 'HEADING-COLOR', 'HEADING-DIVIDER-COLOR',
      'HEADING-FONT', 'HEAVY', 'HEIGHT-IN-CELLS', 'HIDDEN-DATA',
      'HIGH-COLOR', 'HOT-TRACK', 'HSCROLL', 'HSCROLL-POS',
      'ICON', 'IDENTIFIED', 'INDEPENDENT', 'INQUIRE',
      'INSERTION-INDEX', 'INSERTION-ROWS',  'ITEM', 'ITEM-TEXT', 'ITEM-TO-ADD',
      'ITEM-TO-DELETE', 'ITEM-TO-EMPTY', 'ITEM-VALUE',
      'LABEL', 'LABEL-OFFSET', 'LARGE-FONT', 'LARGE-OFFSET',
      'LAST-ROW', 'LAYOUT-DATA', 'LAYOUT-MANAGER', 'LEADING-SHIFT',
      'LEFT-TEXT', 'LINES-AT-ROOT', 'LIST-BOX', 'LM-RESIZE',
      'LONG-DATE', 'LOW-COLOR', 'LOWERED',
      'MASS-UPDATE', 'MAX-LINES', 'MAX-PROGRESS', 'MAX-TEXT', 'MAX-VAL',
      'MEDIUM-FONT', 'MENU', 'MIN-VAL', 'MODIFY', 'MULTILINE',
      'NAVIGATE-URL', 'NEXT-ITEM', 'NO-AUTOSEL', 'NO-AUTO-DEFAULT', 'NO-BOX',
      'NO-DIVIDERS', 'NO-F4', 'NO-FOCUS', 'NO-GROUP-TAB',
      'NO-KEY-LETTER', 'NO-SEARCH', 'NO-UPDOWN', 'NOTAB',
      'NOTIFY', 'NOTIFY-CHANGE', 'NOTIFY-DBLCLICK', 'NOTIFY-SELCHANGE',
      'NUM-COL-HEADINGS', 'NUM-ROWS',
      'OK-BUTTON', 'OVERLAP-LEFT', 'OVERLAP-TOP',
      'PAGE-SETUP', 'PAGED', 'PARENT', 'PERMANENT', 'PIXEL',
      'PLACEMENT', 'POP-UP', 'POSITION-SHIFT', 'PRINT-NO-PROMPT', 'PRINT-PREVIEW',
      'PRIORITY', 'PROGRESS', 'PROPERTIES', 'PROPERTY', 'PUSH-BUTTON',
      'QUERY-INDEX',
      'RADIO-BUTTON', 'RAISED', 'READ-ONLY',
      'RECORD-DATA', 'RECORD-TO-ADD', 'RECORD-TO-DELETE', 'REFRESH',
      'REGION-COLOR', 'RESET-GRID', 'RESET-LIST', 'RESET-TABS',
      'RIGHT-ALIGN', 'RIMMED', 'ROW-COLOR', 'ROW-COLOR-PATTERN',
      'ROW-DIVIDERS', 'ROW-FONT', 'ROW-HEADINGS', 'ROW-PROTECTION',
      'SAVE-AS', 'SAVE-AS-NO-PROMPT', 'SCROLL-BAR', 'SEARCH-OPTIONS', 'SEARCH-TEXT',
      'SELECT-ALL', 'SELECTION-INDEX', 'SELECTION-TEXT', 'SELF-ACT', 'SEPARATION',
      'SHADING', 'SHADOW', 'SHORT-DATE', 'SHOW-LINES', 'SHOW-NONE', 'SHOW-SEL-ALWAYS',
      'SMALL-FONT', 'SORT-ORDER', 'SPINNER', 'SQUARE', 'START-X', 'START-Y',
      'STATIC-LIST', 'STATUS-BAR', 'STATUS-TEXT', 'STYLE', 'SUBWINDOW',
      'TAB-TO-ADD', 'TAB-TO-DELETE', 'TEMPORARY', 'TERMINATION-VALUE', 'THREAD',
      'THREADS', 'THUMB-POSITION', 'TILED-HEADINGS', 'TITLE', 'TITLE-POSITION',
      'TRADITIONAL-FONT', 'TRAILING-SHIFT', 'TRANSPARENT', 'TREE-VIEW',
      'UNFRAMED', 'UNSORTED', 'USE-ALT', 'USE-RETURN', 'USE TAB',
      'VALUE-FORMAT', 'VARIANT', 'VERTICAL', 'VERY-HEAVY',
      'VIRTUAL-WIDTH', 'VPADDING', 'VSCROLL', 'VSCROLL-BAR', 'VSCROLL-POS', 'VTOP',
      'WEB-BROWSER', 'WIDTH', 'WIDTH-IN-CELLS', 'WINDOW', 'X', 'Y'
    ]

    if year in ['68', '1968']:
      keywords += keywords_68_only

    if year in ['74', '1974', '85', '1985', '2002', '2014']:
      keywords += keywords_74

    if year in ['85', '1985', '2002', '2014']:
      keywords += keywords_85

    if year in ['2002', '2014']:
      keywords += keywords_2002

    if year == '2014':
      keywords += keywords_2014

    if extension.lower() == 'acu':
      keywords += keywords_acu

    if extension.lower() == 'ibm':
      keywords += keywords_ibm

    if extension.lower() == 'gnu':
      keywords += keywords_gnu

    keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    if year in ['68', '1968', '74', '1974']:
      values = [
        'BLANK', 'SPACE', 'SPACES', 'ZERO', 'ZEROES', 'ZEROS',
        'NO', 'OFF', 'ON'
      ]
    elif year in ['85', '1985', '2002', '2014']:
      values = [
        'BLANK', 'SPACE', 'SPACES', 'ZERO', 'ZEROES', 'ZEROS',
        'NO', 'OFF', 'ON',
        'FALSE', 'TRUE'
      ]
    else:
      values = [
        'BLANK', 'SPACE', 'SPACES', 'ZERO', 'ZEROES', 'ZEROS',
        'NO', 'OFF', 'ON',
        'FALSE', 'TRUE',
        'NULL', 'NULLS', 'SELF', 'SUPER'
      ]

    value_tb = CaseInsensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    cobol_preprocessor_tb = NullTokenBuilder()

    exec_tb = BlockTokenBuilder('EXEC', 'END-EXEC', 'exec block')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders_fixed = [
      newline_tb,
      whitespace_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      picture_tb,
      cr_picture_tb,
      keyword_tb,
      star_comment_tb,  # before operator, to catch single star as comment
      known_operator_tb,
      groupers_tb,
      value_tb,
      identifier_tb,
      string_tb,
      n_string_tb,
      nx_string_tb,
      inline_comment_tb,
      cobol_preprocessor_tb,
      exec_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer_fixed = Tokenizer(tokenbuilders_fixed)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = self.tokenize_code(ascii_code, tab_size, tokenizer_fixed)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'whitespace')

    self.convert_numbers_to_pictures()
    self.convert_numbers_to_levels()

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      # self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    # self.calc_operand_n_confidence(tokens, operand_types, 2)
    # self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()
    self.calc_picture_confidence()
    self.calc_line_length_confidence(code, self.max_expected_line)

    expected_keyword_confidence = self.check_expected_keywords()
    self.confidences['expected_keywords'] = expected_keyword_confidence


  def tokenize_line_number(self, line_number):
    token = None

    if len(line_number) > 0:
      if line_number.isspace():
        token = Token(line_number, 'whitespace', False)
      else:
        if line_number.isdigit():
          token = Token(line_number, 'line number', False)
        else:
          token = Token(line_number, 'line identification', False)

    return token


  def tokenize_alt_line(self, line, line_indicator):
    token = None

    if line_indicator in ['*', '/', 'D', 'd']:
      # the entire line is a comment (including DEBUG lines)
      token = Token(line[6:], 'comment', False)
    if line_indicator == '$':
      token = Token(line[6:], 'preprocessor', False)

    return token


  def tokenize_line_indicator(self, line_indicator):
    token = None

    if line_indicator == ' ':
      token = Token(' ', 'whitespace', False)
    else:
      if line_indicator == '-':
        token = Token(line_indicator, 'continuation line', False)
      else:
        if line_indicator != '':
          token = Token(line_indicator, 'invalid', False)

    return token


  def tokenize_line(self, line, tokenizer):
    # break apart the line based on fixed format
    tokens = []

    # The COBOL line format is:
    # 1-6: line number or blank (ignored)
    # 7: space or one of *, /, D, d, $, -
    # 8-71: program text
    # 72-: identification, traditionally sequence number (ignored)

    line_indicator = line[6:7]
    line_text = line[7:72]
    line_identification = line[72:]

    if line.startswith(('//', '/*')):
      tokens.append(Token(line, 'jcl', False))
    else:
      line_number = line[:6]
      token = self.tokenize_line_number(line_number)
      if token is not None:
        tokens.append(token)

      # tokenize the line indicator
      if line_indicator in ['*', '/', 'D', 'd', '$']:
        token = self.tokenize_alt_line(line, line_indicator)
        if token is not None:
          tokens.append(token)
      else:
        token = self.tokenize_line_indicator(line_indicator)
        if token is not None:
          tokens.append(token)

        # tokenize the code
        tokens += tokenizer.tokenize(line_text)

    # tokenize the line identification
    if len(line_identification) > 0:
      tokens.append(Token(line_identification, 'line identification', False))

    tokens.append(Token('\n', 'newline', False))

    return tokens


  def tokenize_code(self, code, tab_size, tokenizer):
    lines = code.split('\n')

    tokens = []

    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = Examiner.tabs_to_spaces(line, tab_size)

      line_tokens = self.tokenize_line(line, tokenizer)
      tokens += line_tokens

    return tokens


  def unwrapped_code(self, lines):
    unwrapped_lines = ''

    buffer = None
    for line in lines:
      # remove line description (if any)
      line = line[:72]

      # force line length to 72 (used later when continuing strings)
      while len(line) < 72:
        line += ' '

      # if continuation (not comment, longer than 6, not space in column)
      if len(line) > 6 and line[6] == '-':
        # drop leading columns
        line = line[7:]

        # append to buffer
        if buffer is None:
          buffer = line
        else:
          # drop leading spaces and the leading quote
          line = line.lstrip()

          buffer2 = buffer.rstrip()
          if len(buffer2) > 0:
            # if the buffer ends with other than quote,
            if buffer2[-1] not in "'\"":
              # combine strings by dropping this line's opening quote
              if len(line) > 0 and line[0] in "'\"":
                line = line[1:]
                buffer += line
            else:
              # previous line ends in quote
              buffer = buffer2 + ' '  # space to separate string tokens
              buffer += line
          else:
            buffer = line
      else:
        if buffer is not None:
          # now drop the extra spaces on the right
          unwrapped_lines += buffer.rstrip()
          unwrapped_lines += '\n'
        buffer = line

    if buffer is not None and len(buffer) > 0:
      unwrapped_lines += buffer
      unwrapped_lines += '\n'

    return unwrapped_lines
