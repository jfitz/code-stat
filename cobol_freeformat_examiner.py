import string

from codestat_exception import CodeStatException
from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StuffedQuoteStringTokenBuilder,
  PrefixedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadCommentTokenBuilder,
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

class CobolFreeFormatExaminer(CobolExaminer):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StuffedQuoteStringTokenBuilder.__escape_z__()
    PrefixedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadCommentTokenBuilder.__escape_z__()
    BlockTokenBuilder.__escape_z__()
    CobolIdentifierTokenBuilder.__escape_z__()
    PictureTokenBuilder.__escape_z__()
    CRPictureTokenBuilder.__escape_z__()
    CobolPreprocessorTokenBuilder.__escape_z__()
    AsteriskCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, year, extension):
    super().__init__()

    if year is not None and year not in ['2002', '2014']:
      raise CodeStatException('Unknown year for language')

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, True, None)
    real_exponent_tb = RealExponentTokenBuilder(False, True, 'E', None)
    identifier_tb = CobolIdentifierTokenBuilder()
    string_tb = StuffedQuoteStringTokenBuilder(['"', "'"], False)
    n_string_tb = PrefixedStringTokenBuilder('N', False, ['"', "'"])
    nx_string_tb = PrefixedStringTokenBuilder('NX', False, ['"', "'"])
    picture_tb = PictureTokenBuilder()
    cr_picture_tb = CRPictureTokenBuilder()
    inline_comment_tb = LeadCommentTokenBuilder('*>')
    star_comment_tb = AsteriskCommentTokenBuilder()

    terminators_tb = SingleCharacterTokenBuilder('.', 'statement terminator')

    known_operators = [
      'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
      '+', '-', '*', '/', '**',
      '=', '<>', '>', '>=', '<', '<=',
      'AND', 'OR', 'NOT',
      'B-AND', 'B-NOT', 'B-OR', 'B-XOR',
      ':'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-', 'NOT'
    ]

    groupers = ['(', ')', ',']
    # group_ends = [')']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'ACCEPT', 'ACCESS',
      'ADD',
      'ADD', 'ADDRESS',
      'ADVANCING',
      'AFTER',
      'ALL',
      'ALPHABET',
      'ALPHABETIC', 'ALPHABETIC-LOWER', 'ALPHABETIC-UPPER',
      'ALPHANUMERIC', 'ALPHANUMERIC-EDITED',
      'ALSO',
      'ALTER', 'ALTERNATE', 'AND',
      'ANY',
      'APPLY',
      'ARE', 'AREA', 'AREAS',
      'ASCENDING',
      'ASSIGN', 'AT',
      'AUTHOR',
      'BEFORE',
      'BEGINNING',
      'BELL',
      'BINARY',
      'BLANK', 'BLOCK',
      'BOTTOM',
      'BY',
      'BYTE-LENGTH',
      'CALL', 'CANCEL',
      'CBL',
      'CD',
      'CF', 'CH', 'CHARACTER', 'CHARACTERS',
      'CLOCK-UNITS', 'CLOSE', 'COBOL', 'CODE',
      'CODE-SET',
      'COL',
      'COLLATING',
      'COLS',
      'COLUMN',
      'COMMA',
      'COMMON',
      'COMMUNICATION',
      'COMP',
      'COMPUTATIONAL',
      'COMPUTE', 'CONFIGURATION',
      'CONTAINS',
      'CONTENT', 'CONTINUE',
      'CONTROL', 'CONTROLS',
      'CONVERTING',
      'COPY',
      'CORR', 'CORRESPONDING',
      'COUNT', 'CURRENCY',
      'DATA',
      'DATE', 'DATE-COMPILED', 'DATE-WRITTEN',
      'DAY', 'DAY-OF-WEEK',
      'DE', 'DEBUG-CONTENTS', 'DEBUG-ITEM', 'DEBUG-LINE', 'DEBUG-NAME',
      'DEBUG-SUB-1', 'DEBUG-SUB-2', 'DEBUG-SUB-3',
      'DECIMAL-POINT', 'DECLARATIVES',
      'DELETE',
      'DELIMITED', 'DELIMITER',
      'DEPENDING', 'DESCENDING', 'DESTINATION',
      'DISABLE',
      'DIVIDE', 'DIVISION', 'DOWN',
      'DUPLICATES', 'DYNAMIC',
      'EGI',
      'ELSE',
      'EMI', 'ENABLE', 'END',
      'END-ACCEPT', 'END-ADD', 'END-CALL', 'END-COMPUTE', 'END-DELETE', 'END-DISPLAY',
      'END-DIVIDE', 'END-EVALUATE', 'END-EXEC', 'END-IF',
      'END-MULTIPLY',
      'END-OF-PAGE',
      'END-PERFORM', 'END-READ', 'END-RECEIVE', 'END-RETURN', 'END-REWRITE',
      'END-SEARCH', 'END-START', 'END-STRING', 'END-SUBTRACT', 'END-UNSTRING',
      'END-WRITE',
      'ENTER',
      'ENVIRONMENT',
      'EOL', 'EOP',
      'EQUAL', 'ERROR', 'ESI',
      'EVALUATE',
      'EVERY',
      'EXCEPTION',
      'EXEC',
      'EXIT', 'EXTEND',
      'EXTERNAL',
      'FALSE',
      'FD', 'FILE', 'FILE-CONTROL',
      'FILLER', 'FINAL', 'FIRST',
      'FOOTING', 'FOR', 'FROM', 'FULL',
      'GENERATE', 'GIVING', 'GLOBAL', 'GO',
      'GOBACK',
      'GREATER', 'GROUP',
      'HEADING', 'HIGH-VALUE', 'HIGH-VALUES',
      'I-O', 'I-O-CONTROL',
      'IDENTIFICATION', 'IF',
      'IN', 'INDEX', 'INDEXED',
      'INDICATE',
      'INITIAL', 'INITIALIZE',
      'INITIATE', 'INPUT', 'INPUT-OUTPUT',
      'INSPECT',
      'INSTALLATION', 'INTO', 'INVALID',
      'IS',
      'JUST', 'JUSTIFIED',
      'KEY',
      'LABEL', 'LAST',
      'LEADING', 'LEFT', 'LENGTH', 'LESS', 'LIMIT', 'LIMITS',
      'LINAGE', 'LINAGE-COUNTER',
      'LINE', 'LINE-COUNTER', 'LINES', 'LINKAGE',
      'LOCK', 'LOW-VALUE', 'LOW-VALUES',
      'MEMORY', 'MERGE', 'MESSAGE',
      'MODE', 'MODULES',
      'MOVE', 'MULTIPLE', 'MULTIPLY',
      'NATIVE',
      'NEGATIVE', 'NEXT', 'NO',
      'NOT',
      'NUMBER', 'NUMBERS', 'NUMERIC', 'NUMERIC-EDITED',
      'OBJECT-COMPUTER', 'OCCURS', 'OF', 'OFF', 'OMITTED', 'ON',
      'OPEN', 'OPTIONAL', 'OR',
      'ORDER',
      'ORGANIZATION',
      'OTHER',
      'OUTPUT', 'OVERFLOW',
      'PACKED-DECIMAL', 'PADDING',
      'PAGE', 'PAGE-COUNTER', 'PARAGRAPH',
      'PERFORM', 'PF', 'PH', 'PIC', 'PICTURE',
      'PLUS', 'POINTER', 'POSITION', 'POSITIVE',
      'PRINTING',
      'PROCEDURE', 'PROCEDURES',
      'PROCEED',
      'PROGRAM', 'PROGRAM-ID',
      'PURGE',
      'QUEUE', 'QUOTE', 'QUOTES',
      'RANDOM', 'RD', 'READ',
      'RECEIVE', 'RECORD',
      'RECORDS',
      'REDEFINES', 'REEL', 'REFERENCE',
      'RELATIVE', 'RELEASE',
      'REMAINDER',
      'REMOVAL',
      'RENAMES', 'REPLACE', 'REPLACING', 'REPORT', 'REPORTING', 'REPORTS',
      'RERUN', 'RESERVE', 'RESET', 'RESUME', 'RETRY', 'RETURN',
      'REVERSED', 'REWIND', 'REWRITE',
      'RF', 'RH', 'RIGHT', 'ROUNDED', 'RUN',
      'SAME', 'SD', 'SEARCH', 'SECTION', 'SECURE', 'SECURITY',
      'SEGMENT', 'SEGMENT-LIMIT', 'SELECT',
      'SEND', 'SENTENCE',
      'SEPARATE',
      'SEQUENCE', 'SEQUENTIAL',
      'SET',
      'SIGN', 'SIZE',
      'SORT',
      'SORT-MERGE',
      'SOURCE', 'SOURCE-COMPUTER', 'SPACE', 'SPACES', 'SPECIAL-NAMES',
      'STANDARD',
      'STANDARD-1', 'STANDARD-2',
      'START',
      'STATUS', 'STOP', 'STRING',
      'SUB-QUEUE-1', 'SUB-QUEUE-2', 'SUB-QUEUE-3',
      'SUBTRACT', 'SUM',
      'SUPPRESS', 'SYMBOLIC', 'SYNC', 'SYNCHRONIZED',
      'TABLE',
      'TALLY',
      'TALLYING',
      'TAPE', 'TERMINAL', 'TERMINATE', 'TEST',
      'TEXT', 'THAN', 'THEN', 'THROUGH', 'THRU', 'TIME', 'TIMES',
      'TITLE', 'TO',
      'TOP',
      'TRAILING',
      'TRUE',
      'TYPE',
      'UNIT', 'UNSTRING',
      'UNTIL', 'UP', 'UPON', 'USAGE', 'USE', 'USING',
      'VALUE', 'VALUES', 'VARYING',
      'WHEN',
      'WITH', 'WORDS', 'WORKING-STORAGE', 'WRITE',
      'ZERO', 'ZEROES', 'ZEROS'
    ]

    keywords_2002 = [
      'ACTIVE-CLASS',
      'ALIGNED',
      'ALLOCATE',
      'ANYCASE',
      'ARITHMETIC',
      'AUTO',
      'AUTOMATIC',
      'BACKGROUND-COLOR',
      'BASED',
      'BASIS',
      'BINARY-CHAR', 'BINARY-DOUBLE', 'BINARY-LONG',
      'BINARY-SHORT', 'BIT',
      'BLINK',
      'BOOLEAN',
      'CENTER',
      'CLASS', 'CLASS-ID',
      'CLASSIFICATION',
      'COLUMNS',
      'COM-REG',
      'CONDITION',
      'CONSTANT',
      'CRT', 'CURSOR',
      'CYCLE',
      'DATA-POINTER',
      'DBCS',
      'DEBUGGING',
      'DETAIL',
      'DISPLAY', 'DISPLAY-1', 'DISPLAY-OF',
      'EC',
      'EGCS',
      'EJECT',
      'END-INVOKE',
      'ENDING',
      'ENTRY-CONVENTION', 'ENTRY-FIELD',
      'EO', 'EOS',
      'ERASE',
      'EXCEPTION-OBJECT',
      'EXCLUSIVE', 'EXPANDS', 'EXTERN',
      'FACTORY',
      'FLOAT-EXTENDED', 'FLOAT-LONG', 'FLOAT-SHORT',
      'FOREGROUND-COLOR', 'FOREVER', 'FORMAT', 'FREE',
      'FUNCTION', 'FUNCTION-ID',
      'GET',
      'GROUP-USAGE',
      'HIGHLIGHT',
      'IGNORING', 'IMPLEMENTS',
      'INHERITS',
      'INITIALIZED',
      'INSERT',
      'INTERFACE', 'INTERFACE-ID',
      'INTRINSIC', 'INVOKE',
      'KANJI',
      'LC_ALL', 'LC_COLLATE', 'LC_CTYPE', 'LC_MESSAGES', 'LC_MONEY', 'LC_NUMERIC', 'LC_TIME',
      'LOCAL-STORAGE', 'LOCALE', 'LOWLIGHT',
      'MANUAL',
      'METACLASS',
      'METHOD', 'METHOD-ID',
      'MINUS',
      'MORE-LABELS', 
      'NATIONAL', 'NATIONAL-EDITED', 'NATIONAL-OF',
      'NATIVE_BINARY', 'NESTED', 'NEW', 'NONE', 'NORMAL',
      'NULL', 'NULLS',
      'OBJECT', 'OBJECT-REFERENCE', 'ONLY', 'OPTIONS',
      'OVERRIDE',
      'PHYSICAL',
      'PRESENT', 'PREVIOUS',
      'PROCEDURE-POINTER',
      'PROCESSING', 'PROGRAM-POINTER',
      'PROPERTY', 'PROTOTYPE',
      'RAISE', 'RAISING',
      'READY',
      'RECURSIVE',
      'REFERENCES',
      'RELATION',
      'RELOAD',
      'REPOSITORY', 'REQUIRED',
      'RETURN-CODE', 'RETURNING',
      'ROUNDING',
      'SCREEN', 'SECONDS',
      'SELF',
      'SERVICE',
      'SHARING',
      'SHIFT-IN', 'SHIFT-OUT',
      'SIGNED',
      'SKIP1', 'SKIP2', 'SKIP3',
      'SORT-CONTROL', 'SORT-CORE-SIZE', 'SORT-FILE-SIZE',
      'SORT-MESSAGE', 'SORT-MODE-SIZE', 'SORT-RETURN',
      'SOURCES', 'STATEMENT', 'STEP', 'STRONG',
      'SUPER', 'SYMBOL',
      'SYSTEM-DEFAULT',
      'TRACE', 'TYPEDEF',
      'UCS-4', 'UNDERLINE', 'UNIVERSAL', 'UNLOCK', 'UNSIGNED', 'USER-DEFAULT',
      'UTF-16', 'UTF-8',
      'VAL-STATUS', 'VALID', 'VALIDATE', 'VALIDATE-STATUS',
      'WHEN-COMPILED',
      'WRITE-ONLY',
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
      'ASCII', 'EBCDIC',
      'BINARY-C-LONG', 'BINARY-SEQUENTIAL',
      'CARD-PUNCH', 'CARD-READER', 'CASSETTE',
      'CHAIN', 'CHAINING',
      'COLOR',
      'COMMAND-LINE',
      'COMMIT',
      'COMP-1', 'COMP-2', 'COMP-3', 'COMP-4', 'COMP-5', 'COMP-6', 'COMP-X',
      'COMPUTATIONAL-1', 'COMPUTATIONAL-2', 'COMPUTATIONAL-3',
      'COMPUTATIONAL-4', 'COMPUTATIONAL-5', 'COMPUTATIONAL-6',
      'COMPUTATIONAL-X',
      'CONVERSION',
      'CRT-UNDER',
      'DISC', 'DISK',
      'EBCDIC',
      'ECHO',
      'END-CHAIN',
      'ENTRY',
      'ENVIRONMENT-NAME', 'ENVIRONMENT-VALUE',
      'ESCAPE',
      'F',
      'FILE-ID', 'FIXED',
      'FLOAT-DECIMAL-7',
      'ID', 'IGNORE',
      'KEPT', 'KEYBOARD',
      'LEFT-JUSTIFY', 'LEFTLINE', 'LINE-SEQUENTIAL', 'LOWER',
      'MAGNETIC-TAPE',
      'NAME', 'NO-ECHO', 'NOTHING', 'NULLS',
      'OVERLINE',
      'PRINT', 'PRINTER', 'PRINTER-1',
      'PROCEDURE-POINTER', 'PROCEDURES',
      'PROMPT',
      'PROTECTED',
      'RECORDING', 'REVERSE',
      'RIGHT-JUSTIFY',
      'ROLLBACK',
      'S',
      'SCROLL',
      'SIGNED-INT', 'SIGNED-LONG', 'SIGNED-SHORT',
      'SPACE-FILL',
      'STATIC',
      'STDCALL', 'SYSTEM-OFFSET',
      'TAB',
      'TIME-OUT',
      'TRAILING-SIGN',
      'U',
      'UNSIGNED-INT', 'UNSIGNED-LONG', 'UNSIGNED-SHORT',
      'UPDATE', 'UPPER',
      'USER',
      'V',
      'VARIABLE',
      'WAIT',
      'WRAP',
      'ZERO-FILL'
    ]

    keywords_acu = [
      '3-D',
      'ACTION',
      'ACTIVE-X',
      'ADJUSTABLE-COLUMNS',
      'ALIGNMENT',
      'AUTO-DECIMAL', 'AUTO-SPIN',
      'BACKGROUND-HIGH', 'BACKGROUND-LOW', 'BACKGROUND-STANDARD',
      'BAR',
      'BITMAP', 'BITMAP-END', 'BITMAP-HANDLE', 'BITMAP-NUMBER', 'BITMAP-START',
      'BITMAP-TRAILING', 'BITMAP-TRANSPARENT-COLOR', 'BITMAP-WIDTH',
      'BOX', 'BOXED', 'BUSY', 'BUTTONS',
      'CALENDAR-FONT',
      'CANCEL-BUTTON',
      'CELL', 'CELL-COLOR', 'CELL-DATA', 'CELL-FONT', 'CELL-PROTECTION',
      'CENTERED-HEADING',
      'CENTURY-DATE',
      'CHECK-BOX',
      'CLEAR-SELECTION', 'CLINE', 'CLINES',
      'COLORS',
      'COLUMN-COLOR', 'COLUMN-DIVIDERS', 'COLUMN-FONT', 'COLUMN-HEADINGS',
      'COLUMN-PROTECTION',
      'COMBO-BOX',
      'COPY-SELECTION',
      'CSIZE',
      'CURSOR-COL', 'CURSOR-COLOR', 'CURSOR-FRAME-WIDTH', 'CURSOR-ROW',
      'CURSOR-X', 'CURSOR-Y', 'CUSTOM-PRINT-TEMPLATE',
      'DASHED',
      'DATA-COLUMNS', 'DATA-TYPES',
      'DATE-ENTRY',
      'DEFAULT-BUTTON', 'DEFAULT-FONT',
      'DESTROY',
      'DISPLAY-COLUMNS', 'DISPLAY-FORMAT',
      'DOTDASH', 'DOTTED', 'DOUBLE',
      'DRAG-COLOR', 'DROP-DOWN', 'DROP-LIST',
      'END-COLOR', 'END-MODIFY',
      'ENGRAVED', 'ENSURE-VISIBLE',
      'ENTRY-FIELD', 'ENTRY-REASON', 'ESCAPE-BUTTON',
      'EVENT', 'EVENT-LIST', 'EXCEPTION-VALUE',
      'EXPAND', 'EXTERNAL-FORM',
      'FILE-NAME', 'FILE-POS',
      'FILL-COLOR', 'FILL-COLOR-2', 'FILL-PERCENT',
      'FINISH-REASON',
      'FIXED-FONT', 'FIXED-WIDTH', 'FLAT', 'FLAT-BUTTONS',
      'FLOAT', 'FLOATING', 'FONT', 'FRAME', 'FRAMED', 'FULL-HEIGHT',
      'GRID', 'GO-BACK', 'GO-FORWARD', 'GO-HOME', 'GO-SEARCH',
      'GRAPHICAL', 'GRID', 'GROUP-VALUE',
      'HANDLE', 'HAS-CHILDREN', 'HEADING-COLOR', 'HEADING-DIVIDER-COLOR',
      'HEADING-FONT',
      'HEAVY', 'HEIGHT-IN-CELLS', 'HIDDEN-DATA',
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
      'SELECT-ALL', 'SELECTION-INDEX', 'SELECTION-TEXT', 'SELF-ACT',
      'SEPARATION', 'SHADING', 'SHADOW', 'SHORT-DATE', 'SHOW-LINES',
      'SHOW-NONE', 'SHOW-SEL-ALWAYS', 'SMALL-FONT', 'SORT-ORDER',
      'SPINNER', 'SQUARE', 'START-X', 'START-Y', 'STATIC-LIST', 'STATUS-BAR', 'STATUS-TEXT',
      'STYLE', 'SUBWINDOW', 'TAB-TO-ADD', 'TAB-TO-DELETE',
      'TEMPORARY', 'TERMINATION-VALUE', 'THREAD', 'THREADS', 'THUMB-POSITION',
      'TILED-HEADINGS', 'TITLE', 'TITLE-POSITION', 'TRADITIONAL-FONT',
      'TRAILING-SHIFT', 'TRANSPARENT', 'TREE-VIEW',
      'UNFRAMED', 'UNSORTED', 'USE-ALT', 'USE-RETURN', 'USE TAB',
      'VALUE-FORMAT', 'VARIANT', 'VERTICAL', 'VERY-HEAVY',
      'VIRTUAL-WIDTH', 'VPADDING', 'VSCROLL', 'VSCROLL-BAR', 'VSCROLL-POS', 'VTOP',
      'WEB-BROWSER', 'WIDTH', 'WIDTH-IN-CELLS', 'WINDOW', 'X', 'Y'
    ]

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

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    cobol_preprocessor_tb = CobolPreprocessorTokenBuilder()

    exec_tb = BlockTokenBuilder('EXEC', 'END-EXEC', 'exec block')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
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
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      string_tb,
      n_string_tb,
      nx_string_tb,
      inline_comment_tb,
      star_comment_tb,
      cobol_preprocessor_tb,
      exec_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens += tokenizer.tokenize(code)

    self.convert_numbers_to_pictures()
    self.convert_numbers_to_levels()

    expected_keyword_confidence = self.check_expected_keywords()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # self.calc_operator_3_confidence(group_ends)
    # operand_types = ['number', 'string', 'symbol']
    # self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_picture_confidence()
    self.confidences['expected_keywords'] = expected_keyword_confidence
    self.calc_statistics()
