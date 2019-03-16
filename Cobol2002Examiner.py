import string
from Token import Token
from Examiner import Examiner
from CobolExaminer import CobolExaminer
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  PrefixedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder,
  LeadCommentTokenBuilder,
  BlockTokenBuilder
)
from CobolTokenBuilders import (
  CobolIdentifierTokenBuilder,
  PictureTokenBuilder,
  CRPictureTokenBuilder,
  CobolPreprocessorTokenBuilder,
  AsteriskCommentTokenBuilder
)
from Tokenizer import Tokenizer

class Cobol2002Examiner(CobolExaminer):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, True)
    real_exponent_tb = RealExponentTokenBuilder(False, True, 'E')
    identifier_tb = CobolIdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], True, False)
    n_string_tb = PrefixedStringTokenBuilder('N', False, ['"', "'"])
    nx_string_tb = PrefixedStringTokenBuilder('NX', False, ['"', "'"])
    picture_tb = PictureTokenBuilder()
    cr_picture_tb = CRPictureTokenBuilder()
    inline_comment_tb = LeadCommentTokenBuilder('*>')
    star_comment_tb = AsteriskCommentTokenBuilder()

    terminators = ['.']

    terminators_tb = ListTokenBuilder(terminators, 'statement terminator', False)

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

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'ACCEPT', 'ACCESS',
      'ACTIVE-CLASS',
      'ADD', 'ADDRESS',
      'ADVANCING',
      'AFTER', 'ALIGNED',
      'ALL',
      'ALLOCATE',
      'ALPHABET',
      'ALPHABETIC', 'ALPHABETIC-LOWER', 'ALPHABETIC-UPPER',
      'ALPHANUMERIC', 'ALPHANUMERIC-EDITED',
      'ALSO',
      'ALTER', 'ALTERNATE', 'AND',
      'ANY', 'ANYCASE',
      'APPLY',
      'ARE', 'AREA', 'AREAS',
      'ARITHMETIC',
      'ASCENDING',
      'ASSIGN', 'AT', 'AUTO',
      'AUTHOR',
      'AUTOMATIC',
      'BACKGROUND-COLOR',
      'BASED',
      'BASIS',
      'BEFORE',
      'BEGINNING',
      'BELL',
      'BINARY',
      'BINARY-CHAR', 'BINARY-DOUBLE', 'BINARY-LONG',
      'BINARY-SHORT', 'BIT',
      'BLANK', 'BLINK', 'BLOCK',
      'BOOLEAN',
      'BOTTOM',
      'BY',
      'BYTE-LENGTH',
      'CALL', 'CANCEL',
      'CBL',
      'CD',
      'CENTER',
      'CF', 'CH', 'CHARACTER', 'CHARACTERS',
      'CLASS', 'CLASS-ID',
      'CLOCK-UNITS', 'CLOSE', 'COBOL', 'CODE',
      'CODE-SET',
      'COL',
      'COLLATING',
      'COLS',
      'COLUMN',
      'COLUMNS',
      'COM-REG',
      'COMMA',
      'COMMON',
      'COMMUNICATION',
      'COMP',
      'COMPUTATIONAL',
      'COMPUTE', 'CONDITION', 'CONFIGURATION',
      'CONSTANT', 'CONTAINS',
      'CONTENT', 'CONTINUE',
      'CONTROL', 'CONTROLS',
      'CONVERTING',
      'COPY',
      'CORR', 'CORRESPONDING',
      'COUNT', 'CRT', 'CURRENCY', 'CURSOR',
      'DATA', 'DATA-POINTER',
      'DATE', 'DATE-COMPILED', 'DATE-WRITTEN',
      'DAY', 'DAY-OF-WEEK',
      'DBCS',
      'DE', 'DEBUGGING', 'DEBUG-CONTENTS', 'DEBUG-ITEM', 'DEBUG-LINE', 'DEBUG-NAME',
      'DEBUG-SUB-1', 'DEBUG-SUB-2', 'DEBUG-SUB-3',
      'DEBUGGING',
      'DECIMAL-POINT', 'DECLARATIVES',
      'DELETE',
      'DELIMITED', 'DELIMITER',
      'DEPENDING', 'DESCENDING', 'DESTINATION',
      'DETAIL',
      'DISABLE',
      'DISPLAY', 'DISPLAY-1', 'DISPLAY-OF',
      'DIVIDE', 'DIVISION', 'DOWN',
      'DUPLICATES', 'DYNAMIC',
      'EC',
      'EGCS',
      'EGI',
      'EJECT',
      'ELSE',
      'EMI', 'ENABLE', 'END',
      'END-ACCEPT', 'END-ADD', 'END-CALL', 'END-COMPUTE', 'END-DELETE', 'END-DISPLAY',
      'END-DIVIDE', 'END-EVALUATE', 'END-IF',
      'END-INVOKE',
      'END-MULTIPLY',
      'END-OF-PAGE',
      'END-PERFORM', 'END-READ', 'END-RECEIVE', 'END-RETURN', 'END-REWRITE',
      'END-SEARCH', 'END-START', 'END-STRING', 'END-SUBTRACT', 'END-UNSTRING',
      'END-WRITE',
      'ENDING',
      'ENTER',
      'ENTRY-CONVENTION', 'ENTRY-FIELD',
      'ENVIRONMENT',
      'EO',
      'EOL', 'EOP', 'EOS',
      'EQUAL', 'ERASE', 'ERROR', 'ESI',
      'EVALUATE',
      'EVERY',
      'EXCEPTION', 'EXCEPTION-OBJECT',
      'EXCLUSIVE',
      'EXIT', 'EXPANDS', 'EXTEND',
      'EXTERN', 'EXTERNAL',
      'FACTORY',
      'FALSE',
      'FD', 'FILE', 'FILE-CONTROL',
      'FILLER', 'FINAL', 'FIRST',
      'FLOAT-EXTENDED', 'FLOAT-LONG', 'FLOAT-SHORT',
      'FOOTING', 'FOR', 'FOREGROUND-COLOR', 'FOREVER', 'FORMAT', 'FREE', 'FROM', 'FULL',
      'FUNCTION', 'FUNCTION-ID',
      'GENERATE', 'GET', 'GIVING', 'GLOBAL', 'GO',
      'GOBACK',
      'GREATER', 'GROUP', 'GROUP-USAGE',
      'HEADING', 'HIGH-VALUE', 'HIGH-VALUES', 'HIGHLIGHT',
      'I-O', 'I-O-CONTROL',
      'IDENTIFICATION', 'IF',
      'IGNORING', 'IMPLEMENTS',
      'IN', 'INDEX', 'INDEXED',
      'INDICATE',
      'INHERITS',
      'INITIAL', 'INITIALIZE', 'INITIALIZED',
      'INITIATE', 'INPUT', 'INPUT-OUTPUT',
      'INSERT',
      'INSPECT',
      'INTERFACE', 'INTERFACE-ID',
      'INSTALLATION', 'INTO', 'INTRINSIC', 'INVALID',
      'INVOKE',
      'IS',
      'JUST', 'JUSTIFIED',
      'KANJI',
      'KEY',
      'LABEL', 'LAST',
      'LC_ALL', 'LC_COLLATE', 'LC_CTYPE', 'LC_MESSAGES', 'LC_MONEY', 'LC_NUMERIC', 'LC_TIME',
      'LEADING', 'LEFT', 'LENGTH', 'LESS', 'LIMIT', 'LIMITS',
      'LINAGE', 'LINAGE-COUNTER',
      'LINE', 'LINE-COUNTER', 'LINES', 'LINKAGE',
      'LOCAL-STORAGE', 'LOCALE',
      'LOCK', 'LOW-VALUE', 'LOW-VALUES', 'LOWLIGHT',
      'MANUAL',
      'MEMORY', 'MERGE', 'MESSAGE',
      'METACLASS',
      'METHOD', 'METHOD-ID',
      'MINUS',
      'MODE', 'MODULES',
      'MORE-LABELS', 
      'MOVE', 'MULTIPLE', 'MULTIPLY',
      'NATIONAL', 'NATIONAL-EDITED', 'NATIONAL-OF',
      'NATIVE',
      'NATIVE_BINARY',
      'NEGATIVE', 'NESTED', 'NEW', 'NEXT', 'NO',
      'NONE', 'NORMAL', 'NOT',
      'NULL', 'NULLS',
      'NUMBER', 'NUMBERS', 'NUMERIC', 'NUMERIC-EDITED',
      'OBJECT',
      'OBJECT-COMPUTER', 'OBJECT-REFERENCE', 'OCCURS', 'OF', 'OFF', 'OMITTED', 'ON',
      'ONLY', 'OPEN', 'OPTIONAL', 'OPTIONS', 'OR',
      'ORDER',
      'ORGANIZATION',
      'OTHER',
      'OUTPUT', 'OVERFLOW',
      'OVERRIDE',
      'PACKED-DECIMAL', 'PADDING',
      'PAGE', 'PAGE-COUNTER', 'PARAGRAPH',
      'PASSWORD',
      'PERFORM', 'PF', 'PH', 'PIC', 'PICTURE',
      'PLUS', 'POINTER', 'POSITION', 'POSITIVE',
      'PRINTING',
      'PROCEDURE', 'PROCEDURE-POINTER', 'PROCEDURES',
      'PROCEED',
      'PROCESSING',
      'PROGRAM', 'PROGRAM-ID', 'PROGRAM-POINTER',
      'PROHIBITED', 'PROPERTY', 'PROTOTYPE',
      'PURGE',
      'QUEUE', 'QUOTE', 'QUOTES',
      'RAISE', 'RAISING', 'RANDOM', 'RD', 'READ',
      'READY',
      'RECEIVE', 'RECORD',
      'RECORDS',
      'RECURSIVE',
      'REDEFINES', 'REEL', 'REFERENCE',
      'REFERENCES',
      'RELATION', 'RELATIVE', 'RELEASE',
      'RELOAD',
      'REMAINDER',
      'REMOVAL',
      'RENAMES', 'REPLACE', 'REPLACING', 'REPORT', 'REPORTING', 'REPORTS',
      'REPOSITORY', 'REQUIRED',
      'RERUN', 'RESERVE', 'RESET', 'RESUME', 'RETRY', 'RETURN',
      'RETURN-CODE', 'RETURNING',
      'REVERSED', 'REWIND', 'REWRITE',
      'RF', 'RH', 'RIGHT', 'ROUNDED', 'ROUNDING', 'RUN',
      'SAME', 'SD', 'SCREEN', 'SEARCH', 'SECONDS', 'SECTION', 'SECURE', 'SECURITY',
      'SEGMENT', 'SEGMENT-LIMIT', 'SELECT',
      'SELF',
      'SEND', 'SENTENCE',
      'SEPARATE',
      'SEQUENCE', 'SEQUENTIAL',
      'SERVICE',
      'SET',
      'SHARING',
      'SHIFT-IN', 'SHIFT-OUT',
      'SIGN', 'SIGNED', 'SIZE',
      'SKIP1', 'SKIP2', 'SKIP3',
      'SORT',
      'SORT-CONTROL', 'SORT-CORE-SIZE', 'SORT-FILE-SIZE',
      'SORT-MERGE',
      'SORT-MESSAGE', 'SORT-MODE-SIZE', 'SORT-RETURN',
      'SOURCE', 'SOURCE-COMPUTER', 'SOURCES', 'SPACE', 'SPACES', 'SPECIAL-NAMES',
      'STANDARD',
      'STANDARD-1', 'STANDARD-2',
      'START',
      'STATEMENT',
      'STATUS', 'STEP',  'STOP', 'STRING', 'STRONG',
      'SUB-QUEUE-1', 'SUB-QUEUE-2', 'SUB-QUEUE-3',
      'SUBTRACT', 'SUM',
      'SUPER',
      'SUPPRESS', 'SYMBOL,' 'SYMBOLIC', 'SYNC', 'SYNCHRONIZED',
      'SYSTEM-DEFAULT',
      'TABLE',
      'TALLY',
      'TALLYING',
      'TAPE', 'TERMINAL', 'TERMINATE', 'TEST',
      'TEXT', 'THAN', 'THEN', 'THROUGH', 'THRU', 'TIME', 'TIMES',
      'TITLE', 'TO',
      'TOP',
      'TRACE',
      'TRAILING',
      'TRUE',
      'TYPE', 'TYPEDEF',
      'UCS-4', 'UNDERLINE',
      'UNIT', 'UNIVERSAL', 'UNLOCK', 'UNSIGNED', 'UNSTRING',
      'UNTIL', 'UP', 'UPON', 'USAGE', 'USE', 'USER-DEFAULT', 'USING',
      'UTF-16', 'UTF-8',
      'VAL-STATUS', 'VALID', 'VALIDATE', 'VALIDATE-STATUS', 'VALUE', 'VALUES', 'VARYING',
      'WHEN',
      'WHEN-COMPILED',
      'WITH', 'WORDS', 'WORKING-STORAGE', 'WRITE',
      'WRITE-ONLY',
      'YYYYDDD', 'YYYYMMDD',
      'ZERO', 'ZEROES', 'ZEROS'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    cobol_preprocessor_tb = CobolPreprocessorTokenBuilder()

    exec_tb = BlockTokenBuilder('EXEC', 'END-EXEC', 'exec block')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
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

    expected_keyword_confidence = self.CheckExpectedKeywords()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # self.calc_operator_3_confidence()
    # self.calc_operand_confidence()
    self.calc_keyword_confidence()
    self.calc_picture_confidence()
    self.confidences['expected_keywords'] = expected_keyword_confidence
