import string
from Token import Token
from Examiner import Examiner
from CobolExaminer import CobolExaminer
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
  BlockTokenBuilder
)
from CobolTokenBuilders import (
  CobolIdentifierTokenBuilder,
  PictureTokenBuilder,
  CRPictureTokenBuilder,
  CobolPreprocessorTokenBuilder
)
from Tokenizer import Tokenizer

class CobolFixedFormatExaminer(CobolExaminer):
  def __init__(self, code, year, extension, tab_size, wide):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, True)
    real_exponent_tb = RealExponentTokenBuilder(False, True, 'E')
    identifier_tb = CobolIdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], True, True)
    picture_tb = PictureTokenBuilder()
    cr_picture_tb = CRPictureTokenBuilder()

    terminators = ['.']

    terminators_tb = ListTokenBuilder(terminators, 'statement terminator', False)

    known_operators = [
      'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
      '+', '-', '*', '/', '**',
      '=', '<>', '>', '>=', '<', '<=',
      'AND', 'OR', 'NOT',
      ':'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-'
    ]

    groupers = ['(', ')', ',']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'ACCEPT', 'ACCESS',
      'ADD', 'ADDRESS', 'ADVANCING',
      'AFTER', 'ALL',
      'ALPHABETIC', 'ALPHABETIC-LOWER', 'ALPHABETIC-UPPER',
      'ALPHANUMERIC', 'ALPHANUMERIC-EDITED',
      'ALTER', 'ALTERNATE', 'AND',
      'APPLY', 'ARE', 'AREA', 'AREAS',
      'ASCENDING', 'ASSIGN', 'AT', 'AUTHOR',
      'BEFORE',
      'BLANK', 'BLOCK',
      'BY',
      'CALL', 'CANCEL',
      'CD', 'CF', 'CH', 'CHARACTER', 'CHARACTERS',
      'CLOCK-UNITS', 'CLOSE', 'COBOL', 'CODE',
      'COLUMN',
      'COMMA',
      'COMMUNICATION',
      'COMP',
      'COMPUTATIONAL',
      'COMPUTE', 'CONFIGURATION',
      'CONTAINS',
      'CONTROL', 'CONTROLS',
      'COPY', 'CORR', 'CORRESPONDING',
      'COUNT', 'CURRENCY',
      'DATA',
      'DATE',
      'DATE-COMPILED', 'DATE-WRITTEN',
      'DE', 'DEBUG-CONTENTS', 'DEBUG-ITEM', 'DEBUG-LINE', 'DEBUG-NAME',
      'DEBUG-SUB-1', 'DEBUG-SUB-2', 'DEBUG-SUB-3',
      'DECIMAL-POINT', 'DECLARATIVES',
      'DELIMITED', 'DELIMITER',
      'DEPENDING', 'DESCENDING', 'DESTINATION', 'DETAIL',
      'DISABLE',
      'DISPLAY',
      'DIVIDE', 'DIVISION', 'DOWN',
      'EGI',
      'ELSE', 'EMI', 'ENABLE', 'END',
      'ENTER',
      'ENVIRONMENT',
      'EQUAL', 'ERROR', 'ESI',
      'EVERY',
      'EXIT', 'EXTEND',
      'FD', 'FILE', 'FILE-CONTROL',
      'FILLER', 'FINAL', 'FIRST', 'FOOTING', 'FOR', 'FROM',
      'GENERATE', 'GIVING', 'GLOBAL', 'GO', 'GOBACK',
      'GREATER', 'GROUP',
      'HEADING', 'HIGH-VALUE', 'HIGH-VALUES',
      'I-O', 'I-O-CONTROL',
      'IDENTIFICATION', 'IF', 'IN', 'INDEX', 'INDEXED',
      'INDICATE',
      'INITIAL',
      'INITIATE', 'INPUT', 'INPUT-OUTPUT',
      'INSTALLATION', 'INTO', 'INVALID',
      'IS',
      'JUST', 'JUSTIFIED',
      'KEY',
      'LABEL', 'LAST', 'LEADING', 'LEFT', 'LENGTH', 'LESS', 'LIMIT', 'LIMITS',
      'LINE', 'LINE-COUNTER', 'LINES', 'LINKAGE',
      'LOCK', 'LOW-VALUE', 'LOW-VALUES',
      'MEMORY', 'MERGE', 'MESSAGE',
      'MODE', 'MODULES',
      'MOVE', 'MULTIPLE', 'MULTIPLY',
      'NEGATIVE', 'NEXT', 'NO',
      'NOT',
      'NUMBER', 'NUMERIC', 'NUMERIC-EDITED',
      'OBJECT-COMPUTER', 'OCCURS', 'OF', 'OFF', 'OMITTED', 'ON',
      'OPEN', 'OPTIONAL', 'OR',
      'OUTPUT', 'OVERFLOW',
      'PAGE', 'PAGE-COUNTER',
      'PERFORM', 'PF', 'PH', 'PIC', 'PICTURE',
      'PLUS', 'POINTER', 'POSITION', 'POSITIVE',
      'PROCEDURE',
      'PROCEED',
      'PROGRAM', 'PROGRAM-ID',
      'QUEUE', 'QUOTE', 'QUOTES',
      'RANDOM', 'RD', 'READ',
      'RECEIVE', 'RECORD',
      'RECORDS',
      'REDEFINES', 'REEL', 'REFERENCE',
      'RELATIVE', 'RELEASE',
      'REMAINDER',
      'RENAMES', 'REPLACE', 'REPLACING', 'REPORT', 'REPORTING', 'REPORTS',
      'RERUN', 'RESERVE', 'RESET', 'RETURN',
      'REVERSED', 'REWIND', 'REWRITE',
      'RF', 'RH', 'RIGHT', 'ROUNDED', 'RUN',
      'SAME', 'SD', 'SEARCH', 'SECTION', 'SECURITY',
      'SEGMENT', 'SEGMENT-LIMIT', 'SELECT',
      'SEND', 'SENTENCE',
      'SEQUENCE', 'SEQUENTIAL',
      'SET',
      'SIGN', 'SIZE',
      'SORT',
      'SOURCE', 'SOURCE-COMPUTER', 'SPACE', 'SPACES', 'SPECIAL-NAMES',
      'STANDARD',
      'STATUS', 'STOP', 'STRING',
      'SUB-QUEUE-1', 'SUB-QUEUE-2', 'SUB-QUEUE-3',
      'SUBTRACT', 'SUM',
      'SUPPRESS', 'SYMBOLIC', 'SYNC', 'SYNCHRONIZED',
      'TABLE',
      'TALLY',
      'TAPE', 'TERMINAL', 'TERMINATE', 'TEST',
      'TEXT', 'THAN', 'THEN', 'THROUGH', 'THRU', 'TIME', 'TIMES',
      'TITLE', 'TO',
      'TYPE',
      'UNIT', 'UNSTRING', 'UNTIL', 'UP', 'UPON', 'USAGE', 'USE', 'USING',
      'VALUE', 'VALUES', 'VARYING',
      'WHEN',
      'WITH', 'WORDS', 'WORKING-STORAGE', 'WRITE',
      'ZERO', 'ZEROES', 'ZEROS'
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
      'CODE-SET',
      'COLLATING',
      'COMMON',
      'DAY',
      'DELETE',
      'DEBUGGING',
      'DUPLICATES', 'DYNAMIC',
      'END-OF-PAGE',
      'EOP',
      'EXCEPTION',
      'INSPECT',
      'LINAGE', 'LINAGE-COUNTER',
      'NATIVE',
      'ORGANIZATION',
      'PACKED-DECIMAL', 'PADDING',
      'PRINTING',
      'PROCEDURES',
      'REFERENCES',
      'REMOVAL',
      'SEPARATE',
      'SORT-MERGE',
      'STANDARD-1', 'STANDARD-2', 'START',
      'TALLYING',
      'TOP',
      'TRAILING',
    ]

    keywords_85 = [
      'ALPHABET',
      'ANY',
      'BINARY',
      'CONTENT', 'CONTINUE',
      'CONVERTING',
      'DAY-OF-WEEK',
      'END-ADD', 'END-CALL', 'END-COMPUTE', 'END-DELETE', 'END-DIVIDE',
      'END-EVALUATE', 'END-IF',
      'END-MULTIPLY',
      'END-PERFORM', 'END-READ', 'END-RECEIVE', 'END-RETURN', 'END-REWRITE',
      'END-SEARCH', 'END-START', 'END-STRING', 'END-SUBTRACT', 'END-UNSTRING',
      'END-WRITE',
      'EVALUATE',
      'EXTERNAL',
      'FALSE',
      'INITIALIZE',
      'ORDER',
      'OTHER',
      'PURGE',
      'TRUE'
    ]

    if year == '68':
      keywords += keywords_68_only

    if year in ['68', '74']:
      keywords += keywords_74

    if year in ['68', '74', '85']:
      keywords += keywords_85

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
      cobol_preprocessor_tb,
      exec_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    self.tokens = self.TokenizeCode(code, tab_size, tokenizer, wide)
    self.tokens = self.combineAdjacentWhitespace(self.tokens)

    expected_keyword_confidence = self.CheckExpectedKeywords()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # self.calc_operator_3_confidence()
    # self.calc_operand_confidence()
    self.calc_keyword_confidence()
    self.calc_picture_confidence()
    if not wide:
      self.calc_line_length_confidence(code, 80)
    self.confidences['expected_keywords'] = expected_keyword_confidence
