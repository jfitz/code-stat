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
  ListTokenBuilder
)
from CobolTokenBuilders import (
  CobolIdentifierTokenBuilder,
  PictureTokenBuilder,
  CRPictureTokenBuilder,
  CobolPreprocessorTokenBuilder
)
from Tokenizer import Tokenizer

class Cobol74Examiner(CobolExaminer):
  def __init__(self, code, tab_size, wide):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, True)
    real_exponent_tb = RealExponentTokenBuilder(False, True, 'E')
    identifier_tb = CobolIdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], True)
    picture_tb = PictureTokenBuilder()
    cr_picture_tb = CRPictureTokenBuilder()

    terminators = ['.']

    terminators_tb = ListTokenBuilder(terminators, 'statement terminator', False)

    known_operators = [
      'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
      '+', '-', '*', '/', '**',
      '=', '>', '<',
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
      'ACCEPT',
      'ACCESS',
      'ADD',
      'ADDRESS',
      'ADVANCING',
      'AFTER',
      'ALL',
      'ALPHABETIC',
      'ALPHABETIC-LOWER', 
      'ALPHABETIC-UPPER',
      'ALPHANUMERIC',
      'ALPHANUMERIC-EDITED',
      'ALSO',
      'ALTER',
      'ALTERNATE',
      'AND',
      'APPLY', 
      'ARE',
      'AREA',
      'AREAS',
      'ASCENDING',
      'ASSIGN',
      'AT',
      'AUTHOR',

      'BEFORE',
      'BLANK',
      'BLOCK',
      'BOTTOM',
      'BY',

      'CALL',
      'CANCEL',
      'CD',
      'CF',
      'CH',
      'CHARACTER',
      'CHARACTERS',
      'CLOCK-UNITS',
      'CLOSE',
      'COBOL',
      'CODE',
      'CODE-SET',
      'COLLATING',
      'COLUMN',
      'COMMA',
      'COMMUNICATION',
      'COMP',
      'COMPUTATIONAL',
      'COMPUTE',
      'CONFIGURATION',
      'CONTAINS',
      'CONTROL',
      'CONTROLS',
      'COPY',
      'CORR',
      'CORRESPONDING',
      'COUNT',
      'CURRENCY',

      'DATA',
      'DATE',
      'DATE-COMPILED',
      'DATE-WRITTEN',
      'DAY',
      'DE',
      'DEBUG-CONTENTS',
      'DEBUG-ITEM',
      'DEBUG-LINE',
      'DEBUG-NAME',
      'DEBUG-SUB-1',
      'DEBUG-SUB-2',
      'DEBUG-SUB-3',
      'DEBUGGING',
      'DECIMAL-POINT',
      'DECLARATIVES',
      'DELETE',
      'DELIMITED',
      'DELIMITER',
      'DEPENDING',
      'DESCENDING',
      'DESTINATION', 
      'DETAIL',
      'DISABLE',
      'DISPLAY',
      'DIVIDE',
      'DIVISION',
      'DOWN',
      'DUPLICATES',
      'DYNAMIC',

      'EGI',
      'ELSE',
      'EMI',
      'ENABLE',
      'END',
      'END-OF-PAGE',
      'ENTER',
      'ENVIRONMENT',
      'EOP',
      'EQUAL',
      'ERROR',
      'ESI',
      'EVERY',
      'EXCEPTION',
      'EXIT',
      'EXTEND',

      'FD',
      'FILE',
      'FILE-CONTROL',
      'FILLER',
      'FINAL',
      'FIRST',
      'FOOTING',
      'FOR',
      'FROM',

      'GENERATE',
      'GIVING',
      'GLOBAL',
      'GO',
      'GREATER',
      'GROUP',

      'HEADING',
      'HIGH-VALUE',
      'HIGH-VALUES',

      'I-O',
      'I-O-CONTROL',
      'IDENTIFICATION',
      'IF',
      'IN',
      'INDEX',
      'INDEXED',
      'INDICATE',
      'INITIAL',
      'INITIATE',
      'INPUT',
      'INPUT-OUTPUT',
      'INSPECT',
      'INSTALLATION',
      'INTO',
      'INVALID',
      'IS',

      'JUST',
      'JUSTIFIED',
      
      'KEY',

      'LABEL',
      'LAST',
      'LEADING',
      'LEFT',
      'LENGTH',
      'LESS',
      'LIMIT',
      'LIMITS',
      'LINAGE',
      'LINAGE-COUNTER',
      'LINE',
      'LINE-COUNTER',
      'LINES',
      'LINKAGE',
      'LOCK',
      'LOW-VALUE',
      'LOW-VALUES',

      'MEMORY',
      'MERGE',
      'MESSAGE',
      'METACLASS',
      'MODE',
      'MODULES',
      'MOVE',
      'MULTIPLE',
      'MULTIPLY',

      'NATIVE',
      'NEGATIVE',
      'NEXT',
      'NO',
      'NOT',
      'NUMBER',
      'NUMERIC',
      'NUMERIC-EDITED',

      'OBJECT-COMPUTER',
      'OCCURS',
      'OF',
      'OFF',
      'OMITTED',
      'ON',
      'OPEN',
      'OPTIONAL',
      'OR',
      'ORGANIZATION',
      'OUTPUT',
      'OVERFLOW',

      'PAGE',
      'PAGE-COUNTER',
      'PERFORM',
      'PF',
      'PH',
      'PIC',
      'PICTURE',
      'PLUS',
      'POINTER',
      'POSITION',
      'POSITIVE',
      'PRINTING',
      'PROCEDURE',
      'PROCEDURES',
      'PROCEED',
      'PROGRAM',
      'PROGRAM-ID',

      'QUEUE',
      'QUOTE',
      'QUOTES',

      'RANDOM',
      'RD',
      'READ',
      'RECEIVE',
      'RECORD',
      'RECORDS',
      'REDEFINES',
      'REEL',
      'REFERENCE',
      'REFERENCES',
      'RELATIVE',
      'RELEASE',
      'REMAINDER',
      'REMOVAL',
      'RENAMES',
      'REPLACE',
      'REPLACING',
      'REPORT',
      'REPORTING',
      'REPORTS',
      'RERUN',
      'RESERVE', 
      'RESET',
      'RETURN',
      'REVERSED',
      'REWIND',
      'REWRITE',
      'RF',
      'RH',
      'RIGHT',
      'ROUNDED',
      'RUN',

      'SAME',
      'SD',
      'SEARCH',
      'SECTION',
      'SECURITY',
      'SEGMENT',
      'SEGMENT-LIMIT',
      'SELECT',
      'SEND',
      'SENTENCE',
      'SEPARATE',
      'SEQUENCE',
      'SEQUENTIAL',
      'SET',
      'SIGN',
      'SIZE',
      'SORT',
      'SORT-MERGE',
      'SOURCE',
      'SOURCE-COMPUTER',
      'SPACE',
      'SPACES',
      'SPECIAL-NAMES',
      'STANDARD',
      'STANDARD-1',
      'STANDARD-2',
      'START',
      'STATUS',
      'STOP',
      'STRING',
      'SUB-QUEUE-1',
      'SUB-QUEUE-2',
      'SUB-QUEUE-3',
      'SUBTRACT',
      'SUM',
      'SUPPRESS',
      'SYMBOLIC',
      'SYNC',
      'SYNCHRONIZED',

      'TABLE',
      'TALLYING',
      'TAPE',
      'TERMINAL',
      'TERMINATE',
      'TEST',
      'TEXT',
      'THAN',
      'THEN',
      'THROUGH',
      'THRU',
      'TIME',
      'TIMES',
      'TITLE', 
      'TO',
      'TOP',
      'TRAILING',
      'TYPE',

      'UNIT',
      'UNSTRING',
      'UNTIL',
      'UP',
      'UPON',
      'USAGE',
      'USE',
      'USING',

      'VALUE',
      'VALUES',
      'VARYING',

      'WHEN',
      'WITH',
      'WORDS',
      'WORKING-STORAGE',
      'WRITE',

      'ZERO',
      'ZEROES',
      'ZEROS'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    cobol_preprocessor_tb = CobolPreprocessorTokenBuilder()
    
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
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    num_ok_lines = 0
    lines = code.split('\n')
    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      if wide or len(line) <= 80:
        num_ok_lines += 1

    line_length_confidence = 1.0
    if len(lines) > 0:
      line_length_confidence = num_ok_lines / len(lines)

    self.tokens = self.TokenizeCode(code, tab_size, tokenizer, wide)
    self.tokens = self.combineAdjacentWhitespace(self.tokens)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # do not check for two operands in a row
    self.calc_picture_confidence()
    self.confidences['line_length'] = line_length_confidence
