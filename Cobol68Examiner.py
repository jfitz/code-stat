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
  StarCommentTokenBuilder,
  CobolPreprocessorTokenBuilder
)
from Tokenizer import Tokenizer

class Cobol68Examiner(CobolExaminer):
  def __init__(self, code, tab_size):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, True)
    real_exponent_tb = RealExponentTokenBuilder(False, True, 'E')
    identifier_tb = CobolIdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False)
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
      'ACTUAL',
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
      'DE',
      'DEBUG-CONTENTS',
      'DEBUG-ITEM',
      'DEBUG-LINE',
      'DEBUG-NAME',
      'DEBUG-SUB-1',
      'DEBUG-SUB-2',
      'DEBUG-SUB-3',
      'DECIMAL-POINT',
      'DECLARATIVES',
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

      'EGI',
      'ELSE',
      'EMI',
      'ENABLE',
      'END',
      'ENTER',
      'ENVIRONMENT',
      'EQUAL',
      'ERROR',
      'ESI',
      'EVERY',
      'EXAMINE',
      'EXIT',
      'EXTEND',

      'FD',
      'FILE',
      'FILE-CONTROL',
      'FILE-LIMITS',
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
      'GOBACK',
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

      'NEGATIVE',
      'NEXT',
      'NO',
      'NOMINAL',
      'NOT',
      'NOTE',
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
      'PROCEDURE',
      'PROCEED',
      'PROCESSING',
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
      'RELATIVE',
      'RELEASE',
      'REMAINDER',
      'REMARKS',
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
      'SEEK',
      'SEGMENT',
      'SEGMENT-LIMIT',
      'SELECT',
      'SEND',
      'SENTENCE',
      'SEQUENCE',
      'SEQUENTIAL',
      'SET',
      'SIGN',
      'SIZE',
      'SORT',
      'SOURCE',
      'SOURCE-COMPUTER',
      'SPACE',
      'SPACES',
      'SPECIAL-NAMES',
      'STANDARD',
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
      'TALLY',
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
      'TODAY',
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
      self.unknown_operator_tb
    ]

    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    num_ok_lines = 0
    lines = code.split('\n')
    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      if len(line) <= 80:
        num_ok_lines += 1

    line_length_confidence = 1.0
    if len(lines) > 0:
      line_length_confidence = num_ok_lines / len(lines)

    self.tokens = self.TokenizeCode(code, tab_size, tokenizer)
    self.tokens = self.combineAdjacentWhitespace(self.tokens)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # do not check for two operands in a row
    self.calc_picture_confidence()
    self.confidences['line_length'] = line_length_confidence
