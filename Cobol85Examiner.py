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
  StarCommentTokenBuilder,
  CobolPreprocessorTokenBuilder
)
from Tokenizer import Tokenizer

class Cobol85Examiner(CobolExaminer):
  def __init__(self, code, tab_size):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, True)
    real_exponent_tb = RealExponentTokenBuilder(False, True)
    identifier_tb = CobolIdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], True)
    picture_tb = PictureTokenBuilder()

    known_operators = [
      'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
      '+', '-', '*', '/', '**',
      '=', '<>', '>', '>=', '<', '<=',
      'AND', 'OR', 'NOT',
      ':', '.'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

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
      'ALPHABET',
      'ALPHABETIC',
      'ALPHABETIC-LOWER', 
      'ALPHABETIC-UPPER',
      'ALPHANUMERIC',
      'ALPHANUMERIC-EDITED',
      'ALSO',
      'ALTER',
      'ALTERNATE',
      'AND',
      'ANY',
      'APPLY', 
      'ARE',
      'AREA',
      'AREAS',
      'ASCENDING',
      'ASSIGN',
      'AT',
      'AUTHOR',

      'BEFORE',
      'BINARY',
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
      'CLASS',
      'CLOCK-UNITS',
      'CLOSE',
      'COBOL',
      'CODE',
      'CODE-SET',
      'COLLATING',
      'COLUMN',
      'COMMA',
      'COMMON',
      'COMMUNICATION',
      'COMP',
      'COMPUTATIONAL',
      'COMPUTE',
      'CONFIGURATION',
      'CONTAINS',
      'CONTENT',
      'CONTINUE',
      'CONTROL',
      'CONTROLS',
      'CONVERTING',
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
      'DAY-OF-WEEK',
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
      'END-ADD',
      'END-CALL',
      'END-COMPUTE',
      'END-DELETE',
      'END-DIVIDE',
      'END-EVALUATE',
      'END-IF',
      'END-MULTIPLY',
      'END-OF-PAGE',
      'END-PERFORM',
      'END-READ',
      'END-RECEIVE',
      'END-RETURN',
      'END-REWRITE',
      'END-SEARCH',
      'END-START',
      'END-STRING',
      'END-SUBTRACT',
      'END-UNSTRING',
      'END-WRITE',
      'ENTER',
      'ENVIRONMENT',
      'EOP',
      'EQUAL',
      'ERROR',
      'ESI',
      'EVALUATE',
      'EVERY',
      'EXCEPTION',
      'EXIT',
      'EXTEND',
      'EXTERNAL',

      'FALSE',
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
      'INITIALIZE',
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
      'ORDER',
      'ORGANIZATION',
      'OTHER',
      'OUTPUT',
      'OVERFLOW',

      'PACKED-DECIMAL',
      'PADDING',
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
      'PURGE',

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
      'TRUE',
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
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      picture_tb,
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

    lines = code.split('\n')

    self.tokens = []
    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = self.tabs_to_spaces(line, tab_size)

      # break apart the line based on fixed format

      # The COBOL line format is:
      # 1-6: line number or blank (ignored)
      # 7: space or one of *, /, D, d, $, -
      # 8-71: program text
      # 72-: identification, traditionally sequence number (ignored)

      line_number = line[:6]
      line_indicator = line[6:7]
      line_text = line[7:71]
      line_identification = line[72:]

      token = self.TokenizeLineNumber(line_number)
      if token is not None:
        self.tokens.append(token)

      # tokenize the line indicator
      if line_indicator in ['*', '/', 'D', 'd', '$']:
        token = self.TokenizeAltLine(line, line_indicator)
        if token is not None:
          self.tokens.append(token)
      else:
        token = self.TokenizeLineIndicator(line_indicator)
        if token is not None:
          self.tokens.append(token)

        # tokenize the code
        self.tokens += tokenizer.tokenize(line_text)

      # tokenize the line identification
      if len(line_identification) > 0:
        self.tokens.append(Token(line_identification, 'line identification'))

      self.tokens.append(Token('\n', 'newline'))

    self.tokens = self.combineAdjacentWhitespace(self.tokens)

    num_known_tokens = self.count_valid_tokens(self.tokens)
    num_invalid_operators = self.count_invalid_operators(self.tokens)
    num_known_operators = self.count_known_operators(self.tokens)

    # check expected keywords
    counts = {
      'IDENTIFICATION': 0,
      'ENVIRONMENT': 0,
      'DATA': 0,
      'PROCEDURE': 0
    }

    tokens = self.drop_whitespace(self.tokens)
    tokens = self.drop_comments(tokens)

    prev_text = ''
    for token in tokens:
      text = token.text

      if text == 'DIVISION' and prev_text in ['IDENTIFICATION', 'ID']:
        counts['IDENTIFICATION'] += 1
      if text == 'DIVISION' and prev_text == 'ENVIRONMENT':
        counts['ENVIRONMENT'] += 1
      if text == 'DIVISION' and prev_text == 'DATA':
        counts['DATA'] += 1
      if text == 'DIVISION' and prev_text == 'PROCEDURE':
        counts['PROCEDURE'] += 1

      prev_text = text
    
    expected_keyword_confidence = 0.50
    if counts['IDENTIFICATION'] == 1:
      expected_keyword_confidence += 0.125
    if counts['ENVIRONMENT'] == 1:
      expected_keyword_confidence += 0.125
    if counts['DATA'] == 1:
      expected_keyword_confidence += 0.125
    if counts['PROCEDURE'] == 1:
      expected_keyword_confidence += 0.125

    #  unknown tokens reduce confidence
    token_confidence = 1.0

    if len(self.tokens) > 0:
      token_confidence = num_known_tokens / len(self.tokens)

    #  unknown operators reduce confidence
    operator_confidence = 1.0
    num_operators = num_known_operators + num_invalid_operators

    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    self.confidences = {
      'token': token_confidence,
      'operator': operator_confidence
    }
