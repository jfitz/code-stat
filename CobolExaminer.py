import string
from Token import Token
from Examiner import Examiner
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

class CobolExaminer(Examiner):
  def __init__(self, code, fixed_format, tab_size):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, True)
    real_exponent_tb = RealExponentTokenBuilder(False, True)
    identifier_tb = CobolIdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"])
    picture_tb = PictureTokenBuilder()
    star_comment_tb = StarCommentTokenBuilder()

    known_operators = [
      'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
      '+', '-', '*', '/', '**',
      '=', '<>', '>', '>=', '<', '<=',
      'AND', 'OR', 'NOT',
      '(', ')', ',', ':', '.'
      ]
    
    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

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

      'BASIS',
      'BEFORE',
      'BEGINNING',
      'BINARY',
      'BLANK',
      'BLOCK',
      'BOTTOM',
      'BY',

      'CALL',
      'CANCEL',
      'CBL',
      'CD',
      'CF',
      'CH',
      'CHARACTER',
      'CHARACTERS',
      'CLASS',
      'CLASS-ID',
      'CLOCK-UNITS',
      'CLOSE',
      'COBOL',
      'CODE',
      'CODE-SET',
      'COLLATING',
      'COLUMN',
      'COM-REG',
      'COMMA',
      'COMMON',
      'COMMUNICATION',
      'COMP',
      'COMP-1',
      'COMP-2',
      'COMP-3',
      'COMP-4',
      'COMP-5',
      'COMPUTATIONAL',
      'COMPUTATIONAL-1',
      'COMPUTATIONAL-2',
      'COMPUTATIONAL-3',
      'COMPUTATIONAL-4',
      'COMPUTATIONAL-5', 
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
      'DATE-COMPILED',
      'DATE-WRITTEN',
      'DAY',
      'DAY-OF-WEEK',
      'DBCS',
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
      'DISPLAY',
      'DISPLAY-1',
      'DIVIDE',
      'DIVISION',
      'DOWN',
      'DUPLICATES',
      'DYNAMIC',

      'EGCS',
      'EGI',
      'EJECT',
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
      'END-INVOKE',
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
      'ENDING',
      'ENTER',
      'ENTRY',
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
      'FUNCTION',

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
      'ID',
      'IDENTIFICATION',
      'IF',
      'IN',
      'INDEX',
      'INDEXED',
      'INDICATE',
      'INHERITS',
      'INITIAL',
      'INITIALIZE',
      'INITIATE',
      'INPUT',
      'INPUT-OUTPUT',
      'INSERT',
      'INSPECT',
      'INSTALLATION',
      'INTO',
      'INVALID',
      'INVOKE',
      'IS',

      'JUST',
      'JUSTIFIED',
      
      'KANJI',
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
      'LOCAL-STORAGE',
      'LOCK',
      'LOW-VALUE',
      'LOW-VALUES',

      'MEMORY',
      'MERGE',
      'MESSAGE',
      'METACLASS',
      'METHOD',
      'METHOD-ID',
      'MODE',
      'MODULES',
      'MORE-LABELS', 
      'MOVE',
      'MULTIPLE',
      'MULTIPLY',

      'NATIVE',
      'NATIVE_BINARY',
      'NEGATIVE',
      'NEXT',
      'NO',
      'NOT',
      'NULL',
      'NULLS',
      'NUMBER',
      'NUMERIC',
      'NUMERIC-EDITED',

      'OBJECT',
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
      'OVERRIDE',

      'PACKED-DECIMAL',
      'PADDING',
      'PAGE',
      'PAGE-COUNTER',
      'PASSWORD',
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
      'PROCEDURE-POINTER',
      'PROCEDURES',
      'PROCEED',
      'PROCESSING',
      'PROGRAM',
      'PROGRAM-ID',
      'PURGE',

      'QUEUE',
      'QUOTE',
      'QUOTES',

      'RANDOM',
      'RD',
      'READ',
      'READY',
      'RECEIVE',
      'RECORD',
      'RECORDING',
      'RECORDS',
      'RECURSIVE',
      'REDEFINES',
      'REEL',
      'REFERENCE',
      'REFERENCES',
      'RELATIVE',
      'RELEASE',
      'RELOAD',
      'REMAINDER',
      'REMOVAL',
      'RENAMES',
      'REPLACE',
      'REPLACING',
      'REPORT',
      'REPORTING',
      'REPORTS',
      'REPOSITORY',
      'RERUN',
      'RESERVE', 
      'RESET',
      'RETURN',
      'RETURN-CODE',
      'RETURNING',
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
      'SELF',
      'SEND',
      'SENTENCE',
      'SEPARATE',
      'SEQUENCE',
      'SEQUENTIAL',
      'SERVICE',
      'SET',
      'SHIFT-IN',
      'SHIFT-OUT',
      'SIGN',
      'SIZE',
      'SKIP1',
      'SKIP2',
      'SKIP3',
      'SORT',
      'SORT-CONTROL',
      'SORT-CORE-SIZE',
      'SORT-FILE-SIZE',
      'SORT-MERGE',
      'SORT-MESSAGE',
      'SORT-MODE-SIZE',
      'SORT-RETURN',
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
      'SUPER',
      'SUPPRESS',
      'SYMBOLIC',
      'SYNC',
      'SYNCHRONIZED',

      'TABLE',
      'TALLY',
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
      'TRACE',
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
      'WHEN-COMPILED',
      'WITH',
      'WORDS',
      'WORKING-STORAGE',
      'WRITE',
      'WRITE-ONLY',

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
      self.unknown_operator_tb,
      identifier_tb,
      string_tb,
      star_comment_tb,
      cobol_preprocessor_tb
    ]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    lines = code.split('\n')

    self.tokens = []
    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = self.tabs_to_spaces(line, tab_size)

      line_number = line[:6]
      line_indicator = ''
      line_text = ''
      line_identification = ''

      # break apart the line based on fixed format or free format style
      if fixed_format:
        # The fixed-format COBOL line format is:
        # 1-6: line number or blank (ignored)
        # 7: space or *
        # 8-71: program text
        # 72-: identification, traditionally sequence number (ignored)

        if len(line_number) > 0:
          if line_number.isspace():
            self.tokens.append(Token(line_number, 'whitespace'))
          else:
            if line_number.isdigit():
              self.tokens.append(Token(line_number, 'line number'))
            else:
              self.tokens.append(Token(line_number, 'line identification'))

        if len(line) > 6:
          line_indicator = line[6]

        if len(line) > 7:
          line_text = line[7:71]

        if len(line) > 72:
          line_identification = line[72:]
      else:
        # in free-format COBOL, the entire line can contain tokens
        line_text = line

      # tokenize the line indicator
      if fixed_format:
        if line_indicator in ['*', '/', 'D']:
          # the entire line is a comment (including DEBUG lines)
          self.tokens.append(Token(line[6:], 'comment'))
        else:
          if line_indicator == '$':
            self.tokens.append(Token(line[6:], 'preprocessor'))
          else:
            if line_indicator == ' ':
              self.tokens.append(Token(' ', 'whitespace'))
            else:
              if line_indicator != '':
                self.tokens.append(Token(line_indicator, 'invalid'))

          # tokenize the code    
          self.tokens += tokenizer.tokenize(line_text)
      else:
        # tokenize the code    
        self.tokens += tokenizer.tokenize(line_text)

      # tokenize the line identification
      if fixed_format:
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
      'expected_keyword': expected_keyword_confidence,
      'operator': operator_confidence
    }
