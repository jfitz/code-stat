import string
from Examiner import Examiner
from TokenBuilders import *
from CobolTokenBuilders import *
from Tokenizer import Tokenizer

class CobolExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    lines = code.split('\n')

    num_tokens = 0
    num_known_tokens = 0
    num_operators = 0
    num_known_operators = 0

    wtb = WhitespaceTokenBuilder()

    ntb = NumberTokenBuilder()
    itb = IdentifierTokenBuilder()
    stb = StringTokenBuilder()
    ptb = PictureTokenBuilder()

    known_operators = [
      'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
      '=', '<>', '>', '>=', '<', '<=',
      'AND', 'OR', 'NOT',
      '(', ')', ',', ':', '.'
      ]
    
    kotb = ListTokenBuilder(known_operators, 'operator', True)

    unknown_operators = set(self.common_operators()) - set(known_operators)
    uotb = ListTokenBuilder(unknown_operators, 'invalid operator', True)

    nltb = NewlineTokenBuilder()

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

    ktb = ListTokenBuilder(keywords, 'keyword', True)
    
    tokenbuilders = [wtb, nltb, ptb, ntb, ktb, kotb, uotb, itb, stb]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    found_keywords = set()
    
    # The official COBOL line format is:
    # 1-6: line number or blank (ignored)
    # 7: space or *
    # 8-71: program text
    # 72-: identification, traditionally sequence number (ignored)
    #
    
    self.tokens = []
    for line in lines:
      line = line.rstrip()

      line_number = line[:5]
      line_indicator = ''
      if len(line) > 6:
        line_indicator = line[6]
      line_text = ''
      if len(line) > 7:
        line_text = line[7:71]
      line_identification = ''
      if len(line) > 72:
        line_identification = line[72:]

      if len(line_number) > 0:
        if line_number.isspace():
          self.tokens.append(Token(line_number, 'whitespace'))
        else:
          self.tokens.append(Token(line_number, 'number'))

      if len(line_identification) > 0:
        self.tokens.append(Token(line_identification, 'string'))

      if line_indicator == '*':
        self.tokens.append(Token(line[6:], 'comment'))
        self.tokens.append(Token('\n', 'newline'))
      else:
        self.tokens.append(Token(' ', 'whitespace')) # the indicator column
        tokens = tokenizer.tokenize(line_text)
        tokens.append(Token('\n', 'newline'))
        self.tokens += tokens
        
        for token in tokens:
          num_tokens += 1
          if not token.group.startswith('invalid'):
            num_known_tokens += 1

          # count operators
          if token.group == 'operator' or token.group == 'invalid operator':
            num_operators += 1
          if token.group == 'operator':
            num_known_operators += 1

          # collect keywords for counting
          if token.group == 'keyword':
            found_keywords.add(str(token))

    # count unique keywords and compare to number of tokens
    keyword_confidence = 0
    num_keywords = len(found_keywords)
    if num_keywords > 10:
      keyword_confidence = 1

    #  unknown tokens reduce confidence
    token_confidence = 1
    if num_tokens > 0:
      token_confidence = num_known_tokens / num_tokens

    #  unknown operators reduce confidence
    operator_confidence = 1
    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    # compute confidence
    self.confidence = keyword_confidence * token_confidence * operator_confidence
    self.confidences = {
      'keyword': keyword_confidence,
      'token': token_confidence,
      'operator': operator_confidence
    }