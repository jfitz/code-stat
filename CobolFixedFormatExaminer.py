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
  CRPictureTokenBuilder
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
    group_ends = [')']

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

    if year in ['74', '85']:
      keywords += keywords_74

    if year in ['85']:
      keywords += keywords_85

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

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
      exec_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    self.tokens = self.tokenize_code(code, tab_size, tokenizer, wide)
    self.tokens = self.combine_adjacent_whitespace(self.tokens)

    self.convert_numbers_to_pictures()
    self.convert_numbers_to_levels()

    expected_keyword_confidence = self.check_expected_keywords()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'symbol']
    # self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_picture_confidence()
    if not wide:
      self.calc_line_length_confidence(code, 80)
    self.confidences['expected_keywords'] = expected_keyword_confidence
    self.calc_statistics()


  def tokenize_line_number(self, line_number):
    token = None

    if len(line_number) > 0:
      if line_number.isspace():
        token = Token(line_number, 'whitespace')
      else:
        if line_number.isdigit():
          token = Token(line_number, 'line number')
        else:
          token = Token(line_number, 'line identification')

    return token


  def tokenize_alt_line(self, line, line_indicator):
    token = None

    if line_indicator in ['*', '/', 'D', 'd']:
      # the entire line is a comment (including DEBUG lines)
      token = Token(line[6:], 'comment')
    if line_indicator == '$':
      token = Token(line[6:], 'preprocessor')

    return token


  def tokenize_line_indicator(self, line_indicator):
    token = None

    if line_indicator == ' ':
      token = Token(' ', 'whitespace')
    else:
      if line_indicator == '-':
        token = Token(line_indicator, 'continuation line')
      else:
        if line_indicator != '':
          token = Token(line_indicator, 'invalid')

    return token


  def tokenize_line(self, line, tokenizer, wide):
    # break apart the line based on fixed format
    tokens = []

    # The COBOL line format is:
    # 1-6: line number or blank (ignored)
    # 7: space or one of *, /, D, d, $, -
    # 8-71: program text
    # 72-: identification, traditionally sequence number (ignored)

    line_number = line[:6]
    line_indicator = line[6:7]
    if wide:
      line_text = line[7:]
      line_identification = ''
    else:
      line_text = line[7:71]
      line_identification = line[72:]

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
      tokens.append(Token(line_identification, 'line identification'))

    tokens.append(Token('\n', 'newline'))

    return tokens


  def tokenize_code(self, code, tab_size, tokenizer, wide):
    lines = code.split('\n')

    tokens = []

    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = self.tabs_to_spaces(line, tab_size)

      line_tokens = self.tokenize_line(line, tokenizer, wide)
      tokens += line_tokens

    return tokens


def unwrap_cobol_lines(lines):
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
