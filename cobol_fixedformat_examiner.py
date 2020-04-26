import string

from codestat_exception import CodeStatException
from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StuffedQuoteStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  BlockTokenBuilder
)
from cobol_token_builders import (
  CobolIdentifierTokenBuilder,
  PictureTokenBuilder,
  CRPictureTokenBuilder
)
from cobol_examiner import CobolExaminer
from examiner import Examiner

class CobolFixedFormatExaminer(CobolExaminer):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StuffedQuoteStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    BlockTokenBuilder.__escape_z__()
    CobolIdentifierTokenBuilder.__escape_z__()
    PictureTokenBuilder.__escape_z__()
    CRPictureTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, year, extension, tab_size, wide):
    super().__init__()

    if year is not None and year not in ['68', '1968', '74', '1974', '85', '1985']:
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

    quotes = ['"', "'", "’"]
    string_tb = StuffedQuoteStringTokenBuilder(quotes, True)
    operand_types.append('string')

    picture_tb = PictureTokenBuilder()
    cr_picture_tb = CRPictureTokenBuilder()
    operand_types.append('picture')

    terminators_tb = SingleCharacterTokenBuilder('.', 'statement terminator', False)

    known_operators = [
      'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
      '+', '-', '*', '/', '**',
      '=', '<>', '>', '>=', '<', '<=',
      'AND', 'OR', 'NOT',
      ':'
    ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-'
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

    if year in ['68', '1968']:
      keywords += keywords_68_only

    if year in ['74', '1974', '85', '1985']:
      keywords += keywords_74

    if year in ['85', '1985']:
      keywords += keywords_85

    keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    values = [
      'BLANK', 'SPACE', 'SPACES', 'ZERO', 'ZEROES', 'ZEROS',
      'NO', 'OFF', 'ON'
    ]

    values_85 = ['FALSE', 'TRUE']

    if year in ['85', '1985']:
      values += values_85

    value_tb = CaseInsensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

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
      value_tb,
      identifier_tb,
      string_tb,
      exec_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    tokens = self.tokenize_code(code, tab_size, tokenizer, wide)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'whitespace')

    self.convert_numbers_to_pictures()
    self.convert_numbers_to_levels()

    expected_keyword_confidence = self.check_expected_keywords()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_operator_confidence()

    allow_pairs = []

    self.calc_operator_2_confidence(tokens, allow_pairs)
    # self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
    self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)
    self.calc_group_confidence(tokens, group_mids)
    # self.calc_operand_confidence(tokens, operand_types)
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


  def tokenize_line(self, line, tokenizer, wide):
    # break apart the line based on fixed format
    tokens = []

    # The COBOL line format is:
    # 1-6: line number or blank (ignored)
    # 7: space or one of *, /, D, d, $, -
    # 8-71: program text
    # 72-: identification, traditionally sequence number (ignored)

    line_indicator = line[6:7]
    if wide:
      line_text = line[7:]
      line_identification = ''
    else:
      line_text = line[7:71]
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
