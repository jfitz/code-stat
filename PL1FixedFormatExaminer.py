import string
import math
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
  IdentifierTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder
)
from CXTokenBuilders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder,
  ClassTypeTokenBuilder
)
from Tokenizer import Tokenizer

class PL1FixedFormatExaminer(Examiner):
  def __init__(self, code, tab_size, wide):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False, False)

    class_type_tb = ClassTypeTokenBuilder()

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    directives = (
      '%ACTIVATE',
      '%DEACTIVATE',
      '%DECLARE', '%DCL',
      '%DICTIONARY',
      '%DO',
      '%ELSE',
      '%END',
      '%ERROR',
      '%FATAL',
      '%GOTO',
      '%IF',
      '%INCLUDE',
      '%INFORM',
      '%LIST',
      '%NOLIST',
      '%PAGE',
      '%PROCEDURE', '%PROC',
      '%REPLACE',
      '%RETURN',
      '%SBTTL',
      '%THEN',
      '%TITLE',
      '%WARN'
    )

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation')
    preprocessor_tb = ListTokenBuilder(directives, 'preprocessor', True)
    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

    known_operators = [
      '+', '-', '*', '/', '**',
      '>', '<', '=', '>=', '<=',
      '^>', '^<', '^=', '^',
      '~>', '~<', '~=', '~',
      '&', '&:',
      '|', '|:', '||',
      '!', '!:', '!!',
      ':'
    ]

    self.unary_operators = [
      '+', '-', '^', '~'
    ]

    self.postfix_operators = [
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'ALLOCATE', 'ALLOC',
      'BEGIN',
      'CALL',
      'CLOSE',
      'DECLARE', 'DCL',
      'DO',
      'ELSE',
      'END',
      'FORMAT',
      'FREE',
      'GET',
      'GOTO', 'GO TO',
      'IF',
      'LEAVE',
      'ON',
      'OPEN',
      'OTHERWISE', 'OTHER',
      'PROCEDURE', 'PROC',
      'PUT',
      'READ',
      'RETURN',
      'REVERT',
      'REWRITE',
      'SELECT',
      'SIGNAL',
      'STOP',
      'THEN',
      'WHEN',
      'WRITE'
     ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    attributes = [
      'ALIGNED',
      'ANY',
      'AREA',
      'BASED',
      'BUILTIN',
      'CONDITION', 'COND',
      'CONTROLLED', 'CTL',
      'DEFINED', 'DEF',
      'DIRECT',
      'ENTRY',
      'ENVIRONMENT', 'ENV',
      'EXTERNAL', 'EXT',
      'FILE',
      'GLOBALDEF',
      'GLOBALREF',
      'INITIAL', 'INIT',
      'INPUT',
      'INTERNAL', 'INT'
      'KEYED',
      'LABEL',
      'LIKE',
      'LIST',
      'MEMBER',
      'NONVARYING', 'NONVAR',
      'OPTIONAL',
      'OPTIONS',
      'OUTPUT',
      'PARAMETER', 'PARM',
      'PICTURE', 'PIC',
      'POSITION', 'POS',
      'PRECISION', 'PREC',
      'PRINT',
      'READONLY',
      'RECORD',
      'REFER',
      'RETURNS',
      'SEQUENTIAL', 'SEQL',
      'STATIC',
      'STREAM',
      'STRUCTURE',
      'TRUNCATE',
      'UNALIGNED', 'UNAL',
      'UNION',
      'UPDATE',
      'VARIABLE',
      'VARYING', 'VAR'
    ]

    attributes_tb = ListTokenBuilder(attributes, 'attribute', True)

    functions = [
      'ABS',
      'ACOS',
      'ACTUALCOUNT',
      'ADD',
      'ADDR',
      'ADDREL',
      'ALLOCATION', 'ALLOCN',
      'ASIN',
      'ATAN',
      'ATAND',
      'ATANH',
      'AUTOMATIC', 'AUTO',
      'BINARY', 'BIN',
      'BIT',
      'BOOL',
      'BYTE',
      'BYTESIZE',
      'CEIL',
      'CHARACTER', 'CHAR',
      'COLLATE',
      'COPY',
      'COS',
      'COSD',
      'COSH',
      'DATE',
      'DATETIME',
      'DECIMAL', 'DEC',
      'DECODE',
      'DESCRIPTOR', 'DESC',
      'DIMENSION', 'DIM',
      'DIVIDE',
      'EMPTY',
      'ENCODE',
      'ERROR',
      'EVERY',
      'EXP',
      'FIXED',
      'FLOAT',
      'FLOOR',
      'HBOUND',
      'HIGH',
      'INDEX',
      'INFORM',
      'INT',
      'LBOUND',
      'LENGTH',
      'LINE',
      'LINENO',
      'LOG',
      'LOG10',
      'LOG2',
      'LOW',
      'LTRIM',
      'MAX',
      'MAXLENGTH',
      'MIN',
      'MOD',
      'MULTIPLY',
      'NULL',
      'OFFSET',
      'ONARGSLIST',
      'ONCHAR',
      'ONCODE',
      'ONFILE',
      'ONKEY',
      'ONSOURCE',
      'PAGENO',
      'POINTER', 'PTR',
      'POSINT',
      'PRESENT',
      'PROD',
      'RANK',
      'REFERENCE',
      'REVERSE',
      'ROUND',
      'RTRIM',
      'SEARCH',
      'SIGN',
      'SIN',
      'SIND',
      'SINH',
      'SIZE',
      'SOME',
      'SQRT',
      'STRING',
      'SUBSTR',
      'SUBTRACT',
      'SUM',
      'TAN',
      'TAND',
      'TANH',
      'TIME',
      'TRANSLATE',
      'TRIM',
      'TRUNC',
      'UNSPEC',
      'VALID',
      'VALUE', 'VAL',
      'VARIANT',
      'VERIFY',
      'WARN'
    ]

    function_tb = ListTokenBuilder(functions, 'function', True)

    format_items = [
      'A',
      'B', 'B1', 'B2', 'B3', 'B4',
      'COLUMN', 'COL',
      'E',
      'F',
      'P',
      'R',
      'TAB',
      'X'
    ]

    format_item_tb = ListTokenBuilder(format_items, 'format', True)

    options = [
      'APPEND',
      'BACKUP_DATE',
      'BATCH',
      'BLOCK_BOUNDARY_FORMAT',
      'BLOCK_IO',
      'BLOCK_SIZE',
      'BUCKET_SIZE',
      'BY',
      'CANCEL_CONTROL_O',
      'CARRIAGE_RETURN_FORMAT',
      'CONTIGUOUS',
      'CONTIGUOUS_BEST_TRY',
      'CREATION_DATE',
      'CURRENT_POSITION',
      'DEFAULT_FILE_NAME',
      'DEFERRED_WRITE',
      'DELETE',
      'EDIT',
      'EXPIRATION_DATE',
      'EXTENSION_SIZE',
      'FAST_DELETE',
      'FILE_ID',
      'FILE_ID_TO',
      'FILE_SIZE',
      'FIXED_CONTROL_FROM',
      'FIXED_CONTROL_SIZE',
      'FIXED_CONTROL_SIZE_TO',
      'FIXED_CONTROL_TO',
      'FIXED_LENGTH_RECORDS',
      'FROM',
      'GROUP_PROTECTION',
      'IDENT',
      'IGNORE_LINE_MARKS',
      'IN',
      'INDEXED',
      'INDEX_NUMBER',
      'INITIAL_FILL',
      'INTO',
      'KEY',
      'KEYFROM',
      'KEYTO',
      'LINESIZE',
      'LOCK_ON_READ',
      'LOCK_ON_WRITE',
      'MAIN PROCEDURE',
      'MANUAL_UNLOCKING',
      'MATCH_GREATER',
      'MATCH_GREATER_EQUAL',
      'MATCH_NEXT',
      'MATCH_NEXT_EQUAL',
      'MAXIMUM_RECORD_NUMBER',
      'MAXIMUM_RECORD_SIZE',
      'MULTIBLOCK_COUNT',
      'MULTIBUFFER_COUNT',
      'NOLOCK',
      'NONEXISTENT_RECORD',
      'NONRECURSIVE',
      'NORESCAN',
      'NO_ECHO',
      'NO_FILTER',
      'NO_SHARE',
      'OWNER_GROUP',
      'OWNER_ID',
      'OWNER_MEMBER',
      'OWNER_PROTECTION',
      'PAGE',
      'PAGESIZE',
      'PRINTER_FORMAT',
      'PROMPT',
      'PURGE_TYPE_AHEAD',
      'READ_AHEAD',
      'READ_CHECK',
      'READ_REGARDLESS',
      'RECORD_ID',
      'RECORD_ID_ACCESS',
      'RECORD_ID_TO',
      'RECURSIVE',
      'REPEAT',
      'RESCAN',
      'RETRIEVAL_POINTERS',
      'REVISION_DATE',
      'REWIND_ON_CLOSE',
      'REWIND_ON_OPEN',
      'SCALARVARYING',
      'SET READ',
      'SHARED_READ',
      'SHARED_WRITE',
      'SKIP',
      'SNAP',
      'SPOOL',
      'STATEMENT',
      'SUPERSEDE',
      'SYSTEM',
      'SYSTEM_PROTECTION',
      'TEMPORARY',
      'TIMEOUT_PERIOD',
      'TITLE',
      'TO',
      'UNDERFLOW', 'UFL',
      'UNTIL',
      'USER_OPEN',
      'WAIT_FOR_RECORD',
      'WHILE',
      'WORLD_PROTECTION',
      'WRITE_BEHIND',
      'WRITE_CHECK'
    ]

    options_tb = ListTokenBuilder(options, 'option', True)

    conditions = [
      'ANYCONDITION',
      'CONVERSION', 'CONV',
      'ENDFILE',
      'ENDPAGE',
      'FINISH',
      'FIXEDOVERFLOW', 'FOFL',
      'OVERFLOW', 'OFL',
      'STORAGE',
      'STRINGRANGE', 'STRG',
      'SUBSCRIPTRANGE', 'SUBRG',
      'UNDEFINEDFILE', 'UNDF',
      'VAXCONDITION',
      'ZERODIVIDE', 'ZDIV'
    ]

    conditions_tb = ListTokenBuilder(conditions, 'condition', True)

    subroutines = [
      'DISPLAY',
      'EXTEND',
      'FLUSH',
      'NEXT_VOLUME',
      'RELEASE',
      'RESIGNAL',
      'REWIND',
      'SPACEBLOCK'
    ]

    subroutines_tb = ListTokenBuilder(subroutines, 'subroutine', True)

    types = [
      'FIXED', 'BINARY', 'FLOAT', 'DECIMAL',
      'BIT', 'CHARACTER', 'PICTURE'
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'SYSIN',
      'SYSPRINT'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      function_tb,
      attributes_tb,
      options_tb,
      conditions_tb,
      subroutines_tb,
      format_item_tb,
      types_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      class_type_tb,
      string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      preprocessor_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    self.tokens = self.tokenize_code(code, tab_size, tokenizer, wide)
    self.tokens = self.combine_adjacent_whitespace(self.tokens)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()


  def tokenize_line(self, line, tokenizer, wide):
    # break apart the line based on fixed format
    tokens = []

    # The fixed-format FORTRAN line format is:
    # 1: space or C or *
    # 2-6: line number or blank
    # 7: continuation character
    # 8-72: program text
    # 73-: identification, traditionally sequence number (ignored)

    line_indicator = line[0:1]
    if wide:
      line_text = line[1:]
      line_identification = ''
    else:
      line_text = line[1:72]
      line_identification = line[72:]

    # tokenize the line indicator
    if line_indicator in ['C', '*']:
      tokens.append(Token(line, 'comment'))
    else:
      if len(line_indicator) > 0 and line_indicator != ' ':
        tokens.append(Token(line, 'invalid'))
      else:
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
