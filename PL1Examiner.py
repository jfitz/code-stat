import string
import math
from Token import Token
from Examiner import Examiner
from TokenBuilders import (
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
  SlashStarCommentTokenBuilder
)
from Tokenizer import Tokenizer

class PL1Examiner(Examiner):
  def __init__(self):
    super().__init__()

    self.whitespace_tb = WhitespaceTokenBuilder()
    self.newline_tb = NewlineTokenBuilder()

    self.integer_tb = IntegerTokenBuilder(False)
    self.integer_exponent_tb = IntegerExponentTokenBuilder()
    self.real_tb = RealTokenBuilder(False, False)
    self.real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    self.identifier_tb = IdentifierTokenBuilder()
    self.string_tb = StringTokenBuilder(['"', "'"], False, False)

    self.slash_star_comment_tb = SlashStarCommentTokenBuilder()

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

    self.line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation')
    self.preprocessor_tb = ListTokenBuilder(directives, 'preprocessor', True)
    self.terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

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
    self.group_ends = [')', ']', '}']

    self.groupers_tb = ListTokenBuilder(groupers, 'group', False)

    self.known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

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

    self.keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

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

    self.attributes_tb = ListTokenBuilder(attributes, 'attribute', True)

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

    self.function_tb = ListTokenBuilder(functions, 'function', True)

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

    self.format_item_tb = ListTokenBuilder(format_items, 'format', True)

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

    self.options_tb = ListTokenBuilder(options, 'option', True)

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

    self.conditions_tb = ListTokenBuilder(conditions, 'condition', True)

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

    self.subroutines_tb = ListTokenBuilder(subroutines, 'subroutine', True)

    types = [
      'FIXED', 'BINARY', 'FLOAT', 'DECIMAL',
      'BIT', 'CHARACTER', 'PICTURE'
    ]

    self.types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'SYSIN',
      'SYSPRINT'
    ]

    self.values_tb = ListTokenBuilder(values, 'value', True)
