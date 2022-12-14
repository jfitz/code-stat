import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  EscapedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  SuffixedIntegerTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  SuffixedRealTokenBuilder,
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadToEndOfLineTokenBuilder,
  InvalidTokenBuilder
)
from cx_token_builders import (
  SlashStarCommentTokenBuilder
)
from pl1_token_builders import (
  PL1CommentStartTokenBuilder,
  PL1CommentMiddleTokenBuilder,
  PL1CommentEndTokenBuilder,
  PL1LabelTokenBuilder
)
from jcl_token_builders import (
  JCLTokenBuilder
)
from examiner import Examiner

class PL1Examiner(Examiner):
  @staticmethod
  def __escape_z__():
    PL1CommentStartTokenBuilder.__escape_z__()
    PL1CommentMiddleTokenBuilder.__escape_z__()
    PL1CommentEndTokenBuilder.__escape_z__()
    PL1LabelTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, tab_size):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    binary_integer_tb = SuffixedIntegerTokenBuilder(['B'], False, None)
    real_tb = RealTokenBuilder(False, False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    binary_real_tb = SuffixedRealTokenBuilder(True, True, ['B'], False, None)
    operand_types.append('number')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    quotes = ['"', "'"]
    string_tb = EscapedStringTokenBuilder(quotes, 0)
    operand_types.append('string')

    label_tb = PL1LabelTokenBuilder()
    operand_types.append('label')

    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    jcl_tb = JCLTokenBuilder()

    directives = [
      '%ACTIVATE',
      '%DEACTIVATE', '%DECLARE', '%DCL', '%DICTIONARY', '%DO',
      '%ELSE', '%END',
      '%FATAL',
      '%GOTO',
      '%IF', '%INCLUDE',
      '%LIST',
      '%NOLIST',
      '%PAGE', '%PROCEDURE', '%PROC',
      '%REPLACE', '%RETURN',
      '%THEN'
    ]

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation', False)
    preprocessor_tb = CaseInsensitiveListTokenBuilder(directives, 'preprocessor', False)
    title_tb = LeadToEndOfLineTokenBuilder('%TITLE', True, 'preprocessor')
    subtitle_tb = LeadToEndOfLineTokenBuilder('%SBTTL', True, 'preprocessor')
    error_tb = LeadToEndOfLineTokenBuilder('%ERROR', True, 'preprocessor')
    warn_tb = LeadToEndOfLineTokenBuilder('%WARN', True, 'preprocessor')
    inform_tb = LeadToEndOfLineTokenBuilder('%INFORM', True, 'preprocessor')
    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '/', '**',
      '>', '<', '=', '>=', '<=',
      '¬>', '¬<', '¬=',
      '^>', '^<', '^=', '^',
      '~>', '~<', '~=', '~', '¬',
      '&', '&:',
      '|', '|:', '||',
      '!', '!:', '!!',
      ':'
    ]

    self.unary_operators = [
      '+', '-', '^', '~', '¬'
    ]

    self.postfix_operators = [
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_starts = ['(', '[', ',', '{']
    group_mids = [',']
    group_ends = [')', ']', '}']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'ALLOCATE', 'ALLOC',
      'BEGIN',
      'CALL', 'CLOSE',
      'DECLARE', 'DCL', 'DO',
      'ELSE', 'END',
      'FORMAT', 'FREE',
      'GET', 'GOTO', 'GO TO',
      'IF',
      'LEAVE',
      'ON', 'OPEN', 'OTHERWISE', 'OTHER',
      'PROCEDURE', 'PROC', 'PUT',
      'READ', 'RETURN', 'REVERT', 'REWRITE',
      'SELECT', 'SIGNAL', 'STOP',
      'THEN',
      'WHEN', 'WRITE'
     ]

    keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    attributes = [
      'ALIGNED', 'ANY', 'AREA',
      'BASED', 'BUILTIN',
      'CONDITION', 'COND', 'CONTROLLED', 'CTL',
      'DEFINED', 'DEF', 'DIRECT',
      'ENTRY', 'ENVIRONMENT', 'ENV', 'EXTERNAL', 'EXT',
      'FILE',
      'GLOBALDEF', 'GLOBALREF',
      'INITIAL', 'INIT', 'INPUT', 'INTERNAL', 'INT'
      'KEYED',
      'LABEL', 'LIKE', 'LIST',
      'MEMBER',
      'NONVARYING', 'NONVAR',
      'OPTIONAL', 'OPTIONS', 'OUTPUT',
      'PARAMETER', 'PARM', 'PICTURE', 'PIC', 'POSITION', 'POS', 'PRECISION', 'PREC',
      'PRINT',
      'READONLY', 'RECORD', 'REFER', 'RETURNS',
      'SEQUENTIAL', 'SEQL', 'STATIC', 'STREAM', 'STRUCTURE',
      'TRUNCATE',
      'UNALIGNED', 'UNAL', 'UNION', 'UPDATE',
      'VARIABLE', 'VARYING', 'VAR'
    ]

    attributes_tb = CaseInsensitiveListTokenBuilder(attributes, 'attribute', False)

    functions = [
      'ABS', 'ACOS', 'ACTUALCOUNT', 'ADD', 'ADDR', 'ADDREL', 'ALLOCATION', 'ALLOCN',
      'ASIN', 'ATAN', 'ATAND', 'ATANH', 'AUTOMATIC', 'AUTO',
      'BINARY', 'BIN', 'BIT', 'BOOL', 'BYTE', 'BYTESIZE',
      'CEIL', 'CHARACTER', 'CHAR', 'COLLATE', 'COPY', 'COS', 'COSD', 'COSH',
      'DATE', 'DATETIME', 'DECIMAL', 'DEC', 'DECODE', 'DESCRIPTOR', 'DESC',
      'DIMENSION', 'DIM', 'DIVIDE',
      'EMPTY', 'ENCODE', 'ERROR', 'EVERY', 'EXP',
      'FIXED', 'FLOAT', 'FLOOR',
      'HBOUND', 'HIGH',
      'INDEX', 'INFORM', 'INT',
      'LBOUND', 'LENGTH', 'LINE', 'LINENO', 'LOG', 'LOG10', 'LOG2', 'LOW', 'LTRIM',
      'MAX', 'MAXLENGTH', 'MIN', 'MOD', 'MULTIPLY',
      'NULL',
      'OFFSET', 'ONARGSLIST', 'ONCHAR', 'ONCODE', 'ONFILE', 'ONKEY', 'ONSOURCE',
      'PAGENO','POINTER', 'PTR', 'POSINT', 'PRESENT', 'PROD',
      'RANK', 'REFERENCE', 'REVERSE', 'ROUND', 'RTRIM',
      'SEARCH', 'SIGN', 'SIN', 'SIND', 'SINH', 'SIZE', 'SOME', 'SQRT', 'STRING',
      'SUBSTR', 'SUBTRACT', 'SUM',
      'TAN', 'TAND', 'TANH', 'TIME', 'TRANSLATE', 'TRIM', 'TRUNC',
      'UNSPEC',
      'VALID', 'VALUE', 'VAL', 'VARIANT', 'VERIFY',
      'WARN'
    ]

    function_tb = CaseInsensitiveListTokenBuilder(functions, 'function', True)

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

    format_item_tb = CaseSensitiveListTokenBuilder(format_items, 'format', True)
    operand_types.append('format')

    options = [
      'APPEND',
      'BACKUP_DATE', 'BATCH', 'BLOCK_BOUNDARY_FORMAT', 'BLOCK_IO', 'BLOCK_SIZE',
      'BUCKET_SIZE', 'BY',
      'CANCEL_CONTROL_O', 'CARRIAGE_RETURN_FORMAT', 'CONTIGUOUS',
      'CONTIGUOUS_BEST_TRY', 'CREATION_DATE', 'CURRENT_POSITION',
      'DEFAULT_FILE_NAME', 'DEFERRED_WRITE', 'DELETE',
      'EDIT', 'EXPIRATION_DATE', 'EXTENSION_SIZE',
      'FAST_DELETE', 'FILE_ID', 'FILE_ID_TO', 'FILE_SIZE', 'FIXED_CONTROL_FROM',
      'FIXED_CONTROL_SIZE', 'FIXED_CONTROL_SIZE_TO', 'FIXED_CONTROL_TO',
      'FIXED_LENGTH_RECORDS', 'FROM',
      'GROUP_PROTECTION',
      'IDENT', 'IGNORE_LINE_MARKS', 'IN', 'INDEXED', 'INDEX_NUMBER', 'INITIAL_FILL',
      'INTO',
      'KEY', 'KEYFROM', 'KEYTO',
      'LINESIZE', 'LOCK_ON_READ', 'LOCK_ON_WRITE',
      'MAIN PROCEDURE', 'MANUAL_UNLOCKING', 'MATCH_GREATER', 'MATCH_GREATER_EQUAL',
      'MATCH_NEXT', 'MATCH_NEXT_EQUAL', 'MAXIMUM_RECORD_NUMBER', 'MAXIMUM_RECORD_SIZE',
      'MULTIBLOCK_COUNT', 'MULTIBUFFER_COUNT',
      'NOLOCK', 'NONEXISTENT_RECORD', 'NONRECURSIVE', 'NORESCAN', 'NO_ECHO',
      'NO_FILTER', 'NO_SHARE',
      'OWNER_GROUP', 'OWNER_ID', 'OWNER_MEMBER', 'OWNER_PROTECTION',
      'PAGE', 'PAGESIZE', 'PRINTER_FORMAT', 'PROMPT', 'PURGE_TYPE_AHEAD',
      'READ_AHEAD', 'READ_CHECK', 'READ_REGARDLESS', 'RECORD_ID',
      'RECORD_ID_ACCESS', 'RECORD_ID_TO', 'RECURSIVE', 'REPEAT', 'RESCAN',
      'RETRIEVAL_POINTERS', 'REVISION_DATE', 'REWIND_ON_CLOSE', 'REWIND_ON_OPEN',
      'SCALARVARYING', 'SET READ', 'SHARED_READ', 'SHARED_WRITE', 'SKIP',
      'SNAP', 'SPOOL', 'STATEMENT', 'SUPERSEDE', 'SYSTEM', 'SYSTEM_PROTECTION',
      'TEMPORARY', 'TIMEOUT_PERIOD', 'TITLE', 'TO',
      'UNDERFLOW', 'UFL', 'UNTIL', 'USER_OPEN',
      'WAIT_FOR_RECORD', 'WHILE', 'WORLD_PROTECTION', 'WRITE_BEHIND', 'WRITE_CHECK'
    ]

    options_tb = CaseInsensitiveListTokenBuilder(options, 'option', False)

    conditions = [
      'ANYCONDITION',
      'CONVERSION', 'CONV',
      'ENDFILE','ENDPAGE',
      'FINISH', 'FIXEDOVERFLOW', 'FOFL',
      'OVERFLOW', 'OFL',
      'STORAGE', 'STRINGRANGE', 'STRG', 'SUBSCRIPTRANGE', 'SUBRG',
      'UNDEFINEDFILE', 'UNDF',
      'VAXCONDITION',
      'ZERODIVIDE', 'ZDIV'
    ]

    conditions_tb = CaseInsensitiveListTokenBuilder(conditions, 'condition', False)

    subroutines = [
      'DISPLAY',
      'EXTEND',
      'FLUSH',
      'NEXT_VOLUME',
      'RELEASE', 'RESIGNAL', 'REWIND',
      'SPACEBLOCK'
    ]

    subroutines_tb = CaseInsensitiveListTokenBuilder(subroutines, 'subroutine', False)

    types = [
      'FIXED', 'BINARY', 'FLOAT', 'DECIMAL',
      'BIT', 'CHARACTER', 'PICTURE'
    ]

    types_tb = CaseInsensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    values = [
      'SYSIN', 'SYSPRINT'
    ]

    values_tb = CaseInsensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    invalid_token_builder = InvalidTokenBuilder()

    # tokenize as free-format
    tokenbuilders_free = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      binary_integer_tb,
      real_tb,
      real_exponent_tb,
      binary_real_tb,
      keyword_tb,
      function_tb,
      attributes_tb,
      options_tb,
      conditions_tb,
      subroutines_tb,
      types_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      string_tb,
      label_tb,
      slash_star_comment_tb,
      preprocessor_tb,
      title_tb,
      subtitle_tb,
      error_tb,
      warn_tb,
      inform_tb,
      jcl_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer_free = Tokenizer(tokenbuilders_free)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens_free = tokenizer_free.tokenize(ascii_code)

    tokens_free = Examiner.combine_adjacent_identical_tokens(tokens_free, 'invalid operator')
    tokens_free = Examiner.combine_adjacent_identical_tokens(tokens_free, 'invalid')

    self.tokens = tokens_free

    self.calc_statistics()

    statistics_free = self.statistics
    self.statistics = {}

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'symbol']
    # self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 2)

    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_length_confidence(code, self.max_expected_line)

    confidences_free = self.confidences
    self.confidences = {}
    errors_free = self.errors
    self.errors = []

    # tokenize as fixed-format
    tokenbuilders_fixed = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      binary_integer_tb,
      real_tb,
      real_exponent_tb,
      binary_real_tb,
      keyword_tb,
      function_tb,
      attributes_tb,
      options_tb,
      conditions_tb,
      subroutines_tb,
      types_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      string_tb,
      label_tb,
      slash_star_comment_tb,
      preprocessor_tb,
      title_tb,
      subtitle_tb,
      error_tb,
      warn_tb,
      inform_tb,
      jcl_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    comment_start_tb = PL1CommentStartTokenBuilder()
    comment_middle_tb = PL1CommentMiddleTokenBuilder()
    comment_end_tb = PL1CommentEndTokenBuilder()

    type1_tokenbuilders = [comment_start_tb]
    tokenbuilders_fixed_1 = tokenbuilders_fixed + type1_tokenbuilders + [invalid_token_builder]
    tokenizer_fixed_1 = Tokenizer(tokenbuilders_fixed_1)

    type2_tokenbuilders = [comment_start_tb, comment_middle_tb, comment_end_tb]
    tokenbuilders_fixed_2 = tokenbuilders_fixed + type2_tokenbuilders + [invalid_token_builder]
    tokenizer_fixed_2 = Tokenizer(tokenbuilders_fixed_2)

    tokens_fixed = self.tokenize_code(ascii_code, tab_size, tokenizer_fixed_1, tokenizer_fixed_2)

    tokens_fixed = Examiner.combine_adjacent_identical_tokens(tokens_fixed, 'invalid operator')
    tokens_fixed = Examiner.combine_adjacent_identical_tokens(tokens_fixed, 'invalid')
    tokens_fixed = Examiner.combine_adjacent_identical_tokens(tokens_fixed, 'whitespace')
    tokens_fixed = self.convert_broken_comments_to_comments(tokens_fixed)

    self.tokens = tokens_fixed

    self.calc_statistics()

    statistics_fixed = self.statistics
    self.statistics = {}

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'symbol']
    # self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 2)

    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_length_confidence(code, self.max_expected_line)
    # self.calc_line_description_confidence()

    confidences_fixed = self.confidences
    self.confidences = {}
    errors_fixed = self.errors
    self.errors = []

    # compute confidence for free-format and fixed-format
    confidence_free = 1.0
    if len(confidences_free) == 0:
      confidence_free = 0.0
    else:
      for key in confidences_free:
        factor = confidences_free[key]
        confidence_free *= factor

    confidence_fixed = 1.0
    if len(confidences_fixed) == 0:
      confidence_fixed = 0.0
    else:
      for key in confidences_fixed:
        factor = confidences_fixed[key]
        confidence_fixed *= factor

    # select the better of free-format and spaced-format
    if confidence_fixed > confidence_free:
      self.tokens = tokens_fixed
      self.statistics = statistics_fixed
      self.confidences = confidences_fixed
      self.errors = errors_fixed
    else:
      self.tokens = tokens_free
      self.statistics = statistics_free
      self.confidences = confidences_free
      self.errors = errors_free


  def tokenize_line(self, line, tokenizer):
    # break apart the line based on fixed format
    tokens = []

    # The fixed-format PL/1 line format is:
    # 1: space or C or *
    # 2-72: program text
    # 73-: identification, traditionally sequence number (ignored)
    # but if columns 1 and 2 are '//', then the line is JCL

    if line.startswith(('//', '/*')):
      tokens.append(Token(line, 'jcl', False))
    else:
      line_indicator = line[0:1]
      line_text = line[1:72]
      line_identification = line[72:]

      # tokenize the line indicator
      if line_indicator in ['C', '*']:
        tokens.append(Token(line, 'comment', False))
      else:
        if len(line_indicator) > 0 and line_indicator != ' ':
          tokens.append(Token(line_indicator, 'invalid', False))
        else:
          tokens.append(Token(' ', 'whitespace', False))
        # tokenize the code
        tokens += tokenizer.tokenize(line_text)

        # tokenize the line identification
        if len(line_identification) > 0:
          tokens.append(Token(line_identification, 'line identification', False))

    tokens.append(Token('\n', 'newline', False))

    return tokens


  def tokenize_code(self, code, tab_size, tokenizer1, tokenizer2):
    lines = code.split('\n')

    tokens = []

    mode = 1

    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = Examiner.tabs_to_spaces(line, tab_size)

      if mode == 1:
        line_tokens = self.tokenize_line(line, tokenizer1)
      else:
        line_tokens = self.tokenize_line(line, tokenizer2)

      for token in line_tokens:
        if token.group == 'comment-end':
          mode = 1
        if token.group == 'comment-start':
          mode = 2

      tokens += line_tokens

    return tokens


  def convert_broken_comments_to_comments(self, tokens):
    for token in tokens:
      if token.group in ['comment-start', 'comment-middle', 'comment-end']:
        token.group = 'comment'

    return tokens


  def unwrapped_code(self, lines):
    unwrapped_lines = ''

    for line in lines:
      # remove line identification (if any)
      line = line[:72]
      line = line.rstrip()

      # wrap column-1 comment in slash-star, star-slash
      if len(line) > 0 and line[0] != ' ':
        line = '/*' + line + '*/'

      unwrapped_lines += line
      unwrapped_lines += '\n'

    return unwrapped_lines
