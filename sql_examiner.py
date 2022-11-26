import string

from codestat_token import Token
from codestat_exception import CodeStatException
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StuffedQuoteStringTokenBuilder,
  IdentifierTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  PrefixedIntegerTokenBuilder,
  LeadToEndOfLineTokenBuilder,
  NullTokenBuilder
)
from sql_token_builders import SqlBracketedIdentifierTokenBuilder
from examiner import Examiner

class SqlExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StuffedQuoteStringTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    NullTokenBuilder.__escape_z__()
    SqlBracketedIdentifierTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, extension):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(True, True, None)
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E', None)
    operand_types.append('number')

    quotes = ["'", '"']
    string_tb = StuffedQuoteStringTokenBuilder(quotes, False)
    operand_types.append('string')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    bracketed_identifier_tb = NullTokenBuilder()

    if extension in ['microsoft', 't-sql']:
      bracketed_identifier_tb = SqlBracketedIdentifierTokenBuilder()

    operand_types.append('identifier')

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    comment_tb = LeadToEndOfLineTokenBuilder('--', True, 'comment')

    known_operators = [
      '=', '>', '>=', '<', '<=', '<>', '!=',
      'AND', 'OR', 'NOT',
      'IN', 'EXISTS', 'LIKE', 'BETWEEN', 'ANY', 'ALL', 'SOME',
      '.'
    ]

    operators_tsql = [
      '&', '|', '^', '~', '>>', '<<', '%',
      '+=', '-=', '*=', '/=', '%=', '&=', '^=', '|=',
      'GENERATE_SERIES',
      'OPENDATASOURCE', 'OPENQUERY', 'OPENROWSET', 'OPENXML', 'OPENJSON',
      'PREDICT',
      'STRING_SPLIT'
    ]

    if extension in ['microsoft', 't-sql']:
      known_operators += operators_tsql

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      'NOT', 'EXISTS', 'ANY', 'ALL'
    ]

    groupers = ['(', ')', ',']
    group_starts = ['(', ',']
    group_mids = [',']
    group_ends = [')']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'ACOS', 'ASIN', 'ATAN',
      'ABSOLUTE', 'ACTION', 'ADD', 'ALL', 'ALLOCATE', 'ALTER', 'ARE',
      'ABS', 'ARRAY_AGG', 'AVG',
      'AS', 'ASC', 'ASSERTION', 'AT', 'AUTHORIZATION',
      'AFTER', 'ARRAY', 'ASENSITIVE', 'ASYMMETRIC', 'ATOMIC',
      'ARRAY_MAX_CARDINALITY',
      'BEFORE', 'BEGIN', 'BETWEEN', 'BIT_LENGTH', 'BOTH', 'BY',
      'BEGIN_FRAME', 'BEGIN_PARTITION',
      'BINARY', 'BOOLEAN', 'BREADTH',
      'CALL', 'CASCADE', 'CASCADED', 'CASE', 'CAST', 'CATALOG',
      'CALLED',
      'CHAR_LENGTH', 'CHARACTER_LENGTH',
      'CHECK', 'COALESCE', 'COLLATE', 'COLLATION',
      'COLUMN', 'COMMIT', 'CONDITION', 'CONNECT',
      'CONNECTION', 'CONSTRAINT', 'CONSTRAINTS', 'CONTAINS', 'CONTINUE',
      'CONVERT', 'CORRESPONDING', 'COUNT', 'CREATE', 'CROSS',
      'CURRENT', 'CURRENT_DATE', 'CURRENT_PATH', 'CURRENT_TIME', 'CURRENT_TIMESTAMP',
      'CURRENT_USER', 'CURSOR',
      'CLOSE', 'CONSTRUCTOR', 'CUBE',
      'CURRENT_DEFAULT_TRANSFORM_GROUP', 'CURRENT_ROLE',
      'CURRENT_TRANSFORM_GROUP_FOR_TYPE', 'CYCLE',
      'CARDINALITY', 'CEIL', 'CEILING', 'CONVERT', 'CORR', 'COVAR_POP', 'COVAR_SAMPLE',
      'CUME_DIST', 'CURRENT_CATALOG', 'CURRENT_SCHEMA',
      'CLASSIFIER', 'COS', 'COSH',
      'DAY', 'DEALLOCATE', 'DEC', 'DECLARE', 'DEFAULT',
      'DECFLOAT', 'DEFINE',
      'DEFERRABLE', 'DEFERRED', 'DELETE', 'DEPTH', 'DESC', 'DESCRIBE',
      'DENSE_RANK',
      'DESCRIPTOR', 'DETERMINISTIC', 'DIAGNOSTICS', 'DISCONNECT', 'DISTINCT',
      'DO', 'DOMAIN', 'DROP',
      'DYNAMIC',
      'ELSE', 'END', 'ESCAPE', 'EXCEPT', 'EXCEPTION',
      'ELEMENT',
      'EXEC', 'EXECUTE', 'EXISTS', 'EXIT', 'EXTERNAL', 'EXTRACT',
      'EACH', 'ELSEIF', 'EQUALS',
      'END_EXEC', 'EVERY',
      'EXP',
      'EMPTY', 'EQUALS',
      'FETCH', 'FIRST', 'FOR', 'FOREIGN', 'FOUND',
      'FROM', 'FULL', 'FUNCTION', 'FUSION',
      'FILTER', 'FREE',
      'FIRST_VALUE', 'FRAME_ROW',
      'GENERAL', 'GET', 'GLOBAL', 'GO', 'GOTO', 'GRANT', 'GROUP',
      'GROUPING',
      'GROUPS',
      'HANDLER', 'HAVING', 'HOUR',
      'HOLD',
      'IDENTITY', 'IF', 'IMMEDIATE', 'IN', 'INDICATOR', 'INITIALLY', 'INNER',
      'INOUT', 'INPUT', 'INSENSITIVE', 'INSERT', 'INT', 'INTERSECT',
      'INITIAL',
      'INTERVAL', 'INTO', 'IS', 'ISOLATION',
      'INTERSECTION',
      'ITERATE',
      'JOIN',
      'JSON_ARRY', 'JSON_ARRAYAGG', 'JSON_EXISTS', 'JSON_OBJECT',
      'JSON_OBJECTAGG', 'JSON_QUERY', 'JSON_TABLE', 'JSON_TABLE_PRIMITIVE',
      'JSON_VALUE',
      'KEY',
      'LANGUAGE', 'LAST', 'LEADING', 'LEFT', 'LEVEL', 'LIKE', 'LOCAL',
      'LARGE', 'LATERAL', 'LEAVE', 'LOCALTIME', 'LOCALTIMESTAMP', 'LOCATOR', 'LOOP',
      'LAG', 'LISTAGG', 'LOG', 'LOG10',
      'LIKE_REGEX', 'LN',
      'LOWER',
      'LAST_VALUE', 'LEAD',
      'MATCH', 'MAX', 'MIN', 'MINUTE', 'MODULE', 'MONTH',
      'MAP', 'METHOD', 'MODIFIES',
      'MATCH_NUMBER', 'MATCH_RECOGNIZE', 'MATCHES',
      'MEMBER', 'MERGE', 'MULTISET',
      'MOD',
      'NAMES', 'NATIONAL', 'NATURAL', 'NEXT', 'NO', 'NOT',
      'NULLIF', 'NUMERIC',
      'NTH_VALUE', 'NTILE',
      'NEW',
      'NORMALIZE',
      'OCTET_LENGTH', 'OF', 'ON', 'ONLY', 'OPEN', 'OPTION', 'ORDER',
      'OUTPUT', 'OVERLAPS',
      'OBJECT', 'OLD', 'ORDINALITY', 'OUT', 'OUTER',
      'OCTET_LENGTH', 'OFFSET',
      'OMIT',
      'OCCURRENCES_REGEX', 'OVER',
      'OVERLAY',
      'PAD', 'PARAMETER', 'PARTIAL', 'PRECISION', 'PREPARE', 'PRESERVE',
      'PRIMARY', 'PRIOR', 'PRIVILEGES', 'PROCEDURE', 'PUBLIC',
      'PATTERN', 'PER', 'PTF',
      'PARTITION',
      'PERCENT_RANK', 'PERCENTILE_CONT', 'PERCENTILE_DISC', 'POSITION',
      'PERCENT', 'PERIOD', 'PORTION', 'PRECEDES',
      'POSITION_REGEX', 'POWER',
      'RANGE',
      'READ', 'REFERENCES', 'RELATIVE', 'RESTRICT',
      'RETURN', 'RETURNS', 'REVOKE', 'RIGHT', 'ROLLBACK', 'ROLLUP',
      'READS', 'ROWS',
      'RECURSIVE', 'REF', 'REFERENCING', 'RELEASE', 'REPEAT', 'REGIONAL',
      'RESULT', 'ROW',
      'RANK', 'REGR_AVGX', 'REGR_AVGY', 'REGR_COUNT', 'REGR_INTERCEPT', 'REGR_R2',
      'REGR_SLOPE', 'REGR_SXX', 'REGR_SXY', 'REGR_SYY', 'ROW_NUMBER',
      'RUNNING',
      'SCHEMA', 'SCROLL', 'SECOND', 'SECTION', 'SELECT', 'SESSION',
      'SESSION_USER', 'SET', 'SIZE', 'SPACE',
      'SPECIFIC', 'SQL', 'SQLCODE', 'SQLERROR',
      'SQLEXCEPTION', 'SQLSTATE', 'SQLWARNING', 'SUBSTRING', 'SUM',
      'SQRT', 'STDDEV_POP', 'STDDEV_SAMP', 'SUBSTRING_REGEX', 'SUM',
      'SEEK', 'SHOW', 'SIN', 'SINH', 'SUBSET',
      'SUBMULTISET',
      'SYSTEM_USER',
      'SAVEPOINT', 'SCOPE', 'SEARCH', 'SENSITIVE', 'SETS', 'SIGNAL', 'SIMILAR',
      'SPECIFICTYPE', 'START', 'STATE', 'STATIC', 'SYMMETRIC', 'SYSTEM',
      'TABLE', 'TEMPORARY', 'THEN', 'TIME', 'TIMESTAMP', 'TIMEZONE_HOUR',
      'TABLESAMPLE'
      'TAN', 'TANH'
      'TIMEZONE_MINUTE', 'TO', 'TRAILING', 'TRANSACTION', 'TRANSLATE',
      'TRANSLATION', 'TRIM',
      'TRANSLATE', 'TRANSLATE_REGEX', 'TRUNCATE',
      'TREAT', 'TRIGGER',
      'TRIM_ARRAY',
      'UNDO', 'UNION', 'UNIQUE', 'UNKNOWN', 'UPDATE', 'UPPER', 'USAGE',
      'USER', 'USING',
      'UNDER', 'UNNEST', 'UNTIL',
      'UESCAPE', 'UPPER',
      'VALUE', 'VALUES', 'VARYING', 'VIEW',
      'VAR_POP', 'VAR_SAMP',
      'VALUE_OF', 'VERSIONING'
      'WHEN', 'WHENEVER', 'WHERE', 'WITH', 'WORK', 'WRITE',
      'WHILE', 'WINDOW', 'WITHIN', 'WITHOUT'
      'WIDTH_BUCKET'
      'YEAR',
      'ZONE'
    ]

    keywords_tsql = [
      'BEGIN', 'BREAK', 'BULK',
      'CASE', 'CATCH', 'CONTINUE', 'CLUSTERED',
      'DATEADD', 'DECLARE',
      'ELSE', 'END',
      'GENERATED', 'GETDATE', 'GOTO',
      'INSTEAD',
      'RETURN',
      'SET',
      'TEXTIMAGE_ON', 'TRY',
      'UPDLOCK',
      'WAITFOR'
    ]

    keywords_plsql = [
      '%TYPE', 'BEFORE', 'DECODE', 'DESCRIBE', 'DUAL', 'INTERSECT', 'MINUS',
      'SYSDATE', 'USER'
    ]

    if extension in ['microsoft', 't-sql']:
      keywords += keywords_tsql

    if extension in ['oracle', 'pl-sql']:
      keywords += keywords_plsql

    keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    values = ['TRUE', 'FALSE', 'NULL', 'OFF', 'NONE', 'ONE']

    values_tsql = [
      'ALLOW_ROW_LOCKS', 'ALLOW_PAGE_LOCKS', 'ALWAYS', 'IGNORE_DUP_KEY',
      'FILLFACTOR', 'HISTORY_TABLE', 'PAD_INDEX',
      'STATISTICS_NORECOMPUTE', 'SUSER_SNAME', 'SYSTEM_VERSIONING',
      'SYSTEM_TIME'
    ]

    if extension in ['microsoft', 't-sql']:
      values += values_tsql

    values_tb = CaseInsensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    types = [
      'BIGINT', 'BIT', 'BLOB',
      'CHAR', 'CHARACTER', 'CLOB',
      'DATE', 'DECIMAL', 'DOUBLE',
      'FLOAT',
      'INTEGER',
      'NCHAR', 'NCLOB',
      'REAL',
      'SMALLINT',
      'VARCHAR'
    ]

    types_tsql = [
      'nvarchar', 'bigint', 'datetime', 'datetime2', 'geography'
    ]

    types_plsql = [
      'VARCHAR2'
    ]

    if extension in ['microsoft', 't-sql']:
      types += types_tsql

    if extension in ['oracle', 'pl-sql']:
      types += types_plsql

    type_tb = CaseInsensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      string_tb,
      known_operator_tb,
      terminators_tb,
      groupers_tb,
      keyword_tb,
      values_tb,
      type_tb,
      identifier_tb,
      bracketed_identifier_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['statement terminator', 'newline'], [], ['whitespace', 'comment'])
    self.tokens = tokens
    self.convert_identifiers_to_labels()
    self.convert_keywords_to_values()

    self.calc_statistics()

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

    operand_types_2 = ['number', 'string', 'identifier', 'value']
    self.calc_operand_n_confidence(tokens, operand_types_2, 1)
    # self.calc_operand_n_confidence(tokens, operand_types, 1)

    self.calc_keyword_confidence()
    self.calc_line_length_confidence(code, self.max_expected_line)


  # convert identifiers after 'goto' to labels
  def convert_keywords_to_values(self):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'keyword' and token.text.lower() == 'on' and \
        prev_token.group == 'operator':
        token.group = 'value'
        token.is_operand = True

      if token.group not in ['whitespace', 'comment', 'newline', 'line description']:
        prev_token = token
