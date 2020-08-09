import string

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
  LeadToEndOfLineTokenBuilder
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
    SqlBracketedIdentifierTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, year, extension):
    super().__init__()

    if year is not None and year not in ['92', '1992', '99', '1999', '2003', '2008', '2011', '2016']:
      raise CodeStatException('Unknown year for language')

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
    bracketed_identifier_tb = SqlBracketedIdentifierTokenBuilder()
    operand_types.append('identifier')

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    comment_tb = LeadToEndOfLineTokenBuilder('--', True, 'comment')

    known_operators = [
      '=', '>', '>=', '<', '<=', '<>', '!=',
      'AND', 'OR', 'NOT',
      'IN', 'EXISTS', 'LIKE', 'BETWEEN', 'ANY', 'ALL',
      '.'
    ]

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
      'ABSOLUTE', 'ACTION', 'ADD', 'ALL', 'ALLOCATE', 'ALTER', 'ARE',
      'AS', 'ASC', 'ASSERTION', 'AT', 'AUTHORIZATION',
      'BEFORE', 'BEGIN', 'BETWEEN', 'BIT_LENGTH', 'BOTH', 'BY',
      'CALL', 'CASCADE', 'CASCADED', 'CASE', 'CAST', 'CATALOG',
      'CHAR_LENGTH', 'CHARACTER_LENGTH',
      'CHECK', 'COALESCE', 'COLLATE', 'COLLATION',
      'COLUMN', 'COMMIT', 'CONDITION', 'CONNECT',
      'CONNECTION', 'CONSTRAINT', 'CONSTRAINTS', 'CONTAINS', 'CONTINUE',
      'CONVERT', 'CORRESPONDING', 'COUNT', 'CREATE', 'CROSS',
      'CURRENT', 'CURRENT_DATE', 'CURRENT_PATH', 'CURRENT_TIME', 'CURRENT_TIMESTAMP',
      'CURRENT_USER', 'CURSOR',
      'DAY', 'DEALLOCATE', 'DEC', 'DECLARE', 'DEFAULT',
      'DEFERRABLE', 'DEFERRED', 'DELETE', 'DEPTH', 'DESC', 'DESCRIBE',
      'DESCRIPTOR', 'DETERMINISTIC', 'DIAGNOSTICS', 'DISCONNECT', 'DISTINCT',
      'DO', 'DOMAIN', 'DROP',
      'ELSE', 'END', 'ESCAPE', 'EXCEPT', 'EXCEPTION',
      'EXEC', 'EXECUTE', 'EXISTS', 'EXIT', 'EXTERNAL', 'EXTRACT',
      'FETCH', 'FIRST', 'FOR', 'FOREIGN', 'FOUND',
      'FROM', 'FULL', 'FUNCTION', 'FUSION',
      'GENERAL', 'GET', 'GLOBAL', 'GO', 'GOTO', 'GRANT', 'GROUP',
      'HANDLER', 'HAVING', 'HOUR',
      'IDENTITY', 'IF', 'IMMEDIATE', 'IN', 'INDICATOR', 'INITIALLY', 'INNER',
      'INOUT', 'INPUT', 'INSENSITIVE', 'INSERT', 'INT', 'INTERSECT',
      'INTERVAL', 'INTO', 'IS', 'ISOLATION',
      'JOIN',
      'KEY',
      'LANGUAGE', 'LAST', 'LEADING', 'LEFT', 'LEVEL', 'LIKE', 'LOCAL',
      'MATCH', 'MAX', 'MIN', 'MINUTE', 'MODULE', 'MONTH',
      'NAMES', 'NATIONAL', 'NATURAL', 'NEXT', 'NO', 'NOT',
      'NULLIF', 'NUMERIC',
      'OCTET_LENGTH', 'OF', 'ONLY', 'OPEN', 'OPTION', 'ORDER',
      'OUTPUT', 'OVERLAPS',
      'PAD', 'PARAMETER', 'PARTIAL', 'PRECISION', 'PREPARE', 'PRESERVE',
      'PRIMARY', 'PRIOR', 'PRIVILEGES', 'PROCEDURE', 'PUBLIC',
      'READ', 'REFERENCES', 'RELATIVE', 'RESTRICT',
      'RETURN', 'RETURNS', 'REVOKE', 'RIGHT', 'ROLLBACK', 'ROLLUP',
      'READS', 'ROWS',
      'SCHEMA', 'SCROLL', 'SECOND', 'SECTION', 'SELECT', 'SESSION',
      'SESSION_USER', 'SET', 'SIZE', 'SOME', 'SPACE',
      'SPECIFIC', 'SQL', 'SQLCODE', 'SQLERROR',
      'SQLEXCEPTION', 'SQLSTATE', 'SQLWARNING', 'SUBSTRING', 'SUM',
      'SYSTEM_USER',
      'TABLE', 'TEMPORARY', 'THEN', 'TIME', 'TIMESTAMP', 'TIMEZONE_HOUR',
      'TIMEZONE_MINUTE', 'TO', 'TRAILING', 'TRANSACTION', 'TRANSLATE',
      'TRANSLATION', 'TRIM',
      'UNDO', 'UNION', 'UNIQUE', 'UNKNOWN', 'UPDATE', 'UPPER', 'USAGE',
      'USER', 'USING',
      'VALUE', 'VALUES', 'VARYING', 'VIEW',
      'WHEN', 'WHENEVER', 'WHERE', 'WITH', 'WORK', 'WRITE',
      'YEAR',
      'ZONE'
    ]

    keywords_99 = [
      'AFTER', 'ARRAY', 'ASENSITIVE', 'ASYMMETRIC', 'ATOMIC',
      'BINARY', 'BOOLEAN', 'BREADTH',
      'CLOSE', 'CONSTRUCTOR', 'CUBE',
      'CURRENT_DEFAULT_TRANSFORM_GROUP', 'CURRENT_ROLE',
      'CURRENT_TRANSFORM_GROUP_FOR_TYPE', 'CYCLE',
      'DYNAMIC',
      'EACH', 'ELSEIF', 'EQUALS',
      'FILTER', 'FREE',
      'GROUPING',
      'HOLD',
      'ITERATE',
      'LARGE', 'LATERAL', 'LEAVE', 'LOCALTIME', 'LOCALTIMESTAMP', 'LOCATOR', 'LOOP',
      'MAP', 'METHOD', 'MODIFIES',
      'NEW',
      'OBJECT', 'OLD', 'ORDINALITY', 'OUT', 'OUTER',
      'PARTITION',
      'RECURSIVE', 'REF', 'REFERENCING', 'RELEASE', 'REPEAT', 'REGIONAL',
      'RESULT', 'ROW',
      'SAVEPOINT', 'SCOPE', 'SEARCH', 'SENSITIVE', 'SETS', 'SIGNAL', 'SIMILAR',
      'SPECIFICTYPE', 'START', 'STATE', 'STATIC', 'SYMMETRIC', 'SYSTEM',
      'TREAT', 'TRIGGER',
      'UNDER', 'UNNEST', 'UNTIL',
      'WHILE', 'WINDOW', 'WITHIN', 'WITHOUT'
    ]

    keywords_2003 = [
      'CALLED',
      'ELEMENT',
      'LOWER',
      'MEMBER', 'MERGE', 'MULTISET',
      'OVERLAY',
      'RANGE',
      'SUBMULTISET',
      'TABLESAMPLE'
    ]

    keywords_2008 = [
      'ABS', 'ARRAY_AGG', 'AVG',
      'BEGIN_FRAME', 'BEGIN_PARTITION',
      'CARDINALITY', 'CEIL', 'CEILING', 'CONVERT', 'CORR', 'COVAR_POP', 'COVAR_SAMPLE',
      'CUME_DIST', 'CURRENT_CATALOG', 'CURRENT_SCHEMA',
      'DENSE_RANK',
      'END_EXEC', 'EVERY',
      'INTERSECTION',
      'LIKE_REGEX', 'LN',
      'MOD',
      'NORMALIZE',
      'OCTET_LENGTH', 'OFFSET',
      'PERCENT_RANK', 'PERCENTILE_CONT', 'PERCENTILE_DISC', 'POSITION',
      'POSITION_REGEX', 'POWER',
      'RANK', 'REGR_AVGX', 'REGR_AVGY', 'REGR_COUNT', 'REGR_INTERCEPT', 'REGR_R2',
      'REGR_SLOPE', 'REGR_SXX', 'REGR_SXY', 'REGR_SYY', 'ROW_NUMBER',
      'SQRT', 'STDDEV_POP', 'STDDEV_SAMP', 'SUBSTRING_REGEX', 'SUM',
      'TRANSLATE', 'TRANSLATE_REGEX', 'TRUNCATE',
      'UESCAPE', 'UPPER',
      'VAR_POP', 'VAR_SAMP',
      'WIDTH_BUCKET'
    ]

    keywords_2011 = [
      'ARRAY_MAX_CARDINALITY',
      'EXP',
      'FIRST_VALUE', 'FRAME_ROW',
      'GROUPS',
      'INITIAL',
      'LAST_VALUE', 'LEAD',
      'MATCH_NUMBER', 'MATCH_RECOGNIZE', 'MATCHES',
      'OMIT',
      'PERCENT', 'PERIOD', 'PORTION', 'PRECEDES',
      'TRIM_ARRAY',
      'VALUE_OF',
      'VERSIONING'
    ]

    keywords_2016 = [
      'ACOS', 'ASIN', 'ATAN',
      'CLASSIFIER', 'COS', 'COSH',
      'DECFLOAT', 'DEFINE',
      'EMPTY', 'EQUALS',
      'JSON_ARRY', 'JSON_ARRAYAGG', 'JSON_EXISTS', 'JSON_OBJECT',
      'JSON_OBJECTAGG', 'JSON_QUERY', 'JSON_TABLE', 'JSON_TABLE_PRIMITIVE',
      'JSON_VALUE',
      'LAG', 'LISTAGG', 'LOG', 'LOG10',
      'NTH_VALUE', 'NTILE',
      'OCCURRENCES_REGEX', 'ONE', 'OVER',
      'PATTERN', 'PER', 'PTF',
      'RUNNING',
      'SEEK', 'SHOW', 'SIN', 'SINH', 'SUBSET',
      'TAN', 'TANH'
    ]

    if year in ['99', '1999', '2003', '2008', '2011', '2016']:
      keywords += keywords_99

    if year in ['2003', '2008', '2011', '2016']:
      keywords += keywords_2003

    if year in ['2008', '2011', '2016']:
      keywords += keywords_2008

    if year in ['2011', '2016']:
      keywords += keywords_2011

    if year in ['2016']:
      keywords += keywords_2016

    keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    values = ['TRUE', 'FALSE', 'NULL', 'OFF', 'ON', 'NONE']

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

    type_tb = CaseInsensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('value')

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
      identifier_tb,
      type_tb,
      bracketed_identifier_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['statement terminator', 'newline'], [], ['whitespace', 'comment'])
    self.tokens = tokens
    self.convert_identifiers_to_labels()

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

    # operand_types_2 = ['number', 'string', 'symbol']
    # self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    # self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()
