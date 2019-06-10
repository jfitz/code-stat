import string
from CodeStatException import CodeStatException
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IdentifierTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  PrefixedIntegerTokenBuilder,
  LeadCommentTokenBuilder
)
# from SqlTokenBuilders import ()
from Tokenizer import Tokenizer

class SqlExaminer(Examiner):
  def __init__(self, code, year, extension):
    super().__init__()

    if year is not None and year not in ['92', '1992', '99', '1999', '2003', '2008', '2011', '2016']:
      raise CodeStatException('Unknown year for language')

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(True, True)
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E')
    string_tb = StringTokenBuilder(["'", '"'], True, False)
    identifier_tb = IdentifierTokenBuilder()

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

    known_operators = [
      '=', '>', '>=', '<', '<=', '<>', '!=',
      'AND', 'OR', 'NOT',
      'IN', 'EXISTS', 'LIKE', 'BETWEEN', 'ANY', 'ALL'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      'NOT', 'EXISTS', 'ANY', 'ALL'
    ]

    groupers = ['(', ')', ',']
    group_ends = [')']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'ABSOLUTE', 'ACTION', 'ADD',
      'ALL', 'ALLOCATE', 'ALTER', 'AND', 'ANY', 'ARE',
      'AS',
      'ASSERTION',
      'AT',
      'AUTHORIZATION',
      'BEFORE',
      'BEGIN',
      'BETWEEN',
      'BIT',
      'BIT_LENGTH',
      'BOTH',
      'BY',
      'CALL',
      'CASCADE',
      'CASCADED', 'CASE', 'CAST',
      'CATALOG',
      'CHAR',
      'CHAR_LENGTH',
      'CHARACTER',
      'CHARACTER_LENGTH',
      'CHECK',
      'COALESCE',
      'COLLATE',
      'COLLATION',
      'COLUMN', 'COMMIT', 'CONDITION', 'CONNECT',
      'CONNECTION',
      'CONSTRAINT', 'CONSTRAINTS',
      'CONTAINS',
      'CONTINUE',
      'CONVERT',
      'CORRESPONDING',
      'COUNT',
      'CREATE', 'CROSS',
      'CURRENT',
      'CURRENT_DATE',
      'CURRENT_PATH',
      'CURRENT_TIME', 'CURRENT_TIMESTAMP',
      'CURRENT_USER', 'CURSOR',
      'DATE', 'DAY', 'DEALLOCATE', 'DEC',
      'DECIMAL', 'DECLARE', 'DEFAULT',
      'DEFERRABLE', 'DEFERRED',
      'DELETE',
      'DEPTH',
      'DESC',
      'DESCRIBE',
      'DESCRIPTOR',
      'DETERMINISTIC',
      'DIAGNOSTICS',
      'DISCONNECT', 'DISTINCT',
      'DO',
      'DOMAIN',
      'DOUBLE', 'DROP',
      'ELSE',
      'END',
      'ESCAPE',
      'EXCEPT',
      'EXCEPTION',
      'EXEC', 'EXECUTE', 'EXISTS',
      'EXIT',
      'EXTERNAL',
      'EXTRACT',
      'FALSE', 'FETCH',
      'FIRST',
      'FLOAT', 'FOR', 'FOREIGN',
      'FOUND',
      'FROM', 'FULL', 'FUNCTION',
      'FUSION',
      'GENERAL',
      'GET', 'GLOBAL',
      'GO', 'GOTO',
      'GRANT', 'GROUP',
      'HANDLER', 'HAVING',
      'HOUR',
      'IDENTITY',
      'IF',
      'IMMEDIATE',
      'IN', 'INDICATOR',
      'INITIALLY',
      'INNER', 'INOUT',
      'INPUT',
      'INSENSITIVE', 'INSERT',
      'INT', 'INTEGER', 'INTERSECT',
      'INTERVAL', 'INTO', 'IS',
      'ISOLATION',
      'JOIN',
      'KEY',
      'LANGUAGE',
      'LAST',
      'LEADING',
      'LEFT', 'LEVEL', 'LIKE',
      'LOCAL',
      'MATCH',
      'MAX',
      'MIN',
      'MINUTE',
      'MODULE', 'MONTH',
      'NAMES',
      'NATIONAL', 'NATURAL', 'NCHAR',
      'NEXT',
      'NO',
      'NOT',
      'NULL', 'NULLIF',
      'NUMERIC',
      'OCTET_LENGTH',
      'OF',
      'ON',
      'ONLY', 'OPEN',
      'OPTION',
      'OR', 'ORDER',
      'OUTPUT',
      'OVERLAPS',
      'PAD',
      'PARAMETER',
      'PARTIAL',
      'PRECISION',
      'PREPARE',
      'PRESERVE',
      'PRIMARY',
      'PRIOR',
      'PRIVILEGES',
      'PROCEDURE',
      'PUBLIC',
      'READ',
      'REAL',
      'REFERENCES',
      'RELATIVE',
      'RESTRICT',
      'RETURN', 'RETURNS', 'REVOKE', 'RIGHT',
      'ROLLBACK',
      'ROLLUP',
      'READS',
      'ROWS',
      'SCHEMA',
      'SCROLL',
      'SECOND',
      'SECTION',
      'SELECT',
      'SESSION',
      'SESSION_USER', 'SET',
      'SIZE',
      'SMALLINT', 'SOME',
      'SPACE',
      'SPECIFIC',
      'SQL',
      'SQLCODE', 'SQLERROR',
      'SQLEXCEPTION', 'SQLSTATE', 'SQLWARNING',
      'SUBSTRING',
      'SUM',
      'SYSTEM_USER',
      'TABLE',
      'TEMPORARY',
      'THEN', 'TIME', 'TIMESTAMP', 'TIMEZONE_HOUR',
      'TIMEZONE_MINUTE', 'TO', 'TRAILING',
      'TRANSACTION',
      'TRANSLATE',
      'TRANSLATION',
      'TRIM',
      'TRUE',
      'UNDO',
      'UNION', 'UNIQUE', 'UNKNOWN',
      'UPDATE',
      'UPPER',
      'USAGE',
      'USER', 'USING',
      'VALUE', 'VALUES',
      'VARCHAR', 'VARYING',
      'VIEW',
      'WHEN', 'WHENEVER', 'WHERE',
      'WITH',
      'WORK', 'WRITE',
      'YEAR',
      'ZONE'
    ]

    keywords_99 = [
      'AFTER',
      'ARRAY',
      'ASENSITIVE',
      'ASYMMETRIC',
      'ATOMIC',
      'BINARY',
      'BLOB', 'BOOLEAN',
      'BREADTH',
      'CLOB',
      'CLOSE',
      'CONSTRUCTOR',
      'CUBE',
      'CURRENT_DEFAULT_TRANSFORM_GROUP',
      'CURRENT_ROLE',
      'CURRENT_TRANSFORM_GROUP_FOR_TYPE',
      'CYCLE',
      'DYNAMIC',
      'EACH',
      'ELSEIF',
      'EQUALS',
      'FILTER',
      'FREE',
      'GROUPING',
      'HOLD',
      'ITERATE',
      'LARGE',
      'LATERAL',
      'LEAVE',
      'LOCALTIME', 'LOCALTIMESTAMP',
      'LOCATOR',
      'LOOP',
      'MAP',
      'METHOD',
      'MODIFIES',
      'NCLOB', 'NEW',
      'NONE',
      'OBJECT',
      'OLD',
      'ORDINALITY',
      'OUT', 'OUTER',
      'PARTITION',
      'RECURSIVE', 'REF',
      'REFERENCING',
      'RELEASE', 'REPEAT', 'REGIONAL',
      'RESULT',
      'ROW',
      'SAVEPOINT',
      'SCOPE',
      'SEARCH',
      'SENSITIVE',
      'SETS',
      'SIGNAL',
      'SIMILAR',
      'SPECIFICTYPE',
      'START',
      'STATE',
      'STATIC',
      'SYMMETRIC', 'SYSTEM',
      'TREAT', 'TRIGGER',
      'UNDER',
      'UNNEST', 'UNTIL',
      'WHILE',
      'WINDOW',
      'WITHIN', 'WITHOUT'
    ]

    keywords_2003 = [
      'BIGINT',
      'CALLED',
      'ELEMENT',
      'LOWER',
      'MEMBER', 'MERGE',
      'MULTISET',
      'OVERLAY',
      'RANGE',
      'SUBMULTISET',
      'TABLESAMPLE'
    ]

    keywords_2008 = [
      'ABS',
      'ARRAY_AGG',
      'AVG',
      'BEGIN_FRAME', 'BEGIN_PARTITION',
      'CARDINALITY',
      'CEIL', 'CEILING',
      'CONVERT',
      'CORR',
      'COVAR_POP', 'COVAR_SAMPLE',
      'CUME_DIST',
      'CURRENT_CATALOG',
      'CURRENT_SCHEMA',
      'DENSE_RANK',
      'END_EXEC',
      'EVERY',
      'INTERSECTION',
      'LIKE_REGEX',
      'LN',
      'MOD',
      'NORMALIZE',
      'OCTET_LENGTH',
      'OFFSET',
      'PERCENT_RANK', 'PERCENTILE_CONT', 'PERCENTILE_DISC',
      'POSITION',
      'POSITION_REGEX', 'POWER',
      'RANK',
      'REGR_AVGX', 'REGR_AVGY', 'REGR_COUNT', 'REGR_INTERCEPT', 'REGR_R2',
      'REGR_SLOPE', 'REGR_SXX', 'REGR_SXY', 'REGR_SYY',
      'ROW_NUMBER',
      'SQRT',
      'STDDEV_POP', 'STDDEV_SAMP',
      'SUBSTRING_REGEX',
      'SUM',
      'TRANSLATE',
      'TRANSLATE_REGEX',
      'TRIM',
      'TRUNCATE',
      'UESCAPE',
      'UPPER',
      'VAR_POP', 'VAR_SAMP',
      'WIDTH_BUCKET'
    ]

    keywords_2011 = [
      'ARRAY_MAX_CARDINALITY',
      'EXP',
      'FIRST_VALUE',
      'FRAME_ROW',
      'GROUPS',
      'INITIAL',
      'LAST_VALUE',
      'LEAD',
      'MATCH_NUMBER', 'MATCH_RECOGNIZE', 'MATCHES',
      'OMIT',
      'PERCENT',
      'PERIOD', 'PORTION',
      'PRECEDES',
      'TRIM_ARRAY',
      'VALUE_OF',
      'VERSIONING'
    ]

    keywords_2016 = [
      'ACOS',
      'ASIN',
      'ATAN',
      'CLASSIFIER',
      'COS', 'COSH',
      'DECFLOAT',
      'DEFINE',
      'EMPTY',
      'EQUALS',
      'JSON_ARRY', 'JSON_ARRAYAGG', 'JSON_EXISTS', 'JSON_OBJECT',
      'JSON_OBJECTAGG', 'JSON_QUERY', 'JSON_TABLE', 'JSON_TABLE_PRIMITIVE',
      'JSON_VALUE',
      'LAG',
      'LISTAGG',
      'LOG', 'LOG10',
      'NTH_VALUE', 'NTILE',
      'OCCURRENCES_REGEX',
      'ONE',
      'OVER',
      'PATTERN', 'PER',
      'PTF',
      'RUNNING',
      'SEEK',
      'SHOW',
      'SIN', 'SINH',
      'SUBSET',
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

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      string_tb,
      known_operator_tb,
      terminators_tb,
      groupers_tb,
      keyword_tb,
      identifier_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    # operand_types = ['number', 'string', 'symbol']
    # self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_statistics()
