import string
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
  PrefixedIntegerTokenBuilder,
  LeadCommentTokenBuilder
)
# from SqlTokenBuilders import ()
from Tokenizer import Tokenizer

class Sql2008Examiner(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(True, True)
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E')
    string_tb = StringTokenBuilder(["'", '"'], True, False)
    identifier_tb = IdentifierTokenBuilder()

    terminators = [';']

    terminators_tb = ListTokenBuilder(terminators, 'statement terminator', False)

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

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'ABS',
      'ALL', 'ALLOCATE', 'ALTER', 'AND', 'ANY', 'ARE',
      'ARRAY',
      'ARRAY_AGG',
      'AS',
      'ASENSITIVE',
      'ASYMMETRIC',
      'AT',
      'ATOMIC',
      'AUTHORIZATION',
      'AVG',
      'BEGIN',
      'BETWEEN',
      'BIGINT',
      'BINARY',
      'BLOB', 'BOOLEAN',
      'BOTH',
      'BY',
      'CALL',
      'CALLED',
      'CARDINALITY',
      'CASCADE',
      'CASCADED', 'CASE', 'CAST',
      'CEIL', 'CEILING',
      'CHAR',
      'CHAR_LENGTH',
      'CHARACTER',
      'CHARACTER_LENGTH',
      'CHECK',
      'CLOB',
      'CLOSE',
      'COALESCE',
      'COLLATE',
      'COLUMN', 'COMMIT', 'CONDITION', 'CONNECT',
      'CONSTRAINT',
      'CONVERT',
      'CORR',
      'CORRESPONDING',
      'COUNT',
      'COVAR_POP', 'COVAR_SAMPLE',
      'CREATE', 'CROSS',
      'CUBE',
      'CUME_DIST',
      'CURRENT',
      'CURRENT_CATALOG',
      'CURRENT_DATE',
      'CURRENT_DEFAULT_TRANSFORM_GROUP',
      'CURRENT_PATH',
      'CURRENT_ROLE',
      'CURRENT_SCHEMA',
      'CURRENT_TIME', 'CURRENT_TIMESTAMP',
      'CURRENT_TRANSFORM_GROUP_FOR_TYPE',
      'CURRENT_USER', 'CURSOR',
      'CYCLE',
      'DATE', 'DAY', 'DEALLOCATE', 'DEC',
      'DECIMAL', 'DECLARE', 'DEFAULT',
      'DELETE',
      'DENSE_RANK',
      'DESCRIBE',
      'DETERMINISTIC',
      'DISCONNECT', 'DISTINCT',
      'DO',
      'DOUBLE', 'DROP',
      'DYNAMIC',
      'EACH',
      'ELEMENT',
      'ELSE',
      'ELSEIF',
      'END',
      'END_EXEC',
      'ESCAPE',
      'EVERY',
      'EXCEPT',
      'EXEC', 'EXECUTE', 'EXISTS',
      'EXTERNAL',
      'EXTRACT',
      'FALSE', 'FETCH',
      'FILTER',
      'FLOAT', 'FOR', 'FOREIGN',
      'FREE',
      'FROM', 'FULL', 'FUNCTION',
      'GET', 'GLOBAL',
      'GRANT', 'GROUP',
      'GROUPING',
      'HANDLER', 'HAVING',
      'HOLD',
      'HOUR',
      'IDENTITY',
      'IF',
      'IN', 'INDICATOR',
      'INNER', 'INOUT',
      'INSENSITIVE', 'INSERT',
      'INT', 'INTEGER', 'INTERSECT',
      'INTERSECTION',
      'INTERVAL', 'INTO', 'IS',
      'ITERATE',
      'JOIN',
      'LANGUAGE',
      'LARGE',
      'LATERAL',
      'LEADING',
      'LEAVE',
      'LEFT', 'LEVEL', 'LIKE',
      'LIKE_REGEX',
      'LN',
      'LOCAL',
      'LOCALTIME', 'LOCALTIMESTAMP',
      'LOOP',
      'LOWER',
      'MAP',
      'MATCH',
      'MAX',
      'MEMBER', 'MERGE',
      'METHOD',
      'MIN',
      'MINUTE',
      'MOD',
      'MODIFIES',
      'MODULE', 'MONTH',
      'MULTISET',
      'NATIONAL', 'NATURAL', 'NCHAR',
      'NCLOB', 'NEW',
      'NO',
      'NONE',
      'NORMALIZE',
      'NOT',
      'NULL',
      'NULLIF',
      'NUMERIC',
      'OCTET_LENGTH',
      'OF',
      'OLD',
      'ON',
      'ONLY', 'OPEN',
      'OR', 'ORDER',
      'OUT', 'OUTER',
      'OVERLAPS',
      'OVERLAY',
      'PARAMETER',
      'PARTITION',
      'PERCENT_RANK', 'PERCENTILE_CONT', 'PERCENTILE_DISC',
      'POSITION',
      'POSITION_REGEX', 'POWER',
      'PRECISION',
      'PREPARE',
      'PRIMARY',
      'PROCEDURE',
      'RANGE',
      'RANK',
      'READS',
      'REAL',
      'RECURSIVE', 'REF',
      'REFERENCES',
      'REFERENCING',
      'REGR_AVGX', 'REGR_AVGY', 'REGR_COUNT', 'REGR_INTERCEPT', 'REGR_R2',
      'REGR_SLOPE', 'REGR_SXX', 'REGR_SXY', 'REGR_SYY',
      'RELEASE', 'REPEAT', 'REGIONAL',
      'RESULT',
      'RETURN', 'RETURNS', 'REVOKE', 'RIGHT',
      'ROLLBACK',
      'ROLLUP',
      'ROW',
      'ROW_NUMBER',
      'ROWS',
      'SAVEPOINT',
      'SCOPE',
      'SCROLL',
      'SEARCH',
      'SECOND',
      'SELECT',
      'SENSITIVE',
      'SESSION_USER', 'SET',
      'SIGNAL',
      'SIMILAR',
      'SMALLINT', 'SOME',
      'SPECIFIC',
      'SPECIFICTYPE',
      'SQL',
      'SQLEXCEPTION', 'SQLSTATE', 'SQLWARNING',
      'SQRT',
      'START',
      'STATIC',
      'STDDEV_POP', 'STDDEV_SAMP',
      'SUBMULTISET',
      'SUBSTRING',
      'SUBSTRING_REGEX',
      'SUM',
      'SYMMETRIC', 'SYSTEM',
      'SYSTEM_USER',
      'TABLE',
      'TABLESAMPLE',
      'THEN', 'TIME', 'TIMESTAMP', 'TIMEZONE_HOUR',
      'TIMEZONE_MINUTE', 'TO', 'TRAILING',
      'TRANSLATE',
      'TRANSLATE_REGEX',
      'TRANSLATION',
      'TREAT', 'TRIGGER',
      'TRIM',
      'TRUE',
      'TRUNCATE',
      'UESCAPE',
      'UNDO',
      'UNION', 'UNIQUE', 'UNKNOWN',
      'UNNEST', 'UNTIL',
      'UPDATE',
      'UPPER',
      'USER', 'USING',
      'VALUE', 'VALUES',
      'VAR_POP', 'VAR_SAMP',
      'VARCHAR', 'VARYING',
      'VIEW',
      'WHEN', 'WHENEVER', 'WHERE',
      'WHILE',
      'WIDTH_BUCKET',
      'WINDOW',
      'WITH',
      'WITHIN', 'WITHOUT',
      'YEAR'
    ]

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
    self.calc_operator_3_confidence()
    # self.calc_operand_confidence()
    self.calc_keyword_confidence()
