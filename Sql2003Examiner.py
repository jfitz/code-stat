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

class Sql2003Examiner(Examiner):
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
      'ALL', 'ALLOCATE', 'ALTER', 'AND', 'ANY', 'ARE',
      'ARRAY',
      'AS',
      'ASENSITIVE',
      'ASYMMETRIC',
      'AT',
      'ATOMIC',
      'AUTHORIZATION',
      'BEGIN',
      'BETWEEN',
      'BIGINT',
      'BINARY',
      'BLOB', 'BOOLEAN',
      'BOTH',
      'BY',
      'CALL',
      'CALLED',
      'CASCADE', 'CASCADED', 'CASE', 'CAST',
      'CHAR',
      'CHARACTER',
      'CHECK',
      'CLOB', 'CLOSE',
      'COLLATE',
      'COLUMN', 'COMMIT', 'CONDITION', 'CONNECT',
      'CONSTRAINT',
      'CORRESPONDING',
      'CREATE', 'CROSS',
      'CUBE',
      'CURRENT',
      'CURRENT_DATE',
      'CURRENT_DEFAULT_TRANSFORM_GROUP',
      'CURRENT_PATH',
      'CURRENT_ROLE',
      'CURRENT_TIME', 'CURRENT_TIMESTAMP',
      'CURRENT_TRANSFORM_GROUP_FOR_TYPE',
      'CURRENT_USER', 'CURSOR',
      'CYCLE',
      'DATE', 'DAY', 'DEALLOCATE', 'DEC',
      'DECIMAL', 'DECLARE', 'DEFAULT',
      'DELETE',
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
      'ESCAPE',
      'EXCEPT',
      'EXEC', 'EXECUTE', 'EXISTS',
      'EXIT',
      'EXTERNAL',
      'FALSE', 'FETCH',
      'FILTER',
      'FLOAT', 'FOR', 'FOREIGN',
      'FREE',
      'FROM', 'FULL', 'FUNCTION',
      'FUSION',
      'GET', 'GLOBAL',
      'GRANT', 'GROUP',
      'GROUPING',
      'HANDLER', 'HAVING',
      'HOLD',
      'HOUR',
      'IDENTITY',
      'IF',
      'IMMEDIATE',
      'IN', 'INDICATOR',
      'INNER', 'INOUT',
      'INPUT',
      'INSENSITIVE', 'INSERT',
      'INT', 'INTEGER', 'INTERSECT',
      'INTERVAL', 'INTO', 'IS',
      'ITERATE',
      'JOIN',
      'LANGUAGE',
      'LARGE',
      'LATERAL',
      'LEADING',
      'LEAVE',
      'LEFT', 'LEVEL', 'LIKE',
      'LOCAL',
      'LOCALTIME', 'LOCALTIMESTAMP',
      'LOOP',
      'MAP',
      'MATCH',
      'MEMBER', 'MERGE',
      'METHOD',
      'MINUTE',
      'MODIFIES',
      'MODULE', 'MONTH',
      'MULTISET',
      'NATIONAL', 'NATURAL', 'NCHAR',
      'NCLOB', 'NEW',
      'NO',
      'NONE',
      'NOT', 'NULL',
      'NUMERIC',
      'OF',
      'OLD',
      'ON', 'ONLY', 'OPEN',
      'OR', 'ORDER',
      'OUT', 'OUTER',
      'OUTPUT',
      'OVER',
      'OVERLAPS',
      'PARAMETER',
      'PARTITION',
      'PRECISION',
      'PREPARE',
      'PRIMARY',
      'PROCEDURE',
      'RANGE',
      'READS',
      'REAL',
      'RECURSIVE', 'REF',
      'REFERENCES',
      'REFERENCING',
      'RELEASE', 'REPEAT', 'REGIONAL',
      'RESULT',
      'RETURN', 'RETURNS', 'REVOKE', 'RIGHT',
      'ROLLBACK',
      'ROLLUP',
      'ROW',
      'ROWS',
      'SAVEPOINT',
      'SCOPE',
      'SCROLL',
      'SEARCH',
      'SECOND',
      'SELECT',
      'SENSITIVE',
      'SESSION_USER', 'SET',
      'SIGNALS', 'SIMILAR',
      'SMALLINT', 'SOME',
      'SPECIFIC',
      'SPECIFICTYPE',
      'SQL',
      'SQLEXCEPTION', 'SQLSTATE', 'SQLWARNING',
      'START',
      'STATIC',
      'SUBMULTISET',
      'SYMMETRIC', 'SYSTEM',
      'SYSTEM_USER',
      'TABLE',
      'TABLESAMPLE',
      'THEN', 'TIME', 'TIMESTAMP', 'TIMEZONE_HOUR',
      'TIMEZONE_MINUTE', 'TO', 'TRAILING',
      'TRANSLATION',
      'TREAT', 'TRIGGER',
      'TRUE',
      'UNDO', 'UNION', 'UNIQUE', 'UNKNOWN',
      'UNNEST', 'UNTIL',
      'UPDATE',
      'USER', 'USING',
      'VALUE', 'VALUES',
      'VARCHAR', 'VARYING',
      'VIEW',
      'WHEN', 'WHENEVER', 'WHERE',
      'WHILE',
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
