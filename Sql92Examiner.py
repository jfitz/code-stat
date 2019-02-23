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

class Sql92Examiner(Examiner):
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
      'ABSOLUTE', 'ACTION', 'ADD', 'ALL', 'ALLOCATE', 'ALTER', 'AND', 'ANY',
      'ARE', 'AS', 'ASC', 'ASSERTION', 'AT', 'AUTHORIZATION', 'AVG', 'BEGIN',
      'BETWEEN', 'BIT', 'BIT_LENGTH', 'BOTH', 'BY', 'CALL', 'CASCADE',
      'CASCADED', 'CASE', 'CAST', 'CATALOG', 'CHAR', 'CHAR_LENGTH',
      'CHARACTER', 'CHARACTER_LENGTH', 'CHECK', 'COALESCE', 'COLLATE',
      'COLLATION', 'COLUMN', 'COMMIT', 'CONDITION', 'CONNECT', 'CONNECTION',
      'CONSTRAINT', 'CONSTRAINTS', 'CONTAINS', 'CONTINUE', 'CONVERT',
      'CORRESPONDING', 'COUNT', 'CREATE', 'CROSS', 'CURRENT', 'CURRENT_DATE',
      'CURRENT_PATH', 'CURRENT_TIME', 'CURRENT_TIMESTAMP', 'CURRENT_USER',
      'CURSOR', 'DATE', 'DAY', 'DEALLOCATE', 'DEC', 'DECIMAL', 'DECLARE',
      'DEFAULT', 'DEFERRABLE', 'DEFERRED', 'DELETE', 'DESC', 'DESCRIBE',
      'DESCRIPTOR', 'DETERMINISTIC', 'DIAGNOSTICS', 'DISCONNECT', 'DISTINCT',
      'DOMAIN', 'DOUBLE', 'DROP', 'ELSE', 'END', 'ESCAPE', 'EXCEPT',
      'EXCEPTION', 'EXEC', 'EXECUTE', 'EXISTS', 'EXIT', 'EXTERNAL', 'EXTRACT',
      'FALSE', 'FETCH', 'FIRST', 'FLOAT', 'FOR', 'FOREIGN', 'FOUND', 'FROM',
      'FULL', 'FUNCTION', 'FUSION', 'GET', 'GLOBAL', 'GO', 'GOTO', 'GRANT',
      'GROUP', 'HANDLER', 'HAVING', 'HOUR', 'IDENTITY', 'IMMEDIATE', 'IN',
      'INDICATOR', 'INITIALLY', 'INNER', 'INOUT', 'INPUT', 'INSENSITIVE',
      'INSERT', 'INT', 'INTEGER', 'INTERSECT', 'INTERVAL', 'INTO', 'IS',
      'ISOLATION', 'JOIN', 'KEY', 'LANGUAGE', 'LAST', 'LEADING', 'LEFT',
      'LEVEL', 'LIKE', 'LOCAL', 'LOWER', 'MATCH', 'MAX', 'MIN', 'MINUTE',
      'MODULE', 'MONTH', 'NAMES', 'NATIONAL', 'NATURAL', 'NCHAR',
      'NEXT', 'NO', 'NOT', 'NULL', 'NULLIF', 'NUMERIC', 'OCTET_LENGTH',
      'OF', 'ON', 'ONLY', 'OPEN', 'OPTION', 'OR', 'ORDER', 'OUT', 'OUTER',
      'OUTPUT', 'OVERLAPS', 'PAD', 'PARAMETER', 'PARTIAL', 'PATH',
      'POSITION', 'PRECISION', 'PREPARE', 'PRESERVE', 'PRIMARY', 'PRIOR',
      'PRIVILEGES', 'PROCEDURE', 'PUBLIC', 'RANGE', 'READ', 'REAL',
      'REFERENCES', 'RELATIVE', 'RESTRICT', 'RETURN', 'RETURNS', 'REVOKE',
      'RIGHT', 'ROLLBACK', 'ROUTINE', 'ROWS', 'SCHEMA', 'SCROLL', 'SECOND',
      'SECTION', 'SELECT', 'SESSION', 'SESSION_USER', 'SET', 'SIZE',
      'SMALLINT', 'SOME', 'SPACE', 'SPECIFIC', 'SQL', 'SQLCODE', 'SQLERROR',
      'SQLEXCEPTION', 'SQLSTATE', 'SQLWARNING', 'SUBSTRING', 'SUM',
      'SYSTEM_USER', 'TABLE', 'TEMPORARY', 'THEN', 'TIME', 'TIMESTAMP',
      'TIMEZONE_HOUR', 'TIMEZONE_MINUTE', 'TO', 'TRAILING', 'TRANSACTION',
      'TRANSLATE', 'TRANSLATION', 'TRIM', 'TRUE', 'UNDO', 'UNION', 'UNIQUE',
      'UNKNOWN', 'UPDATE', 'UPPER', 'USAGE', 'USER', 'USING', 'VALUE',
      'VALUES', 'VARCHAR', 'VARYING', 'VIEW', 'WHEN', 'WHENEVER', 'WHERE',
      'WITH', 'WORK', 'WRITE', 'YEAR', 'ZONE'
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
    # do not check for two operands in a row
