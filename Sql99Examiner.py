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

class Sql99Examiner(Examiner):
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
      'ABSOLUTE',
      'ACTION', 'ADD',
      'AFTER',
      'ALL', 'ALLOCATE', 'ALTER', 'AND', 'ANY', 'ARE',
      'ARRAY',
      'AS',
      'ASC',
      'ASENSITIVE',
      'ASYMMETRIC',
      'AT',
      'ATOMIC',
      'AUTHORIZATION',
      'BEFORE',
      'BEGIN',
      'BETWEEN',
      'BINARY',
      'BIT',
      'BLOB', 'BOOLEAN',
      'BOTH',
      'BREADTH',
      'BY',
      'CALL',
      'CASCADE', 'CASCADED', 'CASE', 'CAST',
      'CATALOG',
      'CHAR',
      'CHARACTER',
      'CHECK',
      'CLOB', 'CLOSE',
      'COLLATE',
      'COLLATION',
      'COLUMN', 'COMMIT', 'CONDITION', 'CONNECT',
      'CONNECTION',
      'CONSTRAINT',
      'CONSTRAINTS',
      'CONSTRUCTOR',
      'CONTINUE',
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
      'DATA',
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
      'DYNAMIC',
      'EACH',
      'ELSE',
      'ELSEIF',
      'END',
      'EQUALS',
      'ESCAPE',
      'EXCEPT',
      'EXCEPTION',
      'EXEC', 'EXECUTE', 'EXISTS',
      'EXIT',
      'EXTERNAL',
      'FALSE', 'FETCH',
      'FILTER',
      'FIRST',
      'FLOAT', 'FOR', 'FOREIGN',
      'FOUND',
      'FREE',
      'FROM', 'FULL', 'FUNCTION',
      'FUSION',
      'GENERAL',
      'GET', 'GLOBAL',
      'GO', 'GOTO',
      'GRANT', 'GROUP',
      'GROUPING',
      'HANDLER', 'HAVING',
      'HOLD',
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
      'ITERATE',
      'JOIN',
      'KEY',
      'LANGUAGE',
      'LARGE',
      'LAST',
      'LATERAL',
      'LEADING',
      'LEAVE',
      'LEFT', 'LEVEL', 'LIKE',
      'LIMIT',
      'LOCAL',
      'LOCALTIME', 'LOCALTIMESTAMP',
      'LOCATOR',
      'LOOP',
      'MAP',
      'MATCH',
      'METHOD',
      'MINUTE',
      'MODIFIES',
      'MODULE', 'MONTH',
      'NAMES',
      'NATIONAL', 'NATURAL', 'NCHAR',
      'NCLOB', 'NEW',
      'NEXT',
      'NO',
      'NONE',
      'NOT', 'NULL',
      'NUMERIC',
      'OBJECT',
      'OF',
      'OLD',
      'ON', 'ONLY', 'OPEN',
      'OPTION',
      'OR', 'ORDER',
      'ORDINALITY',
      'OUT', 'OUTER',
      'OUTPUT',
      'OVER',
      'OVERLAPS',
      'PAD',
      'PARAMETER',
      'PARTIAL',
      'PARTITION',
      'PATH',
      'PRECISION',
      'PREPARE',
      'PRESERVE',
      'PRIMARY',
      'PRIOR',
      'PRIVILEGES',
      'PROCEDURE',
      'PUBLIC',
      'READ',
      'READS',
      'REAL',
      'RECURSIVE', 'REF',
      'REFERENCES',
      'REFERENCING',
      'RELATIVE',
      'RELEASE', 'REPEAT', 'REGIONAL',
      'RESTRICT',
      'RESULT',
      'RETURN', 'RETURNS', 'REVOKE', 'RIGHT',
      'ROLE',
      'ROLLBACK',
      'ROLLUP',
      'ROUTINE',
      'ROW',
      'ROWS',
      'SAVEPOINT',
      'SCHEMA',
      'SCOPE',
      'SCROLL',
      'SEARCH',
      'SECOND',
      'SECTION',
      'SELECT',
      'SENSITIVE',
      'SESSION',
      'SESSION_USER', 'SET',
      'SETS',
      'SIGNALS', 'SIMILAR',
      'SIZE',
      'SMALLINT', 'SOME',
      'SPACE',
      'SPECIFIC',
      'SPECIFICTYPE',
      'SQL',
      'SQLEXCEPTION', 'SQLSTATE', 'SQLWARNING',
      'START',
      'STATE',
      'STATIC',
      'SYMMETRIC', 'SYSTEM',
      'SYSTEM_USER',
      'TABLE',
      'TEMPORARY',
      'THEN', 'TIME', 'TIMESTAMP', 'TIMEZONE_HOUR',
      'TIMEZONE_MINUTE', 'TO', 'TRAILING',
      'TRANSACTION',
      'TRANSLATION',
      'TREAT', 'TRIGGER',
      'TRUE',
      'UNDER',
      'UNDO', 'UNION', 'UNIQUE', 'UNKNOWN',
      'UNNEST', 'UNTIL',
      'UPDATE',
      'USAGE',
      'USER', 'USING',
      'VALUE', 'VALUES',
      'VARCHAR', 'VARYING',
      'VIEW',
      'WHEN', 'WHENEVER', 'WHERE',
      'WHILE',
      'WINDOW',
      'WITH',
      'WITHIN', 'WITHOUT',
      'WORK', 'WRITE',
      'YEAR',
      'ZONE'
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
