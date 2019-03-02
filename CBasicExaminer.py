import string
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
  ListTokenBuilder,
  PrefixedIntegerTokenBuilder,
  LeadCommentTokenBuilder
)
from BasicTokenBuilders import (
  BasicSuffixedIntegerTokenBuilder,
  BasicSuffixedRealTokenBuilder,
  RemarkTokenBuilder,
  LineNumberTokenBuilder
)
from CBasicTokenBuilders import (
  CBasicVariableTokenBuilder,
  CBasicSuffixedIntegerTokenBuilder,
  CBasicLineContinuationTokenBuilder
)
from Tokenizer import Tokenizer

class CBasicExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    line_continuation_tb = CBasicLineContinuationTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    hex_constant_tb = CBasicSuffixedIntegerTokenBuilder('0123456789ABCDEF', 'H')
    binary_constant_tb = CBasicSuffixedIntegerTokenBuilder('01', 'B')
    variable_tb = CBasicVariableTokenBuilder('%$')
    string_tb = StringTokenBuilder(['"'], True, False)
    remark_tb = RemarkTokenBuilder()
    comment_tb = LeadCommentTokenBuilder("'")
    line_number_tb = LineNumberTokenBuilder()

    stmt_separator_tb = ListTokenBuilder([':'], 'statement separator', False)

    known_operators = [
      '+', '-', '*', '/', '^',
      '=', '>', '>=', '<', '<=', '<>',
      '#', 'NOT',
      'AND', 'EQ', 'GE', 'GT', 'LE', 'LT', 'NE', 'OR', 'XOR'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-', '#', 'NOT'
    ]

    groupers = ['(', ')', ',', ';']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'AS', 'BUFF', 'CALL',  'CHAIN', 'CLOSE', 'COMMON', 'CONSOLE', 'CREATE',
      'DATA', 'DEF', 'DELETE', 'DIM', 'ELSE', 'END', 'FEND',
      'FILE', 'FOR', 'GOSUB', 'GO', 'GOTO', 'IF', 'INITIALIZE',
      'INPUT', 'INTEGER', 'LET', 'LINE', 'LPRINTER', 'NEXT', 
      'ON', 'OPEN', 'OUT', 'POKE', 'PRINT',
      'RANDOMIZE', 'READ', 'REM', 'REMARK', 'RENAME',
      'RESTORE', 'RETURN', 'SAVEMEM', 'STEP', 'STOP', 'SUB',
      'THEN', 'TO', 'USING', 'WEND', 'WHILE', 'WIDTH',
      'GRAPHIC', 'MAT', 'FILL', 'MAT', 'MARKER', 'PLOT', 'CHARACTER',
      'HEIGHT', 'SET', 'ASK', 'COLOR', 'COUNT', 'JUSTIFY', 'LINE', 'STYLE',
      'TYPE', 'TEXT', 'ANGLE', 'BOUNDS', 'DEVICE', 'VIEWPORT', 'WINDOW',
      'BEAM', 'CLEAR', 'CLIP', 'POSITION'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    functions = [
      'ASC', 'CHR$', 'STR$', 'TAB', 'COMMAND$', 'CONCHAR%', 'CONSTAT%',
      'ATN', 'COS', 'SIN', 'TAN',
      'ABS', 'EXP', 'INT', 'FLOAT', 'LOG', 'RND', 'SGN', 'SQR',
      'LEFT$', 'LEN', 'MID$', 'RIGHT$', 'MATCH', 'VAL',
      'FRE', 'INP', 'INT%', 'PEEK', 'POS', 'TAB',
      'RECL', 'RECS', 'SADD', 'SIZE', 'UCASE$', 'VARPTR'
    ]

    function_tb = ListTokenBuilder(functions, 'function', False)

    directives = [
      '%LIST', '%NOLIST',
      '%PAGE', '%EJECT',
      '%INCLUDE', '%CHAIN'
    ]

    directive_tb = ListTokenBuilder(directives, 'directive', False)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      stmt_separator_tb,
      line_continuation_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      hex_constant_tb,
      binary_constant_tb,
      line_number_tb,
      string_tb,
      known_operator_tb,
      groupers_tb,
      keyword_tb,
      function_tb,
      directive_tb,
      variable_tb,
      remark_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # self.calc_operand_confidence()
    self.calc_keyword_confidence()
    self.calc_line_format_confidence()