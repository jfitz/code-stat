import string

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StuffedQuoteStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  PrefixedIntegerTokenBuilder,
  LeadCommentTokenBuilder
)
from basic_token_builders import (
  RemarkTokenBuilder
)
from cbasic_token_builders import (
  CBasicVariableTokenBuilder,
  CBasicSuffixedIntegerTokenBuilder,
  CBasicLineContinuationTokenBuilder
)
from examiner import Examiner

class CBasicExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StuffedQuoteStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    LeadCommentTokenBuilder.__escape_z__()
    RemarkTokenBuilder.__escape_z__()
    CBasicVariableTokenBuilder.__escape_z__()
    CBasicSuffixedIntegerTokenBuilder.__escape_z__()
    CBasicLineContinuationTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    line_continuation_tb = CBasicLineContinuationTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    hex_constant_tb = CBasicSuffixedIntegerTokenBuilder('0123456789ABCDEF', 'H')
    binary_constant_tb = CBasicSuffixedIntegerTokenBuilder('01', 'B')
    variable_tb = CBasicVariableTokenBuilder('%$')
    quotes = ['"']
    string_tb = StuffedQuoteStringTokenBuilder(quotes, False)
    remark_tb = RemarkTokenBuilder()
    comment_tb = LeadCommentTokenBuilder("'")
    comment2_tb = LeadCommentTokenBuilder("’")

    stmt_separator_tb = SingleCharacterTokenBuilder(':', 'statement separator')

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
    group_ends = [')']

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
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      hex_constant_tb,
      binary_constant_tb,
      keyword_tb,
      known_operator_tb,
      function_tb,
      variable_tb,
      groupers_tb,
      string_tb,
      remark_tb,
      comment_tb,
      comment2_tb,
      directive_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.convert_numbers_to_line_numbers()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_line_format_confidence()
    self.calc_statistics()


  def convert_numbers_to_line_numbers(self):
    prev_token = Token('\n', 'newline')

    prev_line_continuation = False
    line_continuation = False

    for token in self.tokens:
      if token.group == 'number' and\
        prev_token.group == 'newline' and\
        not prev_line_continuation:
        token.group = 'line number'

      if token.group == 'line continuation':
        line_continuation = True

      if token.group not in ['whitespace', 'comment']:
        prev_token = token

      if token.group == 'newline':
        prev_line_continuation = line_continuation
        line_continuation = False
