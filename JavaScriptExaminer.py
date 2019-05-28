import string
import math
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
  IdentifierTokenBuilder,
  ListTokenBuilder,
  PrefixedIntegerTokenBuilder,
  RegexTokenBuilder
)
from CXTokenBuilders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder
)
from Tokenizer import Tokenizer

class JavaScriptExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    hex_constant_tb = PrefixedIntegerTokenBuilder('0X', False, '0123456789ABCDEFabcdef')
    octal_constant_tb = PrefixedIntegerTokenBuilder('0O', False, '01234567')
    binary_constant_tb = PrefixedIntegerTokenBuilder('0B', False, '01')

    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False, False)

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    terminators_tb = ListTokenBuilder([';'], 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '===', '!==', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '**=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '~', '<<', '>>', '>>>', '>>>=',
      '^', '**',
      '.', ':',
      '++', '--', '&&', '||',
      '?'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-',
      '!', '~',
      '++', '--', ':'
    ]

    self.postfix_operators = [
      '++', '--', ':'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    regex_tb = RegexTokenBuilder()

    keywords = [
      'break', 'case', 'catch', 'class', 'const', 'continue',
      'debugger', 'default', 'delete', 'do', 'else', 'export',
      'extends', 'finally', 'for', 'function', 'if', 'import',
      'in', 'instanceof', 'new', 'return', 'switch',
      'throw', 'try', 'typeof', 'var', 'void', 'while',
      'with', 'yield',
      'abstract', 'final',
      'goto', 'native', 'synchronized',
      'throws', 'transient', 'volatile'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'boolean', 'byte', 'char', 'double', 'float', 'int', 'long', 'short'
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'this', 'super', 'null', 'true', 'false'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      hex_constant_tb,
      octal_constant_tb,
      binary_constant_tb,
      keyword_tb,
      types_tb,
      values_tb,
      known_operator_tb,
      groupers_tb,
      regex_tb,
      identifier_tb,
      string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
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
    # self.calc_value_value_confidence()
    # self.calc_value_value_different_confidence()
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
