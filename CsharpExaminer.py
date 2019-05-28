import string
import math
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  PrefixedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  ListTokenBuilder
)
from CXTokenBuilders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder,
  CPreProcessorTokenBuilder
)
from Tokenizer import Tokenizer

class CsharpExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False, False)
    prefixed_string_tb = PrefixedStringTokenBuilder('@', False, ['"'])

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    directives = (
      '#if', '#else', '#elif', '#endif',
      '#define', '#undef', '#warning', '#error'
      '#line', '#region', '#endregion', '#pragma'
    )

    c_preprocessor_tb = CPreProcessorTokenBuilder(directives)

    terminators_tb = ListTokenBuilder([';'], 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '~', '<<', '>>',
      '^',
      '.', ':',
      '++', '--', '->', '&&', '||',
      '?', '??', '?.', '?[',
      '=>',
      'as', 'is', 'await', 'sizeof',
      'typeof', 'new'
    ]
    
    self.unary_operators = [
      '+', '-',
      '!', '~',
      '++', '--',
      'new', 'sizeof', 'typeof'
    ]

    self.postfix_operators = [
      '++', '--', ':'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    groupers = ['(', ')', ',', '[', ']', '{', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'abstract', 'break',
      'case', 'catch', 'checked', 'class', 'const',
      'continue', 'default', 'delegate', 'do',
      'else', 'enum', 'event', 'explicit', 'extern',
      'finally', 'fixed', 'for', 'foreach', 'goto',
      'if', 'implicit', 'in', 'interface', 'internal',
      'lock', 'namespace', 'operator',
      'out', 'override', 'params', 'private', 'protected', 'public',
      'readonly', 'ref', 'return', 'sealed',
      'stackalloc', 'static', 'struct', 'switch',
      'throw', 'try',
      'unchecked', 'unsafe', 'using', 'using static',
      'virtual', 'volatile', 'while'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'bool', 'byte', 'char', 'decimal', 'double', 'float', 'int', 'long', 'object',
      'sbyte', 'short', 'string', 'uint', 'ulong', 'ushort', 'void'
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'base', 'false', 'null', 'this', 'true'
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
      keyword_tb,
      types_tb,
      values_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      string_tb,
      prefixed_string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      c_preprocessor_tb,
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
