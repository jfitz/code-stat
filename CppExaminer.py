import string
import math
from Token import Token
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
  ListTokenBuilder
)
from CXTokenBuilders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder,
  CPreProcessorTokenBuilder
)
from Tokenizer import Tokenizer

class CppExaminer(Examiner):
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

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    directives = (
      '#define', '#undef',
      '#ifdef', '#ifndef', '#if', '#endif', '#else', '#elif',
      '#line', '#error', '#include', '#pragma'
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
      '?',
      '::', '<=>', '.*', '->*',
      'new', 'delete', 'and', 'and_eq', 'bitand', 'bitor', 'compl',
      'not', 'not_eq', 'or', 'or_eq', 'xor', 'xor_eq'
    ]

    self.unary_operators = [
      '+', '-', '*',
      '!', '&', '~',
      '++', '--', ':',
      'new', 'delete', 'compl', 'not'
    ]

    self.postfix_operators = [
      '++', '--', '&', ':'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'alignas', 'alignof', 'asm', 'atomic_cancel',
      'atomic_commit', 'atomic_noexcept', 'auto',
      'break', 'case', 'catch',
      'class', 'concept', 'const', 'consteval',
      'constexpr', 'const_cast', 'continue', 'co_await', 'co_return',
      'co_yield', 'decltype', 'default', 'do',
      'dynamic_cast', 'else', 'enum', 'explicit', 'export', 'extern',
      'for', 'friend', 'goto', 'if', 'import',
      'inline', 'module', 'mutable', 'namespace',
      'noexcept', 'nullptr', 'operator',
      'private', 'protected', 'public',
      'private:', 'protected:', 'public:',
      'reflexpr', 'register',
      'reinterpret_cast', 'requires', 'return', 'signed',
      'sizeof', 'static', 'static_assert', 'static_cast', 'struct',
      'switch', 'synchronized', 'template', 'thread_local',
      'throw', 'try', 'typedef', 'typeid', 'typename', 'union',
      'unsigned', 'using', 'virtual', 'volatile',
      'while',
      'override', 'axiom', 'final', 'audit', 'transaction_safe',
      'transaction_safe_dynamic'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'bool', 'char', 'char8_t', 'char16_t', 'char32_t', 'double', 'float', 'int',
      'long', 'short', 'void', 'wchar_t'
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'false', 'this', 'true', 'cout', 'cin'
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
