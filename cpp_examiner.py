import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  PrefixedIntegerTokenBuilder,
  SuffixedIntegerTokenBuilder,
  SuffixedRealTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder,
  ClassTypeTokenBuilder
)
from examiner import Examiner

class CppExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    SuffixedIntegerTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    SuffixedRealTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    SlashStarCommentTokenBuilder.__escape_z__()
    ClassTypeTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', False, '0123456789abcdefABCDEF')
    binary_integer_tb = PrefixedIntegerTokenBuilder('0b', False, '01')
    suffixed_integer_tb = SuffixedIntegerTokenBuilder(['U', 'L', 'LL', 'ULL', 'LLU'], False, None)
    real_tb = RealTokenBuilder(False, False, "'")
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")
    suffixed_real_tb = SuffixedRealTokenBuilder(False, False, ['f', 'l'], False, None)

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)

    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, False)

    class_type_tb = ClassTypeTokenBuilder()

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    directives = [
      '#define', '#undef',
      '#ifdef', '#ifndef', '#if', '#endif', '#else', '#elif',
      '#line', '#include', '#pragma'
    ]

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation', False)
    c_preprocessor_tb = ListTokenBuilder(directives, 'preprocessor', False, True)
    c_warning_tb = LeadToEndOfLineTokenBuilder('#warning', True, 'preprocessor')
    c_error_tb = LeadToEndOfLineTokenBuilder('#error', True, 'preprocessor')
    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '~', '<<', '>>',
      '^',
      '.',
      '++', '--', '->', '&&', '||',
      '?', '##',
      '::', '<=>', '.*', '->*',
      'new', 'delete', 'and', 'and_eq', 'bitand', 'bitor', 'compl',
      'not', 'not_eq', 'or', 'or_eq', 'xor', 'xor_eq'
    ]

    self.unary_operators = [
      '+', '-', '*',
      '!', '&', '~',
      '++', '--',
      'new', 'delete', 'compl', 'not'
    ]

    self.postfix_operators = [
      '++', '--', '&', '*'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':']
    group_starts = ['(', '[', ',', '{']
    group_ends = [')', ']', '}']
    group_mids = [',', ':']

    groupers_tb = ListTokenBuilder(groupers, 'group', False, False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', False, True)

    keywords = [
      'alignas', 'alignof', 'asm', 'atomic_cancel', 'atomic_commit', 'atomic_noexcept',
      'audit', 'auto', 'axiom',
      'break',
      'case', 'catch', 'class', 'concept', 'const', 'consteval',
      'constexpr', 'const_cast', 'continue', 'co_await', 'co_return',
      'co_yield',
      'decltype', 'default', 'do', 'dynamic_cast',
      'else', 'enum', 'explicit', 'export', 'extern',
      'final', 'for', 'friend',
      'goto',
      'if', 'import', 'inline',
      'module', 'mutable',
      'namespace', 'noexcept', 'nullptr',
      'operator', 'override',
      'private', 'protected', 'public', 'private:', 'protected:', 'public:',
      'reflexpr', 'register', 'reinterpret_cast', 'requires', 'return',
      'signed', 'sizeof', 'static', 'static_assert', 'static_cast', 'struct',
      'switch', 'synchronized',
      'template', 'thread_local', 'throw', 'transaction_safe', 'transaction_safe_dynamic'
      'try', 'typedef', 'typeid', 'typename',
      'union', 'unsigned', 'using',
      'virtual', 'volatile',
      'while',
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False, True)

    types = [
      'bool', 'char', 'char8_t', 'char16_t', 'char32_t', 'double', 'float', 'int',
      'long', 'short', 'void', 'wchar_t'
    ]

    types_tb = ListTokenBuilder(types, 'type', True, True)

    values = [
      'false', 'this', 'true', 'cout', 'cin'
    ]

    values_tb = ListTokenBuilder(values, 'value', True, True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      hex_integer_tb,
      binary_integer_tb,
      suffixed_integer_tb,
      real_tb,
      real_exponent_tb,
      suffixed_real_tb,
      keyword_tb,
      types_tb,
      values_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      class_type_tb,
      string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      c_preprocessor_tb,
      c_error_tb,
      c_warning_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['statement terminator', 'newline'], ['{'], ['whitespace', 'comment'])
    self.tokens = tokens
    self.convert_identifiers_to_labels()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence(['*', ';'])
    self.calc_operator_confidence()

    allow_pairs = []

    self.calc_operator_2_confidence(tokens, allow_pairs)
    self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
    self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)
    self.calc_group_confidence(tokens, group_mids)
    operand_types = ['number', 'string']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
