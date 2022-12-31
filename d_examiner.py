import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  EscapedStringTokenBuilder,
  PrefixedStringTokenBuilder,
  SuffixedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  PrefixedIntegerTokenBuilder,
  SuffixedIntegerTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  SuffixedRealTokenBuilder,
  IdentifierTokenBuilder,
  PrefixedIdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  NestedCommentTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder,
  ClassTypeTokenBuilder
)
from d_token_builders import (
  HexRealExponentTokenBuilder
)
from examiner import Examiner

class DExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    EscapedStringTokenBuilder.__escape_z__()
    PrefixedStringTokenBuilder.__escape_z__()
    SuffixedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    SuffixedIntegerTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    SuffixedRealTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    PrefixedIdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    SlashStarCommentTokenBuilder.__escape_z__()
    ClassTypeTokenBuilder.__escape_z__()
    HexRealExponentTokenBuilder.__escape_z__()
    NestedCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, block_comment_limit):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', False, '0123456789abcdefABCDEF_')
    binary_integer_tb = PrefixedIntegerTokenBuilder('0b', False, '01_')
    suffixed_integer_tb = SuffixedIntegerTokenBuilder(['U', 'L', 'LU', 'UL'], False, None)
    real_tb = RealTokenBuilder(False, False, "'")
    suffixed_real_tb = SuffixedRealTokenBuilder(False, False, ['f', 'l', 'i'], False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")
    hex_real_tb = HexRealExponentTokenBuilder()
    operand_types.append('number')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    attribute_tb = PrefixedIdentifierTokenBuilder('@', 'attribute', False)
    operand_types.append('attribute')

    # string suffix: c,w,d
    quotes = ['"', "'"]
    string_tb = EscapedStringTokenBuilder(quotes, 0)
    r_string_tb = PrefixedStringTokenBuilder('r', True, quotes)
    backtick_string_tb = EscapedStringTokenBuilder(['`'], 0)
    x_string_tb = PrefixedStringTokenBuilder('x', True, quotes)
    q_string_tb = PrefixedStringTokenBuilder('q', True, quotes)
    # q{} string
    cwd_string_tb = SuffixedStringTokenBuilder(quotes, 'cwd', False)
    operand_types.append('string')

    class_type_tb = ClassTypeTokenBuilder()
    operand_types.append('class')

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()
    slash_plus_comment_tb = NestedCommentTokenBuilder('/+', '+/', block_comment_limit)

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation', False)
    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    known_operators = [
      '/', '/=', '.', '..', '...', '&', '&=', '&&', '|', '|=', '||',
      '-', '-=', '--', '+', '+=', '++', '<', '<=', '<<', '<<=', '>', '>=',
      '>>=', '>>>=', '>>', '>>>', '!', '!=', '?', ',', ':', '$',
      '=', '==', '*', '*=', '%', '%=', '^', '^=', '^^', '^^=', '~', '~=',
      '@', '=>', '#',
      'new', 'delete',
      'typeof', 'is'
    ]

    self.unary_operators = [
      '+', '-', '*',
      '!', '&', '~',
      '++', '--', ':',
      'new', 'delete',
      'typeof', 'is'
    ]

    self.postfix_operators = [
      '++', '--', '&', ':'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_starts = ['(', '[', ',', '{']
    group_mids = [',']
    group_ends = [')', ']', '}']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'abstract', 'alias', 'align', 'asm', 'assert', 'auto',
      'body', 'break', 'case', 'cast', 'catch', 'class', 'const', 'continue',
      'debug', 'default', 'delegate', 'deprecated', 'do',
      'else', 'enum', 'export', 'extern',
      'final', 'finally', 'for', 'foreach', 'foreach_reverse', 'function',
      'goto',
      'if', 'immutable', 'import', 'in', 'inout', 'interface', 'invariant',
      'lazy',
      'macro', 'mixin', 'module',
      'nothrow',
      'out', 'override',
      'package', 'pragma', 'private', 'protected', 'public', 'pure',
      'ref', 'return',
      'scope', 'shared', 'static', 'struct', 'switch', 'synchronized',
      'template', 'throw', 'try', 'typeid',
      'union', 'unittest', 'version', 'while', 'with',
      '__gshared', '__traits', '__vector', '__parameters'
]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    types = [
      'bool', 'byte', 'cdouble', 'cent', 'cfloat', 'char', 'creal',
      'dchar', 'double', 'float', 'idouble', 'ifloat', 'int', 'ireal',
      'long', 'real', 'short', 'ubyte', 'ucent', 'uint', 'ulong', 'ushort',
      'void', 'wchar'
    ]

    types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    values = [
      'false', 'null', 'super', 'this', 'true',
      '__FILE__', '__FILE_FULL_PATH__', '__MODULE__', '__LINE__',
      '__FUNCTION__', '__PRETTY_FUNCTION__',
      '__DATE__', '__EOF__', '__TIME__','__TIMESTAMP__',
      '__VENDOR__', '__VERSION__'
    ]

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

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
      hex_real_tb,
      keyword_tb,
      types_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      attribute_tb,
      class_type_tb,
      string_tb,
      r_string_tb,
      x_string_tb,
      backtick_string_tb,
      q_string_tb,
      cwd_string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      slash_plus_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['statement terminator', 'newline'], ['{'], ['whitespace', 'comment'])
    self.tokens = tokens
    self.convert_identifiers_after_goto_to_labels()

    number_suffixes = ['f', 'F', 'i', 'I', 'u', 'U', 'l', 'L', 'ul', 'uL', 'Ul', 'UL', 'lu', 'lU', 'Lu', 'LU']
    tokens = self.combine_tokens_and_adjacent_types(tokens, 'number', 'identifier', number_suffixes)

    string_suffixes = ['c', 'w', 'd']
    self.tokens = self.combine_tokens_and_adjacent_types(tokens, 'string', 'identifier', string_suffixes)

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_length_confidence(code, self.max_expected_line)
