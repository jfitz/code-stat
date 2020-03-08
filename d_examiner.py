import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  PrefixedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  PrefixedIntegerTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  PrefixedIdentifierTokenBuilder,
  ListTokenBuilder,
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
    StringTokenBuilder.__escape_z__()
    PrefixedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    PrefixedIdentifierTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    SlashStarCommentTokenBuilder.__escape_z__()
    ClassTypeTokenBuilder.__escape_z__()
    HexRealExponentTokenBuilder.__escape_z__()
    NestedCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    # integer suffix: L u U Lu LU uL UL
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', False, '0123456789abcdefABCDEF_')
    binary_integer_tb = PrefixedIntegerTokenBuilder('0b', False, '01_')
    real_tb = RealTokenBuilder(False, False, "'")
    # real suffix f F L i
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")
    hex_real_tb = HexRealExponentTokenBuilder()

    leads = '_'
    extras = '_'
    suffixes = ''
    identifier_tb = IdentifierTokenBuilder(leads, extras, suffixes)

    attribute_tb = PrefixedIdentifierTokenBuilder('@', 'attribute')
    # string suffix: c,w,d
    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, False)
    r_string_tb = PrefixedStringTokenBuilder('r', True, quotes)
    backtick_string_tb = StringTokenBuilder(['`'], False)
    x_string_tb = PrefixedStringTokenBuilder('x', True, quotes)
    q_string_tb = PrefixedStringTokenBuilder('q', True, quotes)
    # q{} string

    class_type_tb = ClassTypeTokenBuilder()

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()
    slash_plus_comment_tb = NestedCommentTokenBuilder('/+', '+/')

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation')
    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

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
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

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

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'bool', 'byte', 'cdouble', 'cent', 'cfloat', 'char', 'creal',
      'dchar', 'double', 'float', 'idouble', 'ifloat', 'int', 'ireal',
      'long', 'real', 'short', 'ubyte', 'ucent', 'uint', 'ulong', 'ushort',
      'void', 'wchar'
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'false', 'null', 'super', 'this', 'true',
      '__FILE__', '__FILE_FULL_PATH__', '__MODULE__', '__LINE__',
      '__FUNCTION__', '__PRETTY_FUNCTION__',
      '__DATE__', '__EOF__', '__TIME__','__TIMESTAMP__',
      '__VENDOR__', '__VERSION__'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

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
      real_tb,
      real_exponent_tb,
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
      slash_slash_comment_tb,
      slash_star_comment_tb,
      slash_plus_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    number_suffixes = ['f', 'F', 'i', 'I', 'u', 'U', 'l', 'L', 'ul', 'uL', 'Ul', 'UL', 'lu', 'lU', 'Lu', 'LU']
    tokens = self.combine_tokens_and_adjacent_types(tokens, 'number', 'identifier', number_suffixes)

    string_suffixes = ['c', 'w', 'd']
    self.tokens = self.combine_tokens_and_adjacent_types(tokens, 'string', 'identifier', string_suffixes)

    tokens = self.source_tokens()
    
    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence(tokens)
    self.calc_operator_3_confidence(tokens, group_ends)
    self.calc_operator_4_confidence(tokens, group_starts)
    operand_types = ['number', 'symbol']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
