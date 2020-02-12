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
  SingleCharacterTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder,
  ClassTypeTokenBuilder
)
from examiner import Examiner

class DartExaminer(Examiner):
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
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', False, '0123456789abcdefABCDEF')
    binary_integer_tb = PrefixedIntegerTokenBuilder('0b', False, '01')
    real_tb = RealTokenBuilder(False, False, "'")
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")
    identifier_tb = IdentifierTokenBuilder()
    annotation_tb = PrefixedIdentifierTokenBuilder('@', 'annotation')
    symbol_tb = PrefixedIdentifierTokenBuilder('#', 'symbol')
    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, False)
    raw_string_tb = PrefixedStringTokenBuilder('r', True, quotes)

    class_type_tb = ClassTypeTokenBuilder()

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation')
    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

    known_operators = [
      '+', '-', '*', '/', '~/', '%', '^',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '~', '<<', '>>',
      '~/=', '||', '&&',
      '.', '..', ':', '?', '??', '??=',
      'as', 'is', 'is!',
      '++', '--',
      'new'
    ]

    self.unary_operators = [
      '+', '-', '*',
      '!', '~', '.', '..'
      '?.',
      '++', '--',
      'new'
    ]

    self.postfix_operators = [
      '++', '--'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'abstract', 'assert', 'async', 'await',
      'break',
      'case', 'catch', 'class', 'const', 'continue', 'covariant',
      'default', 'deferred', 'do', 'dynamic',
      'else', 'enum', 'export', 'extends', 'external',
      'factory', 'final', 'finally', 'for', 'Function',
      'get',
      'hide',
      'if', 'implements', 'import', 'in', 'interface',
      'library',
      'mixin',
      'on', 'operator',
      'part',
      'rethrow', 'return',
      'set', 'show', 'static', 'switch', 'sync',
      'throw', 'try', 'typedef',
      'var', 'void',
      'while', 'with',
      'yield'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'int', 'double', 'String', 'List', 'bool', 'void'
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'false', 'true', 'null', 'this', 'super'
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
      keyword_tb,
      types_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      annotation_tb,
      symbol_tb,
      class_type_tb,
      string_tb,
      raw_string_tb,
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
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
