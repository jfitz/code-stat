import string
import math

from codestat_tokenizer import Tokenizer
from codestat_token import Token
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  PrefixedIdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder,
  ClassTypeTokenBuilder
)
from examiner import Examiner

class JavaExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    PrefixedIdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    SlashStarCommentTokenBuilder.__escape_z__()
    ClassTypeTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    real_tb = RealTokenBuilder(False, False, '_')
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', '_')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)

    decorator_tb = PrefixedIdentifierTokenBuilder('@', 'decorator', False)
    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, False)

    class_type_tb = ClassTypeTokenBuilder()

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '~', '<<', '>>', '>>>', '>>>=',
      '^',
      '.', '::',
      '++', '--', '&&', '||',
      '?', '->',
      'new'
    ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-',
      '!', '~',
      '++', '--',
      'new'
    ]

    self.postfix_operators = [
      '++', '--'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':']
    group_starts = ['(', '[', ',', '{']
    group_ends = [')', ']', '}']
    group_mids = [',', ':']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'abstract', 'assert', 'break',
      'case', 'catch', 'class', 'const',
      'continue', 'default', 'do',
      'else', 'enum', 'extends', 'final',
      'finally', 'for', 'goto',
      'if', 'implements', 'import', 'instanceof', 'interface',
      'native', 'package',
      'private', 'protected', 'public',
      'return',
      'static', 'strictfp', 'super', 'switch', 'synchronized',
      'throw', 'throws', 'transient', 'try',
      'volatile', 'while'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    types = [
      'boolean', 'byte', 'char', 'double', 'float', 'int', 'long',
      'short', 'string', 'void',
      'Integer', 'String', 'StringBuilder',
      'File', 'Exception', 'IOException'
    ]

    types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)

    values = [
      'false', 'null', 'this', 'true'
    ]

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
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
      class_type_tb,
      decorator_tb,
      string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
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

    self.convert_keywords_to_identifiers(['::', '.'])
    self.convert_operators_to_identifiers(['::', '.'])

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_operator_confidence()

    allow_pairs = []

    self.calc_operator_2_confidence(tokens, allow_pairs)
    self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
    self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)
    self.calc_group_confidence(tokens, group_mids)
    operand_types = ['number', 'string', 'symbol']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()


  def convert_operators_to_identifiers(self, key_operators):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'operator' and\
        prev_token.group == 'operator' and prev_token.text in key_operators:
        token.group = 'identifier'

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token
