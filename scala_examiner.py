import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  EscapedStringTokenBuilder,
  TripleQuoteStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  SuffixedIntegerTokenBuilder,
  PrefixedIntegerTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  SuffixedRealTokenBuilder,
  IdentifierTokenBuilder,
  PrefixedIdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder
)
from examiner import Examiner

class ScalaExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    EscapedStringTokenBuilder.__escape_z__()
    TripleQuoteStringTokenBuilder.__escape_z__()
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
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', False, '0123456789abcdefABCDEF_')
    long_integer_tb = SuffixedIntegerTokenBuilder('L', False, None)
    real_tb = RealTokenBuilder(False, False, "'")
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")
    float_real_tb = SuffixedRealTokenBuilder(False, False, ['f'], False, None)
    operand_types.append('number')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    symbol_tb = PrefixedIdentifierTokenBuilder("'", 'symbol', True)
    operand_types.append('symbol')

    quotes = ['"']
    string_tb = EscapedStringTokenBuilder(quotes, 0)
    triple_string_tb = TripleQuoteStringTokenBuilder(quotes)
    operand_types.append('string')

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation', False)
    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '/', '%', '&', '|', '^', '<<', '>>', '&&', '||',
      '=', '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '>:', '⇒', '=>', '=', '<%', '<:', '←', '<-', '#', '@',
      '==', '!=', '>', '<', '>=', '<=', '!', '~',
      '<<<', '>>>', '.',
      '++', '--',
      'new'
    ]

    self.unary_operators = [
      '+', '-', '*',
      '!', '~',
      '++', '--',
      'new'
    ]

    self.postfix_operators = [
      '++', '--'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':']
    group_starts = ['(', '[', ',', '{']
    group_mids = [',', ':']
    group_ends = [')', ']', '}']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'abstract',
      'case', 'catch', 'class',
      'def', 'do',
      'else', 'extends',
      'final', 'finally', 'for', 'forSome',
      'if', 'implicit', 'import',
      'lazy',
      'match',
      'object', 'override',
      'package', 'private', 'protected',
      'return',
      'sealed',
      'throw', 'trait', 'try', 'type',
      'val', 'var',
      'while', 'with',
      'yield'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    values = [
      'false', 'true', 'null', 'this', 'super'
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
      long_integer_tb,
      real_tb,
      real_exponent_tb,
      float_real_tb,
      keyword_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      symbol_tb,
      string_tb,
      triple_string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

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
