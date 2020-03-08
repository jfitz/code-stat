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
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  ListTokenBuilder,
  LeadCommentTokenBuilder,
  SingleCharacterTokenBuilder
)
from lua_token_builders import (
  LuaBlockCommentTokenBuilder,
  DoubleBracketStringTokenBuilder
)
from examiner import Examiner

class LuaExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    LeadCommentTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LuaBlockCommentTokenBuilder.__escape_z__()
    DoubleBracketStringTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()
    self.newlines_important = 'parens'

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', False, '0123456789abcdefABCDEF')
    binary_integer_tb = PrefixedIntegerTokenBuilder('0b', False, '01')
    real_tb = RealTokenBuilder(False, False, "'")
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")

    leads = '_'
    extras = '_'
    suffixes = ''
    identifier_tb = IdentifierTokenBuilder(leads, extras, suffixes)

    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, False)
    bracket_string_tb = DoubleBracketStringTokenBuilder()

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

    known_operators = [
      '+', '-', '*', '/', '^',
      '<', '>', '<=', '>=', '==', '~=',
      '=', '..', '.', '#', ':',
      'and', 'not', 'or'
    ]

    self.unary_operators = [
      '+', '-',
      '#',
      'not'
    ]

    self.postfix_operators = []

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    # group_starts = ['(', '[', ',', '{']
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'break', 'do', 'else', 'elseif', 'end',
      'for', 'function', 'if', 'in',
      'local', 'repeat', 'return', 'then', 'until', 'while'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    values = [
      'false', 'true', 'nil', '...'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    line_comment_tb = LeadCommentTokenBuilder('--')
    block_comment_tb = LuaBlockCommentTokenBuilder()

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      hex_integer_tb,
      binary_integer_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      string_tb,
      bracket_string_tb,
      line_comment_tb,
      block_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    tokens = self.source_tokens()
    
    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence(tokens)
    self.calc_operator_3_confidence(tokens, group_ends)
    # self.calc_operator_4_confidence(tokens, group_starts)
    operand_types = ['number', 'identifier']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
