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
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadToEndOfLineTokenBuilder,
  BlockTokenBuilder
)
from haskell_token_builders import (
  HaskellIdentifierTokenBuilder,
  HaskellClassTokenBuilder,
  HaskellOperatorTokenBuilder
)
from examiner import Examiner

class HaskellExaminer(Examiner):
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
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    BlockTokenBuilder.__escape_z__()
    HaskellClassTokenBuilder.__escape_z__()
    HaskellIdentifierTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', False, '0123456789abcdefABCDEF')
    real_tb = RealTokenBuilder(False, False, "'")
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")
    wildcard_tb = SingleCharacterTokenBuilder('_', 'value')

    identifier_tb = HaskellIdentifierTokenBuilder()
    class_tb = HaskellClassTokenBuilder()

    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, False)

    line_comment_tb = LeadToEndOfLineTokenBuilder('--', False, 'comment')
    block_comment_tb = BlockTokenBuilder('{-', '-}', 'comment')

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation')

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':', '::']
    # group_starts = ['(', '[', ',', '{']
    # group_ends = [')', ']', '}']
    group_mids = [',', ':']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    operators_tb = HaskellOperatorTokenBuilder('#$%&*+./<=>?@\\^|-~')

    keywords = [
      'case', 'class',
      'data', 'deriving', 'do',
      'else',
      'if', 'import', 'in', 'infix', 'infix1', 'infixr', 'instance',
      'let',
      'module',
      'newtype',
      'of',
      'then', 'type',
      'where'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    values = ['True', 'False', 'Nothing']

    value_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      integer_tb,
      integer_exponent_tb,
      hex_integer_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      groupers_tb,
      operators_tb,
      wildcard_tb,
      identifier_tb,
      value_tb,
      class_tb,
      string_tb,
      line_comment_tb,
      block_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    # tokens = Examiner.combine_identifier_colon(tokens, ['statement terminator', 'newline'], ['{'], ['whitespace', 'comment'])
    HaskellExaminer.convert_keywords_to_identifiers(tokens)
    self.tokens = tokens
    # self.convert_identifiers_to_labels()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    # self.calc_token_2_confidence(['*', ';'])
    self.calc_operator_confidence()
    # self.calc_operator_2_confidence(tokens)
    # self.calc_operator_3_confidence(tokens, group_ends)
    # self.calc_operator_4_confidence(tokens, group_starts)
    self.calc_group_confidence(tokens, group_mids)
    # operand_types = ['number']
    # self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()


  @staticmethod
  def convert_keywords_to_identifiers(tokens):
    prev_token = Token('\n', 'newline')

    for token in tokens:
      if prev_token.text == 'type' and token.group != 'class':
        prev_token.group = 'identifier'

      prev_token = token
