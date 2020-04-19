import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  IdentifierTokenBuilder,
  StringTokenBuilder,
  ListTokenBuilder,
  TripleQuoteStringTokenBuilder,
  LeadToEndOfLineTokenBuilder,
  BlockTokenBuilder
)
from pascal_token_builders import (
  BraceCommentTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder
)
from generic_token_builders import (
  GenericNumberTokenBuilder
)
from examiner import Examiner

class GenericCodeExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    StringTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    TripleQuoteStringTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    BlockTokenBuilder.__escape_z__()
    BraceCommentTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    SlashStarCommentTokenBuilder.__escape_z__()
    GenericNumberTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, comment):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    number_tb = GenericNumberTokenBuilder()
    operand_types.append('number')

    leads = ''
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    quotes = ['"', "'", "’", '`']
    string_tb = StringTokenBuilder(quotes, False)
    triple_string_tb = TripleQuoteStringTokenBuilder(quotes)
    operand_types.append('string')

    comment_tbs = []

    if comment == 'ada':
      comment_tbs = [LeadToEndOfLineTokenBuilder('--', True, 'comment')]
    if comment == 'hash':
      comment_tbs = [LeadToEndOfLineTokenBuilder('#', True, 'comment')]
    if comment == 'bang':
      comment_tbs = [LeadToEndOfLineTokenBuilder('!', True, 'comment')]
    if comment == 'cobol-inline':
      comment_tbs = [LeadToEndOfLineTokenBuilder('*>', True, 'comment')]
    if comment == 'percent':
      comment_tbs = [LeadToEndOfLineTokenBuilder('%', True, 'comment')]
    if comment == 'cobol':
      pass
    if comment == 'fortran':
      pass
    if comment == 'basic':
      comment_tbs = [
        LeadToEndOfLineTokenBuilder("REM", False, 'comment'),
        LeadToEndOfLineTokenBuilder("'", True, 'comment')
      ]
    if comment == 'c':
      comment_tbs = [SlashStarCommentTokenBuilder()]
    if comment == 'cpp':
      comment_tbs = [
        SlashSlashCommentTokenBuilder(),
        SlashStarCommentTokenBuilder()
      ]
    if comment == 'pascal':
      comment_tbs = [
        BraceCommentTokenBuilder(),
        BlockTokenBuilder('(*', '*)', 'comment')
      ]

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '~', '<<', '>>',
      '^',
      '.', '..', '...', ':',
      '++', '--', '->', '&&', '||',
      '?', '##',
      '\\', '_', '@', '#', '$', '`', '```'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}', ';']
    # group_starts = ['(', '[', ',', '{']
    # group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False, False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', False, True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders1 = [
        whitespace_tb,
        newline_tb,
        number_tb,
        groupers_tb,
        known_operator_tb,
        identifier_tb,
        string_tb,
        triple_string_tb
    ]

    tokenbuilders2 = [
        self.unknown_operator_tb,
        invalid_token_builder
      ]

    tokenbuilders = tokenbuilders1 + comment_tbs + tokenbuilders2

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['newline', 'statement separator'], ['{'], ['whitespace', 'comment'])
    self.tokens = tokens
    self.convert_identifiers_to_labels()

    # tokens = self.source_tokens()
    # tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_operator_confidence()

    # allow_pairs = []

    # self.calc_operator_2_confidence(tokens, allow_pairs)
    # self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
    # self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)
    # self.calc_operand_confidence(tokens, operand_types)
    # self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
