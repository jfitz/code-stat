import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  IdentifierTokenBuilder,
  EscapedStringTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
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
    EscapedStringTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
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

    quotes = ['"', "'", '`']
    string_tb = EscapedStringTokenBuilder(quotes, 0)
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
    group_mids = [',', ';']
    # group_ends = [')', ']', '}']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

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

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['newline', 'statement separator'], ['{'], ['whitespace', 'comment', 'line description'])
    self.tokens = tokens
    self.convert_identifiers_to_labels()

    self.calc_statistics()

    # tokens = self.source_tokens()
    # tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      # allow_pairs = []
      # self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      # self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      # self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    # self.calc_operand_n_confidence(tokens, operand_types, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    # self.calc_keyword_confidence()

    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_length_confidence(code, self.max_expected_line)
