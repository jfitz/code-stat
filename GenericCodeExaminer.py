import string
import math
from Token import Token
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  ListTokenBuilder,
  TripleQuoteStringTokenBuilder,
  LeadCommentTokenBuilder,
  ParenStarCommentTokenBuilder
)
from PascalTokenBuilders import (
  BraceCommentTokenBuilder
)
from CXTokenBuilders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder
)
from GenericTokenBuilders import (
  GenericIdentifierTokenBuilder,
  GenericNumberTokenBuilder
)
from Tokenizer import Tokenizer

class GenericCodeExaminer(Examiner):
  def __init__(self, code, comment):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    number_tb = GenericNumberTokenBuilder()
    identifier_tb = GenericIdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False, False, False)
    triple_string_tb = TripleQuoteStringTokenBuilder()

    comment_tbs = []

    if comment == 'ada':
      comment_tbs = [LeadCommentTokenBuilder('--')]
    if comment == 'hash':
      comment_tbs = [LeadCommentTokenBuilder('#')]
    if comment == 'bang':
      comment_tbs = [LeadCommentTokenBuilder('!')]
    if comment == 'cobol-inline':
      comment_tbs = [LeadCommentTokenBuilder('*>')]
    if comment == 'percent':
      comment_tbs = [LeadCommentTokenBuilder('%')]
    if comment == 'cobol':
      pass
    if comment == 'fortran':
      pass
    if comment == 'basic':
      comment_tbs = [
        LeadCommentTokenBuilder("REM"),
        LeadCommentTokenBuilder("'")
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
        ParenStarCommentTokenBuilder()
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
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

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
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    # self.calc_operator_2_confidence()
    # self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'symbol']
    # self.calc_operand_confidence(operand_types)
    # self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
