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
  TripleQuoteStringTokenBuilder
)
from GenericTokenBuilders import (
  GenericIdentifierTokenBuilder,
  GenericNumberTokenBuilder
)
from Tokenizer import Tokenizer

class GenericCodeExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    number_tb = GenericNumberTokenBuilder()
    identifier_tb = GenericIdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False, False)
    tripe_string_tb = TripleQuoteStringTokenBuilder()

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

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      number_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      string_tb,
      tripe_string_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

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
