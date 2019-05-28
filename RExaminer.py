import string
import math
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  ListTokenBuilder,
  LeadCommentTokenBuilder
)
from RTokenBuilders import (
  ROperatorTokenBuilder
)
from Tokenizer import Tokenizer

class RExaminer(Examiner):
  def __init__(self, code):
    super().__init__()
    self.newlines_important = 'parens'

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(True)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False, False)

    hash_comment_tb = LeadCommentTokenBuilder('#')

    known_operators = [
        '+', '-', '*', '/', '**', '^',
        '%%', '%/%', '%*%', '%in%',
        '<', '<=', '>', '>=',
        '==', '!=', '!', '|', '&', '||', '&&',
        'isTRUE',
        '.', ':', '{', '}', '[[', ']]', '@', '$',
        '=', '<-', '<<-', '->', '->>'
      ]

    self.unary_operators = [
      '+', '-',
      '!', '@'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    stmt_separator_tb = ListTokenBuilder([';'], 'statement separator', False)

    user_operator_tb = ROperatorTokenBuilder()

    groupers = ['(', ')', ',', '[', ']']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'if', 'else', 'repeat', 'while',
      'function', 'for', 'in', 'next', 'break',
      'library', 'print', 'lapply', 'rep', 'list', 'matrix',
      'colnames', 'rownames', 'cbind', 'dim'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    values = [
      'TRUE', 'FALSE', 'NULL', 'Inf', 'NaN', 'NA',
      'NA_integer_', 'NA_real_', 'NA_complex_', 'NA_character_'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      values_tb,
      user_operator_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      string_tb,
      hash_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence()
    self.calc_operand_confidence()
    # self.calc_value_value_confidence()
    # self.calc_value_value_different_confidence()
    self.calc_keyword_confidence()
    self.calc_statistics()
