import string
import math

from codestat_tokenizer import Tokenizer
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
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadCommentTokenBuilder
)
from r_token_builders import (
  ROperatorTokenBuilder
)
from examiner import Examiner

class RExaminer(Examiner):
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
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadCommentTokenBuilder.__escape_z__()
    ROperatorTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()
    self.newlines_important = 'parens'

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    real_tb = RealTokenBuilder(False, False, '_')
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', '_')

    leads = '_'
    extras = '_'
    suffixes = ''
    identifier_tb = IdentifierTokenBuilder(leads, extras, suffixes)

    quotes = ['"', "'", "’", '`']
    string_tb = StringTokenBuilder(quotes, True)

    hash_comment_tb = LeadCommentTokenBuilder('#')

    known_operators = [
        '+', '-', '*', '/', '**', '^',
        '%%', '%/%', '%*%', '%in%',
        '<', '<=', '>', '>=',
        '==', '!=', '!', '|', '&', '||', '&&',
        '.', ':', '::', '[[', ']]', '@', '$',
        '=', '<-', '<<-', '->', '->>'
      ]

    self.unary_operators = [
      '+', '-',
      '!', '@', '.'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator')
    user_operator_tb = ROperatorTokenBuilder()

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_starts = ['(', '[', ',', '{']
    group_ends = [')', ']', '}']

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
      'NA_integer_', 'NA_real_', 'NA_complex_', 'NA_character_',
      '...'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
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
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    tokens = self.source_tokens()
    
    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence(tokens)
    self.calc_operator_3_confidence(tokens, group_ends)
    self.calc_operator_4_confidence(tokens, group_starts)
    operand_types = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    self.calc_statistics()
