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
  LeadToEndOfLineTokenBuilder
)
from examiner import Examiner

class EiffelExaminer(Examiner):
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
    LeadToEndOfLineTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', False, '0123456789abcdefABCDEF_')
    binary_integer_tb = PrefixedIntegerTokenBuilder('0b', False, '01_')
    octal_integer_tb = PrefixedIntegerTokenBuilder('0c', False, '01234567_')
    real_tb = RealTokenBuilder(False, False, "'")
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")

    leads = '_'
    extras = '_'
    suffixes = ''
    identifier_tb = IdentifierTokenBuilder(leads, extras, suffixes)

    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, False)

    comment_tb = LeadToEndOfLineTokenBuilder('--', True, 'comment')

    known_operators = [
      ':=', '=', '/=',
      '<', '>', '<=', '>=',
      '+', '-', '*', '/', '//', '\\\\', '^',
      '|..|', '..',
      'and', 'or', 'xor', 'not',
      'and then', 'or else', 'implies',
      '.',
      '@', '#', '|', '&'
    ]

    self.unary_operators = [
      '+', '-', 'not',
      '@', '#', '|', '&'
    ]

    self.postfix_operators = []

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':', ';']
    group_starts = ['(', '[', ',', '{']
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'across', 'agent', 'alias', 'all', 'as', 'assign', 'attribute',
      'check', 'class', 'convert', 'create',
      'debug', 'deferred', 'do',
      'else', 'elseif', 'end', 'ensure', 'expanded', 'export', 'external',
      'feature', 'from', 'frozen',
      'if', 'implies', 'inherit', 'inspect', 'invariant',
      'like', 'local', 'loop',
      'note',
      'obsolete', 'old', 'once', 'only',
      'redefine', 'rename', 'require', 'rescue', 'retry',
      'select', 'separate',
      'then',
      'undefine', 'until',
      'variant',
      'when'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'Current', 'Precursor', 'Result', 'Void', 'TUPLE'
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'False', 'True', '?'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      integer_tb,
      integer_exponent_tb,
      hex_integer_tb,
      binary_integer_tb,
      octal_integer_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      types_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      string_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence(tokens)
    self.calc_operator_3_confidence(tokens, group_ends)
    self.calc_operator_4_confidence(tokens, group_starts)
    operand_types = ['number']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
