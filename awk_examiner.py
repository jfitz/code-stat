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
  SingleCharacterTokenBuilder,
  LeadToEndOfLineTokenBuilder,
  RegexTokenBuilder
)
from examiner import Examiner

class AwkExaminer(Examiner):
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
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    RegexTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, extension):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)

    num_variable_tb = PrefixedIntegerTokenBuilder('$', False, '0123456789')

    known_variables = [
      'ARGC', 'ARGV',
      'ENVIRON', 'FILENAME', 'FS', 'NF', 'NR', 'FNR',
      'OFMT', 'OFS', 'ORS', 'RLENGTH', 'RS',
      'RSTART', 'SUBSEP',
    ]

    known_variables_gnu = [
      'ARGIND', 'BINMODE', 'ERRNO', 'FIELDWIDTHS',
      'IGNORECASE', 'LINT', 'PROCINFO', 'TEXTDOMAIN'
    ]

    if extension == 'gnu':
      known_variables += known_variables_gnu

    variable_tb = ListTokenBuilder(known_variables, 'variable', True, True)

    regex_tb = RegexTokenBuilder()

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)

    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, False)

    hash_comment_tb = LeadToEndOfLineTokenBuilder('#', False, 'comment')

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation', False)
    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    known_operators = [
      '=', '+', '-', '*', '/', '%', '^',
      '++', '--',
      '==', '+=', '-=', '*=', '/=', '%=', '^=',
      '!=', '>', '>=', '<', '<=',
      '&&', '||', '|', '!', '?', ':',
      '~', '!~'
    ]

    self.unary_operators = [
      '+', '-',
      '!', '~',
      '++', '--'
    ]

    self.postfix_operators = [
      '++', '--',
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_starts = ['(', '[', ',', '{']
    group_mids = [',']
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False, False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', False, True)

    keywords = [
      'BEGIN', 'END',
      'if', 'else', 'while', 'do', 'for',
      'break', 'continue', 'delete', 'next', 'nextfile',
      'function', 'func', 'exit'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False, True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      variable_tb,
      num_variable_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      known_operator_tb,
      groupers_tb,
      regex_tb,
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
    tokens = Examiner.join_parens_continued_lines(tokens)
    tokens = Examiner.join_operator_continued_lines(tokens, self.postfix_operators)

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_operator_confidence()

    allow_pairs = []

    self.calc_operator_2_confidence(tokens, allow_pairs)
    self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
    self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)
    self.calc_group_confidence(tokens, group_mids)
    operand_types = ['number', 'symbol']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
