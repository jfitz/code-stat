import string
import math
from Token import Token
from Examiner import Examiner
from TokenBuilders import (
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
  LeadCommentTokenBuilder,
  RegexTokenBuilder
)
from Tokenizer import Tokenizer

class AwkExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
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

    variable_tb = ListTokenBuilder(known_variables, 'variable', True)
    real_tb = RealTokenBuilder(False, False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False, False)

    hash_comment_tb = LeadCommentTokenBuilder('#')

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation')
    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

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
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    regex_tb = RegexTokenBuilder()

    keywords = [
      'BEGIN', 'END',
      'if', 'else', 'while', 'do', 'for',
      'break', 'continue', 'delete', 'next', 'nextfile',
      'function', 'func', 'exit'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

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
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
