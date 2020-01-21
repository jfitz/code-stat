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
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadCommentTokenBuilder,
  PrefixedIdentifierTokenBuilder,
  RegexTokenBuilder
)
from ruby_token_builders import (
  RubyIdentifierTokenBuilder,
  HereDocTokenBuilder
)
from examiner import Examiner

class RubyExaminer(Examiner):
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
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadCommentTokenBuilder.__escape_z__()
    PrefixedIdentifierTokenBuilder.__escape_z__()
    RegexTokenBuilder.__escape_z__()
    RubyIdentifierTokenBuilder.__escape_z__()
    HereDocTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator')

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    real_tb = RealTokenBuilder(True, True, '_')
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E', '_')
    identifier_tb = RubyIdentifierTokenBuilder()
    symbol_tb = PrefixedIdentifierTokenBuilder(':', 'symbol')
    string_tb = StringTokenBuilder(['"', "'"], False, False, False)
    heredoc_tb = HereDocTokenBuilder()

    hash_comment_tb = LeadCommentTokenBuilder('#')

    known_operators = [
        '!', '~',
        '**',
        '*', '/', '%',
        '+', '-',
        '<<', '>>',
        '&', '|', '^',
        '<', '<=', '>', '>=',
        '==', '===', '!=', '=~', '!~', '<=>',
        '&&', '||', '..', '...',
        '?', ':',
        '=', '**=', '*=', '/=', '%=', '+=', '-=',
        '<<=', '>>=',
        '&&=', '&=', '||=', '|=', '^=',
        'not', 'and', 'or', 'in',
        '.', '.:', '=>', '::'
      ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-',
      '!', '~',
      '&', '*', '**'
    ]

    self.postfix_operators = [
      '++', '--'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}', 'end']
    group_ends = [')', ']', '}', 'end']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    regex_tb = RegexTokenBuilder()

    keywords = [
      'BEGIN', 'END', 'alias', 'begin', 'break', 'case', 'class',
      'def', 'defined?', 'do', 'else', 'elsif', 'ensure',
      'for', 'if', 'module', 'next', 'redo',
      'rescue', 'retry', 'return', 'then',
      'undef', 'unless', 'until', 'when', 'while', 'yield'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    values = [
      'nil', 'self', 'true', 'false', 'super'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    array_markers = ['%w', '%q', '%Q', '%i', '%s', '%x']

    array_marker_tb = ListTokenBuilder(array_markers, 'identifier', True)

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
      symbol_tb,
      known_operator_tb,
      groupers_tb,
      regex_tb,
      identifier_tb,
      array_marker_tb,
      string_tb,
      heredoc_tb,
      hash_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.convert_bars_to_groups()
    self.convert_keywords_to_identifiers()
    self.convert_operators_to_identifiers()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    openers = ['begin', 'def', 'do', 'class', 'module']
    closers = ['end']
    self.calc_paired_blockers_confidence(openers, closers)
    self.calc_statistics()


  def convert_bars_to_groups(self):
    bar_count = 2

    for token in self.tokens:
      if token.group in ['whitespace', 'comment']:
        continue

      if bar_count < 2:
        if token.group == 'operator' and token.text == '|':
          token.group = 'group'
          bar_count += 1
          continue

        if token.group == 'identifier':
          continue
  
        if token.group ==  'group' and token.text == ',':
          continue

        bar_count = 2

      if (token.group == 'keyword' and token.text == 'do') or \
         (token.group == 'group' and token.text == '{'):
        bar_count = 0


  def convert_operators_to_identifiers(self):
    prev_token = Token('\n', 'newline')

    for token in self.tokens:
      if token.group == 'operator' and\
        prev_token.group == 'keyword' and prev_token.text == 'def':
        token.group = 'identifier'

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token


  def check_paired_tokens(self, tokens, open_tokens, close_tokens):
    level = 0
    min_level = 0
    num_open = 0
    num_close = 0

    prev_token_lower = ''
    prev_token = Token('\n', 'newline')

    prev_reqs = [';', '=']
    conditional_openers = ['if', 'case', 'while', 'until', 'unless']

    drop_types = ['whitespace', 'comment', 'line continuation']
    tokens = self.drop_tokens(tokens, drop_types)

    openers_stack = []
    for token in tokens:
      token_lower = token.text.lower()

      if token.group == 'keyword':
        if token_lower in open_tokens or\
          token_lower in conditional_openers and\
            (prev_token.group == 'newline' or prev_token_lower in prev_reqs):
          num_open += 1
          level += 1
          openers_stack.append(token_lower)

      if token_lower in close_tokens:
        num_close += 1
        level -= 1

        if level < min_level:
          min_level = level

        if len(openers_stack) > 0:
          openers_stack = openers_stack[:-1]

      prev_token_lower = token_lower
      prev_token = token

    ok = level == 0 and min_level == 0
    return ok, num_open, num_close