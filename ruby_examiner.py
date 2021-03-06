import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  EscapedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadToEndOfLineTokenBuilder,
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
    EscapedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    PrefixedIdentifierTokenBuilder.__escape_z__()
    RegexTokenBuilder.__escape_z__()
    RubyIdentifierTokenBuilder.__escape_z__()
    HereDocTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    self.newlines_important = 'parens'

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator', False)

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    real_tb = RealTokenBuilder(True, True, '_')
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E', '_')
    operand_types.append('number')

    identifier_tb = RubyIdentifierTokenBuilder()
    operand_types.append('identifier')

    symbol_tb = PrefixedIdentifierTokenBuilder(':', 'symbol', True)
    operand_types.append('symbol')

    quotes = ['"', "'", "’"]
    string_tb = EscapedStringTokenBuilder(quotes, 10)
    operand_types.append('string')

    regex_tb = RegexTokenBuilder()
    operand_types.append('regex')

    heredoc_tb = HereDocTokenBuilder('<<-')

    hash_comment_tb = LeadToEndOfLineTokenBuilder('#', False, 'comment')

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
        '.', '.:', '=>', '::',
        '<<-'
      ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-',
      '!', '~',
      '&', '*', '**',
      '<<-'
    ]

    self.postfix_operators = [
      '++', '--'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_starts = ['(', '[', ',', '{']
    group_mids = [',']
    group_ends = [')', ']', '}']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'BEGIN', 'END',
      'alias',
      'begin', 'break',
      'case', 'class',
      'def', 'defined?', 'do',
      'else', 'elsif', 'end', 'ensure',
      'for',
      'if',
      'module',
      'next',
      'redo', 'rescue', 'retry', 'return',
      'then',
      'undef', 'unless', 'until',
      'when', 'while',
      'yield'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    values = [
      'nil', 'self', 'true', 'false', 'super'
    ]

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    array_markers = ['%w', '%q', '%Q', '%i', '%s', '%x']

    array_marker_tb = CaseSensitiveListTokenBuilder(array_markers, 'identifier', True)

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
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    self.convert_bars_to_groups()
    self.convert_keywords_to_identifiers(['.'])
    self.convert_operators_to_identifiers()

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_parens_continued_lines(tokens)
    tokens = Examiner.join_operator_continued_lines(tokens, self.postfix_operators)

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'string', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    # self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    openers = ['begin', 'def', 'do', 'class', 'module']
    closers = ['end']
    self.calc_paired_blockers_confidence(openers, closers)

    self.calc_line_length_confidence(code, self.max_expected_line)


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
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'operator' and\
        prev_token.group == 'keyword' and prev_token.text == 'def':
        token.group = 'identifier'
        token.is_operand = True

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token


  def check_paired_tokens(self, tokens, open_tokens, close_tokens):
    level = 0
    min_level = 0
    num_open = 0
    num_close = 0

    prev_token_lower = ''
    prev_token = Token('\n', 'newline', False)

    prev_reqs = [';', '=']
    conditional_openers = ['if', 'case', 'while', 'until', 'unless']

    drop_types = ['whitespace', 'comment', 'line continuation']
    tokens = Examiner.drop_tokens(tokens, drop_types)

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
