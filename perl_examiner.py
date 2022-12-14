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
  PrefixedIntegerTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadToEndOfLineTokenBuilder,
  RegexTokenBuilder
)
from perl_token_builders import (
  PerlIdentifierTokenBuilder,
  PerlDollarCaretIdentifierTokenBuilder,
  PerlQStringTokenBuilder,
  MRegexTokenBuilder,
  SRegexTokenBuilder,
  YRegexTokenBuilder,
  TrRegexTokenBuilder,
  PerlPrototypeTokenBuilder,
  PerlSigilBraceTokenBuilder
)
from examiner import Examiner

class PerlExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    EscapedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    RegexTokenBuilder.__escape_z__()
    PerlIdentifierTokenBuilder.__escape_z__()
    PerlDollarCaretIdentifierTokenBuilder.__escape_z__()
    PerlQStringTokenBuilder.__escape_z__()
    MRegexTokenBuilder.__escape_z__()
    SRegexTokenBuilder.__escape_z__()
    YRegexTokenBuilder.__escape_z__()
    TrRegexTokenBuilder.__escape_z__()
    PerlPrototypeTokenBuilder.__escape_z__()
    PerlSigilBraceTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', False, '0123456789abcdefABCDEF')
    real_tb = RealTokenBuilder(False, False, "'")
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")
    operand_types.append('number')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    perl_identfier_tb = PerlIdentifierTokenBuilder()
    operand_types.append('identifier')

    specials = [
      '$_', '@_', '$$', '$"', '$(', '$)', '$>', '$<', '$;', '$]', '$[',
      '$&', '$`', "$'", '$+', '@+', '%+', '@-', '%-', '$,', '$.',
      '$/', '$\\', '$|', '$%', '$-', '$:', '$=', '$^', '$~', '$!',
      '$?', '$@', '$#', '$*', '@*'
    ]

    specials_tb = CaseInsensitiveListTokenBuilder(specials, 'identifier', True)

    dollar_carat_tb = PerlDollarCaretIdentifierTokenBuilder()

    sigilbrace_tb = PerlSigilBraceTokenBuilder()

    quotes = ['"', "'"]
    string_tb = EscapedStringTokenBuilder(quotes, 0)
    operand_types.append('string')

    q_string_tb = PerlQStringTokenBuilder()

    regex_tb = RegexTokenBuilder()
    m_regex_tb = MRegexTokenBuilder()
    s_regex_tb = SRegexTokenBuilder()
    y_regex_tb = YRegexTokenBuilder()
    tr_regex_tb = TrRegexTokenBuilder()
    operand_types.append('regex')

    prototype_tb = PerlPrototypeTokenBuilder()

    comment_tb = LeadToEndOfLineTokenBuilder('#', False, 'comment')

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation', False)

    directives = [
      '#line'
    ]

    preprocessor_tb = CaseSensitiveListTokenBuilder(directives, 'preprocessor', False)

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '**', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '**=', '+=', '*=', '&=', '&.=', '<<=', '&&=',
      '-=', '/=', '|=', '|.=', '>>=', '||=',
      '.=', '%=', '^=', '^.=', '//=', 'x=',
      'ne', 'gt', 'ge', 'le', 'lt', 'eq', 'cmp',
      '!', '&', '|', '~', '<<', '>>', '<=>',
      '^',
      '.', '..', '...',
      '++', '--', '->', '=>', '&&', '||',
      '?', '<->', '//',
      'and', 'cmp', 'or', 'xor', 'not',
      'x', 'isa', '&.', '|.'
    ]

    self.unary_operators = [
      '+', '-', '*',
      '!', '&', '~',
      '++', '--', 'not'
    ]

    self.postfix_operators = [
      '++', '--'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':', '::']
    group_starts = ['(', '[', ',', '{']
    group_mids = [',', ':', '::']
    group_ends = [')', ']', '}']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'bless', 'break',
      'continue',
      'die', 'do',
      'else', 'elsif', 'eval', 'exit', 'exp',
      'for', 'foreach',
      'if',
      'last', 'lock',
      'my',
      'next', 'no',
      'our',
      'package',
      'redo', 'return',
      'say', 'sub',
      'taint',
      'undef', 'unless', 'until', 'use',
      'wantarray', 'while'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', True)

    values = ['NULL']

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      hex_integer_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      prototype_tb,
      identifier_tb,
      perl_identfier_tb,
      specials_tb,
      dollar_carat_tb,
      sigilbrace_tb,
      string_tb,
      q_string_tb,
      regex_tb,
      m_regex_tb,
      s_regex_tb,
      y_regex_tb,
      tr_regex_tb,
      preprocessor_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['statement terminator', 'newline'], ['{'], ['whitespace', 'comment'])
    self.tokens = tokens
    self.convert_identifiers_to_labels()

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence(['*', ';'])

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_length_confidence(code, self.max_expected_line)
