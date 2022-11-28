import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  EscapedStringTokenBuilder,
  TripleQuoteStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  PrefixedIntegerTokenBuilder,
  SuffixedIntegerTokenBuilder,
  SuffixedRealTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  RegexTokenBuilder,
  LeadToEndOfLineTokenBuilder,
  SheBangTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder,
  ClassTypeTokenBuilder
)
from examiner import Examiner

class GroovyExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    EscapedStringTokenBuilder.__escape_z__()
    TripleQuoteStringTokenBuilder.__escape_z__()
    RegexTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    SuffixedIntegerTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    SuffixedRealTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    SheBangTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    SlashStarCommentTokenBuilder.__escape_z__()
    ClassTypeTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("_")
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', False, '_0123456789abcdefABCDEF')
    binary_integer_tb = PrefixedIntegerTokenBuilder('0b', False, '_01')
    suffixed_integer_tb = SuffixedIntegerTokenBuilder(['G', 'L', 'I', ], False, '_')
    real_tb = RealTokenBuilder(False, False, "_")
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "_")
    suffixed_real_tb = SuffixedRealTokenBuilder(False, False, ['G', 'D', 'F'], False, '_')
    operand_types.append('number')

    leads = '@_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    quotes = ['"', "'", "’"]
    string_tb = EscapedStringTokenBuilder(quotes, 0)
    triple_quote_string_tb = TripleQuoteStringTokenBuilder(quotes)
    regex_tb = RegexTokenBuilder()
    # dollar-slash slash-dollar strings (allow newline)
    operand_types.append('string')

    class_type_tb = ClassTypeTokenBuilder()
    operand_types.append('class')

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    shebang_tb = SheBangTokenBuilder()
    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '/', '%', '**',
      '=', '==', '!=', '===', '!==', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '**=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '~', '<<', '>>', '>>>',
      '^', '?.', '?:', '<>', '>>>=',
      '.', '.&', '.@', '::', '=~', '==~', '*.', '*:', '..', '..<', '<=>',
      '++', '--', '->', '&&', '||',
      '?', '##', 'as', 'in', '!in', 'instanceof', '!instanceof', 'new',
    ]

    self.unary_operators = [
      '+', '-', '*',
      '!', '&', '~',
      '++', '--', '?'
    ]

    self.postfix_operators = [
      '++', '--', '&', '*'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':']
    # group_starts = ['(', '[', ',', '{']
    group_ends = [')', ']', '}']
    group_mids = [',', ':']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'assert',
      'break',
      'case', 'catch', 'class', 'const', 'continue',
      'def', 'default', 'do',
      'else', 'enum', 'extends',
      'finally', 'for',
      'goto',
      'if', 'implements', 'import', 'interface',
      'new',
      'package',
      'return',
      'super', 'switch',
      'throw', 'throws', 'trait', 'try',
      'var',
      'while'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    types = [
      'byte', 'char', 'double', 'float', 'int',
      'long', 'short', 'Java.lang.BigInteger'
    ]

    types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    values = ['null', 'true', 'false', 'this']

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      hex_integer_tb,
      binary_integer_tb,
      suffixed_integer_tb,
      real_tb,
      real_exponent_tb,
      suffixed_real_tb,
      keyword_tb,
      types_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      class_type_tb,
      string_tb,
      triple_quote_string_tb,
      regex_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      shebang_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    # shebang line at start

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
      # self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_length_confidence(code, self.max_expected_line)
