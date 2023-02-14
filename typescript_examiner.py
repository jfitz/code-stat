import string
import math

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
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  PrefixedIntegerTokenBuilder,
  RegexTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder
)
from examiner import Examiner

class TypeScriptExaminer(Examiner):
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
    IdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    RegexTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    SlashStarCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    hex_constant_tb = PrefixedIntegerTokenBuilder('0H', False, '0123456789ABCDEFabcdef')
    octal_constant_tb = PrefixedIntegerTokenBuilder('0O', False, '01234567')
    binary_constant_tb = PrefixedIntegerTokenBuilder('0B', False, '01')
    operand_types.append('number')

    leads = '_$'
    extras = '_$'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    quotes = ['"', "'"]
    string_tb = EscapedStringTokenBuilder(quotes, 0)
    operand_types.append('string')

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    terminators_tb = CaseInsensitiveListTokenBuilder([';'], 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '===', '!==', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '~', '<<', '>>',
      '=>', '^',
      '.', ':',
      '++', '--', '&&', '||',
      '?', '$', '?.',
      'new', 'delete'
    ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-',
      '!', '~',
      '++', '--', ':', '$',
      'new', 'delete'
    ]

    self.postfix_operators = [
      '++', '--', ':'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_starts = ['(', '[', ',', '{']
    group_mids = [',']
    group_ends = [')', ']', '}']

    groupers_tb = CaseSensitiveListTokenBuilder(groupers, 'group', False)

    regex_tb = RegexTokenBuilder()

    keywords = [
      'break', 'case', 'catch', 'class', 'const', 'continue',
      'debugger', 'default', 'do', 'else', 'enum', 'export',
      'extends', 'finally', 'for', 'function', 'if', 'import',
      'in', 'instanceof', 'return', 'switch',
      'throw', 'try', 'typeof', 'while',
      'with',
      'as', 'implements', 'interface', 'let', 'package',
      'private', 'protected', 'public',
      'static', 'yield',
      'constructor', 'declare', 'get', 'module', 'require',
      'set', 'type', 'from', 'of'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    types = [
      'any', 'boolean', 'byte', 'char', 'number', 'string', 'symbol',
      'void', 'never', 'object'
    ]

    types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    values = [
      'this', 'super', 'null', 'true', 'false', 'undefined'
    ]

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      hex_constant_tb,
      octal_constant_tb,
      binary_constant_tb,
      keyword_tb,
      types_tb,
      values_tb,
      known_operator_tb,
      groupers_tb,
      regex_tb,
      identifier_tb,
      string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    self.convert_keywords_to_identifiers(['.'])

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

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
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_length_confidence(code, self.max_expected_line)
