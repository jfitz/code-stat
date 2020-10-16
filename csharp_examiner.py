import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  PrefixedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder
)
from examiner import Examiner

class CsharpExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StringTokenBuilder.__escape_z__()
    PrefixedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
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
    operand_types.append('number')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, 10)
    prefixed_string_tb = PrefixedStringTokenBuilder('@', False, ['"'])
    operand_types.append('string')

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    directives = [
      '#if', '#else', '#elif', '#endif',
      '#define', '#undef',
      '#line', '#pragma'
    ]

    preprocessor_tb = CaseSensitiveListTokenBuilder(directives, 'preprocessor', False)
    c_warning_tb = LeadToEndOfLineTokenBuilder('#warning', True, 'preprocessor')
    c_error_tb = LeadToEndOfLineTokenBuilder('#error', True, 'preprocessor')
    c_region_tb = LeadToEndOfLineTokenBuilder('#region', True, 'preprocessor')
    c_endregion_tb = LeadToEndOfLineTokenBuilder('#endregion', True, 'preprocessor')

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '~', '<<', '>>',
      '^',
      '.',
      '++', '--', '->', '&&', '||',
      '?', '??', '?.', '?[',
      '=>',
      'as', 'is', 'await', 'sizeof',
      'typeof', 'new'
    ]
    
    self.unary_operators = [
      '+', '-',
      '!', '~',
      '++', '--',
      'new', 'sizeof', 'typeof'
    ]

    self.postfix_operators = [
      '++', '--'
    ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':']
    group_starts = ['(', '[', ',', '{']
    group_ends = [')', ']', '}']
    group_mids = [',', ':']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'abstract', 'break',
      'case', 'catch', 'checked', 'class', 'const',
      'continue', 'default', 'delegate', 'do',
      'else', 'enum', 'event', 'explicit', 'extern',
      'finally', 'fixed', 'for', 'foreach', 'goto',
      'if', 'implicit', 'in', 'interface', 'internal',
      'lock', 'namespace', 'operator',
      'out', 'override', 'params', 'partial', 'private', 'protected', 'public',
      'readonly', 'ref', 'return', 'sealed',
      'stackalloc', 'static', 'struct', 'switch',
      'throw', 'try',
      'unchecked', 'unsafe', 'using', 'using static',
      'virtual', 'volatile', 'while'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    types = [
      'bool', 'byte', 'char', 'decimal', 'double', 'float', 'int', 'long', 'object',
      'sbyte', 'short', 'string', 'uint', 'ulong', 'ushort', 'void'
    ]

    types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    values = [
      'base', 'false', 'null', 'this', 'true'
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
      keyword_tb,
      types_tb,
      values_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      string_tb,
      prefixed_string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      preprocessor_tb,
      c_error_tb,
      c_warning_tb,
      c_region_tb,
      c_endregion_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['statement terminator', 'newline'], ['{'], ['whitespace', 'comment'])
    self.tokens = tokens
    self.convert_identifiers_to_labels()

    number_suffixes = ['f', 'F', 'd', 'D', 'm', 'M']
    self.tokens = self.combine_tokens_and_adjacent_types(tokens, 'number', 'identifier', number_suffixes)

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

    self.calc_preprocessor_confidence()

    self.calc_paired_blockers_confidence(['{'], ['}'])
