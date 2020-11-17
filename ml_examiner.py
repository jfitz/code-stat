import string
import math

from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  EscapedStringTokenBuilder,
  PrefixedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  BlockTokenBuilder,
  TripleQuoteStringTokenBuilder,
  LeadToEndOfLineTokenBuilder,
  NullTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  TripleSlashCommentTokenBuilder,
  ClassTypeTokenBuilder
)
from fsharp_token_builders import (
  FsharpCharTokenBuilder
)
from examiner import Examiner

class MlExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    EscapedStringTokenBuilder.__escape_z__()
    PrefixedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    BlockTokenBuilder.__escape_z__()
    TripleQuoteStringTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    TripleSlashCommentTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    NullTokenBuilder.__escape_z__()
    ClassTypeTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, variant):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(True, True, None)
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E', None)
    operand_types.append('number')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    class_type_tb = ClassTypeTokenBuilder()
    operand_types.append('class')

    quotes = ['"']
    string_tb = EscapedStringTokenBuilder(quotes, 0)
    triple_quote_string_tb = TripleQuoteStringTokenBuilder(quotes)
    prefixed_string_tb = PrefixedStringTokenBuilder('@', False, quotes)
    char_tb = FsharpCharTokenBuilder(["'", "’"])
    operand_types.append('string')

    slash_slash_comment_tb = NullTokenBuilder()
    parens_star_comment_tb = BlockTokenBuilder('(*', '*)', 'comment')
    triple_slash_comment_tb = NullTokenBuilder()
    if variant in ['fsharp']:
      slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
      triple_slash_comment_tb = TripleSlashCommentTokenBuilder()

    directives = [
      '#if', '#else', '#elif', '#endif',
      '#define', '#undef',
      '#line', '#region', '#endregion', '#pragma'
    ]

    preprocessor_tb = CaseSensitiveListTokenBuilder(directives, 'preprocessor', False)
    c_warning_tb = LeadToEndOfLineTokenBuilder('#warning', True, 'preprocessor')
    c_error_tb = LeadToEndOfLineTokenBuilder('#error', True, 'preprocessor')

    known_operators = [
      'and', 'as', 'in', 'mod', 'not', 'of', 'or', 'when',
      '::',
      '+', '-', '*', '/',
      '+.', '-.', '*.', '/.',
      '=', "'", '->', '>', '<', '>=', '<=', '==',
      '^', '||', '.', '#'
    ]

    known_operators_fsharp = [
      'new',
      '!', '!=', '%', '%%', '%?', '&', '&&', '&&&',
      '(|', '|)', '*?', '**', '+?', '-?',
      '->', '..', '.. ..',
      '/?', ':', ':=', ':/',
      '<<', '<<<', '<-', '<>', '<>?', '<=?',
      '<|', '<||', '<|||',
      '<@', '@>', '<@@', '@@>',
      '=?', '==', '>?', '>>', '>>>', '>=?',
      '?', '|||', '^^^',
      '?>=', '?>', '?<=', '?<', '?=', '?<>', '?+', '?-', '?*', '?/',
      '>=?', '>?', '<=?', '<?', '=?', '<>?', '+?', '-?', '*?', '/?',
      '?>=?', '?>?', '?<=?', '?<?', '?=?', '?<>?', '?+?', '?-?', '?*?', '?/?',
      '@', '|>', '||>', '|||>',
      '~~', '~~~', '~-', '~+',
      ':>', ':?>', "'"
    ]

    if variant in ['fsharp']:
      known_operators += known_operators_fsharp

    self.unary_operators = [
      'new', 'not', "'", '-'
    ]

    self.postfix_operators = [
      "'"
    ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    groupers = [
      '(', ')', ',', '[', ']', '{', '}',
      'begin', 'end', ';', '|'
    ]

    groupers_fsharp = [
      '[|', '|]', '[<', '>]',
      '^'
    ]

    if variant in ['fsharp']:
      groupers += groupers_fsharp

    # group_starts = ['(', '[', ',', '{', '[|', '[<']
    group_mids = [',', ';', '^', '|']
    group_ends = [')', ']', '}', '|]', '>]']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'assert',
      'class',
      'def', 'do', 'done', 'downto',
      'else', 'exception',
      'failwith', 'for', 'fun', 'function',
      'if', 'inherit',
      'lazy', 'let',
      'match', 'method', 'module',
      'object', 'open',
      'raise', 'rec',
      'sig',
      'then', 'to', 'try', 'type',
      'val', 'virtual',
      'while', 'with'
    ]

    keywords_fsharp = [
      'abstract',
      'break',
      'default', 'delegate', 'downcast',
      'elif', 'extern',
      'finally', 'fixed',
      'global',
      'inline', 'interface', 'internal',
      'let!',
      'match!', 'member', 'mutable',
      'namespace',
      'override',
      'private', 'public',
      'return', 'return!',
      'upcast', 'use', 'use!',
      'yield', 'yield!'
    ]

    if variant in ['fsharp']:
      keywords += keywords_fsharp

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    types = [
      'bool', 'byte',
      'char',
      'double',
      'float',
      'int',
      'list', 'long',
      'number',
      'object',
      'range',
      'string', 'struct',
      'union', 'unit',
      'void'
    ]

    types_fsharp = [
      'decimal',
      'sbyte', 'short',
      'uint', 'ulong', 'ushort',
      'void'
    ]

    if variant in ['fsharp']:
      types += types_fsharp

    types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    values = [
      'base', 'false', 'null', 'true', '_'
    ]

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
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
      class_type_tb,
      string_tb,
      triple_quote_string_tb,
      prefixed_string_tb,
      char_tb,
      triple_slash_comment_tb,
      slash_slash_comment_tb,
      parens_star_comment_tb,
      preprocessor_tb,
      c_error_tb,
      c_warning_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

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
      # self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'string', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    # self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    self.calc_paired_blockers_confidence(['{'], ['}'])
