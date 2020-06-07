import string
import math

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
  PrefixedIntegerTokenBuilder,
  SuffixedIdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  PrefixedIdentifierTokenBuilder,
  TripleQuoteStringTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder
)
from swift_token_builders import (
  SwiftArgumentTokenBuilder,
  SwiftSymbolTokenBuilder
)
from examiner import Examiner

class SwiftExaminer(Examiner):
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
    PrefixedIntegerTokenBuilder.__escape_z__()
    SuffixedIdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    PrefixedIdentifierTokenBuilder.__escape_z__()
    TripleQuoteStringTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    SlashStarCommentTokenBuilder.__escape_z__()
    SwiftArgumentTokenBuilder.__escape_z__()
    SwiftSymbolTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator', False)

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    real_tb = RealTokenBuilder(True, True, '_')
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E', '_')
    operand_types.append('number')

    argument_tb = SwiftArgumentTokenBuilder()

    leads = '_'
    extras = '_'
    suffixes = '?'
    identifier_tb = SuffixedIdentifierTokenBuilder(leads, extras, suffixes)
    operand_types.append('identifier')

    attribute_tb = PrefixedIdentifierTokenBuilder('@', 'attribute', False)

    symbol_tb = SwiftSymbolTokenBuilder('.', 'symbol', True)
    operand_types.append('symbol')

    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, 10)
    triple_quote_comment_tb = TripleQuoteStringTokenBuilder(quotes)
    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()
    operand_types.append('string')

    known_operators = [
        '+', '-', '*', '/', '%',
        '==', '!=', '>', '<', '>=', '<=',
        '&&', '||', '!', '&', '|', '^',
        '~', '<<', '>>', '===',
        '=', '+=', '-=', '*=', '/=', '%=', '<<=', '>>=', '&=', '^=', '|=',
        '...', '..<', '?', ':',
        '.', '++', '--',
        '->', '??', '\\.',
        '&+', '&-', '&*'
      ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-',
      '!', '~', '&',
      '++', '--', ':', '?'
    ]

    self.postfix_operators = [
      '++', '--', ':', '!', '?'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_starts = ['(', '[', ',', '{']
    group_mids = [',']
    group_ends = [')', ']', '}']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'associatedtype', 'class', 'deinit', 'enum', 'extension', 'fileprivate',
      'func', 'import', 'init', 'inout', 'internal', 'let', 'open', 'operator',
      'private', 'protocol', 'public', 'static', 'struct', 'subscript',
      'typealias', 'var',
      'break', 'case', 'continue', 'default', 'defer', 'do', 'else', 'fallthrough',
      'for', 'guard', 'if', 'in', 'repeat', 'return', 'switch', 'where', 'while',
      'as', 'Any', 'catch', 'is', 'rethrows', 'super',
      'throw', 'throws', 'try', 'try?', 'try!',
      '#available', '#colorLiteral', '#column', '#else', '#elseif', '#endif',
      '#file', '#fileLiteral', '#function', '#if', '#imageLiteral', '#line',
      '#selector', '#sourceLocation',
      'associativity', 'convenience', 'dynamic', 'didSet', 'final', 'get',
      'infix', 'indirect', 'lazy', 'left', 'mutating', 'none', 'nonmutating',
      'optional', 'override', 'postfix', 'precedence', 'prefix', 'Protocol',
      'required', 'right', 'set', 'Type', 'unowned', 'weak', 'willSet'
    ]
    
    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    types = [
      'char', 'double', 'float', 'int',
      'long', 'short',
    ]

    types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    values = [
      'nil', 'Self', 'false', 'true'
    ]

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      argument_tb,
      keyword_tb,
      types_tb,
      values_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      attribute_tb,
      symbol_tb,
      string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      triple_quote_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]
    
    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    self.convert_keywords_to_identifiers(['.'])

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_operator_confidence()

    allow_pairs = []

    self.calc_operator_2_confidence(tokens, allow_pairs)
    self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
    self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)
    self.calc_group_confidence(tokens, group_mids)
    operand_types = ['number', 'string', 'symbol']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_operand_n_confidence(tokens, operand_types, 4)
    self.calc_keyword_confidence()
    self.calc_statistics()
