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
  IdentifierTokenBuilder,
  ListTokenBuilder,
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
    IdentifierTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
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

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator')

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    real_tb = RealTokenBuilder(True, True, '_')
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E', '_')
    argument_tb = SwiftArgumentTokenBuilder()
    identifier_tb = IdentifierTokenBuilder()
    attribute_tb = PrefixedIdentifierTokenBuilder('@', 'attribute')
    symbol_tb = SwiftSymbolTokenBuilder('.', 'symbol')
    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, True)
    triple_quote_comment_tb = TripleQuoteStringTokenBuilder(quotes)
    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

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

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-',
      '!', '~', '&',
      '++', '--', ':', '?'
    ]

    self.postfix_operators = [
      '++', '--', ':'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

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
    
    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'char', 'double', 'float', 'int',
      'long', 'short',
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'nil', 'Self', 'false', 'true'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

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

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_statistics()
