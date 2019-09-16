import string
import math
from Examiner import Examiner
from TokenBuilders import (
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
from CXTokenBuilders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder
)
from SwiftTokenBuilders import (
  SwiftArgumentTokenBuilder,
  SwiftSymbolTokenBuilder
)
from Tokenizer import Tokenizer

class SwiftExaminer(Examiner):
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
    string_tb = StringTokenBuilder(['"', "'"], False, False)

    triple_quote_comment_tb = TripleQuoteStringTokenBuilder()
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
        '->', '??',
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
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_statistics()
