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
  ListTokenBuilder,
  PrefixedIdentifierTokenBuilder,
  TripleQuoteCommentTokenBuilder
)
from CXTokenBuilders import (
  IdentifierTokenBuilder,
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder
)
from Tokenizer import Tokenizer

class SwiftExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = ListTokenBuilder([';'], 'statement separator', False)

    integer_tb = IntegerTokenBuilder(True)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(True, True)
    real_exponent_tb = RealExponentTokenBuilder(True, True)
    identifier_tb = IdentifierTokenBuilder()
    attribute_tb = PrefixedIdentifierTokenBuilder('@', 'attribute')
    string_tb = StringTokenBuilder(['"'], False)

    triple_quote_comment_tb = TripleQuoteCommentTokenBuilder()
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
        '->', '??'
      ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-',
      '!', '~',
      '++', '--'
    ]

    self.postfix_operators = [
      '++', '--'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'associatedtype', 'class', 'deinit', 'enum', 'extension', 'fileprivate',
      'func', 'import', 'init', 'inout', 'internal', 'let', 'open', 'operator',
      'private', 'protocol', 'public', 'static', 'struct', 'subscript',
      'typealias', 'var',
      'break', 'case', 'continue', 'default', 'defer', 'do', 'else', 'fallthrough',
      'for', 'guard', 'if', 'in', 'repeat', 'return', 'switch', 'where', 'while',
      'as', 'Any', 'catch', 'false', 'is', 'nil', 'rethrows', 'super', 'self',
      'Self', 'throw', 'throws', 'true', 'try',
      '#available', '#colorLiteral', '#column', '#else', '#elseif', '#endif',
      '#file', '#fileLiteral', '#function', '#if', '#imageLiteral', '#line',
      '#selector', '#sourceLocation',
      'associativity', 'convenience', 'dynamic', 'didSet', 'final', 'get',
      'infix', 'indirect', 'lazy', 'left', 'mutating', 'none', 'nonmutating',
      'optional', 'override', 'postfix', 'precedence', 'prefix', 'Protocol',
      'required', 'right', 'set', 'Type', 'unowned', 'weak', 'willSet'
    ]
    
    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      attribute_tb,
      string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      triple_quote_comment_tb,
      self.unknown_operator_tb
    ]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
