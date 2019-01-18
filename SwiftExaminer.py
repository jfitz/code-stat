import string
import math
from Examiner import Examiner
from TokenBuilders import *
from CXTokenBuilders import *
from SwiftTokenBuilders import *
from Tokenizer import Tokenizer

class SwiftExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(True)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False)
    identifier_tb = IdentifierTokenBuilder()
    attribute_tb = PrefixedIdentifierTokenBuilder('@', 'attribute')
    string_tb = StringTokenBuilder(['"'])

    triple_quote_comment_tb = TripleQuoteCommentTokenBuilder()
    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    known_operators = [
        '+', '-', '*', '/', '%',
        '==', '!=', '>', '<', '>=', '<=',
        '&&', '||', '!', '&', '|', '^',
        '~', '<<', '>>',
        '=', '+=', '-=', '*=', '/=', '%=', '<<=', '>>=', '&=', '^=', '|=',
        '...', '..<', '?', ':',
        '(', ')', '[', ']', '.', '++', '--',
        ',', ';', '{', '}', '->', '??'
      ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

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
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      known_operator_tb,
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

    num_known_tokens = self.count_valid_tokens(self.tokens)
    num_invalid_operators = self.count_invalid_operators(self.tokens)
    num_known_operators = self.count_known_operators(self.tokens)

    # unknown tokens reduce confidence
    token_confidence = 1.0

    if len(self.tokens) > 0:
      token_confidence = num_known_tokens / len(self.tokens)

    # unknown operators reduce confidence
    operator_confidence = 1.0
    num_operators = num_known_operators + num_invalid_operators

    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    self.confidences = {
      'token': token_confidence,
      'operator': operator_confidence
    }
