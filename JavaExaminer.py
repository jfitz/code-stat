import string
import math
from Examiner import Examiner
from TokenBuilders import *
from CXTokenBuilders import *
from JavaTokenBuilders import *
from Tokenizer import Tokenizer

class JavaExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = ListTokenBuilder([';'], 'statement separator', False)

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False)
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"])

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '~', '<<', '>>', '>>>', '>>>=',
      '^',
      '(', ')', ',', '.', ':',
      '[', ']',
      '++', '--', '&&', '||',
      '?', '{', '}'
      ]
    
    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'abstract', 'assert', 'boolean', 'break', 'byte',
      'case', 'catch', 'char', 'class', 'const',
      'continue', 'default', 'do', 'double',
      'else', 'enum', 'extends', 'false', 'final',
      'finally', 'float', 'for', 'goto',
      'if', 'implements', 'import', 'instanceof', 'int', 'interface',
      'long', 'native', 'new', 'null', 'package',
      'private', 'protected', 'public',
      'return', 'short',
      'static', 'strictfp', 'super', 'switch', 'synchronized',
      'this', 'throw', 'throws', 'true', 'transient', 'try',
      'void', 'volatile', 'while'
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
      identifier_tb,
      string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      self.unknown_operator_tb
    ]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = tokenizer.tokenize(code)

    num_known_tokens = self.count_valid_tokens(self.tokens)
    num_invalid_operators = self.count_invalid_operators(self.tokens)
    num_known_operators = self.count_known_operators(self.tokens)

    # consider the number of matches for begin/end
    ok, num_begin, num_end = self.check_paired_tokens(self.tokens, '{', '}')
    num_begin_end = num_begin + num_end
    brace_match_confidence = 0.0

    if num_begin_end > 0:
      brace_match_confidence = (num_begin + num_end) / num_begin_end

    #  unknown tokens reduce confidence
    token_confidence = 1.0

    if len(self.tokens) > 0:
      token_confidence = num_known_tokens / len(self.tokens)

    #  unknown operators reduce confidence
    operator_confidence = 1.0
    num_operators = num_known_operators + num_invalid_operators

    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    self.confidences = {
      'brace_match': brace_match_confidence,
      'token': token_confidence,
      'operator': operator_confidence
    }
