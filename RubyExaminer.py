import string
import math
from Examiner import Examiner
from TokenBuilders import *
from RubyTokenBuilders import *
from Tokenizer import Tokenizer

class RubyExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(True)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False)
    identifier_tb = IdentifierTokenBuilder()
    symbol_tb = SymbolTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"])
    heredoc_tb = HereDocTokenBuilder()

    hash_comment_tb = LeadCommentTokenBuilder('#')

    known_operators = [
        '!', '~',
        '**',
        '*', '/', '%',
        '+', '-',
        '<<', '>>',
        '&', '|', '^',
        '<', '<=', '>', '>=',
        '==', '===', '!=', '=~', '!~', '<=>',
        '&&', '||', '..', '...',
        '?', ':',
        '=', '**=', '*=', '/=', '%=', '+=', '-=',
        '<<=', '>>=',
        '&&=', '&=', '||=', '|=', '^=',
        'not', 'and', 'or',
        '.', ',', '(', ')', '[', ']', '{', '}'
      ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'BEGIN', 'END', 'alias', 'begin', 'break', 'case', 'class',
      'def', 'defined?', 'do', 'else', 'elsif', 'end', 'ensure',
      'false', 'for', 'if', 'in', 'module', 'next', 'nil', 'redo',
      'rescue', 'retry', 'return', 'self', 'super', 'then', 'true',
      'undef', 'unless', 'until', 'when', 'while', 'yield'
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
      symbol_tb,
      known_operator_tb,
      identifier_tb,
      string_tb,
      heredoc_tb,
      hash_comment_tb,
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
