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
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      newline_tb,
      keyword_tb,
      identifier_tb,
      symbol_tb,
      string_tb,
      known_operator_tb,
      self.unknown_operator_tb,
      heredoc_tb,
      hash_comment_tb
    ]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = tokenizer.tokenize(code)

    found_keywords = self.find_keywords(self.tokens)
    found_identifiers = self.find_identifiers(self.tokens)

    num_known_tokens = self.count_valid_tokens(self.tokens)
    num_invalid_operators = self.count_invalid_operators(self.tokens)
    num_known_operators = self.count_known_operators(self.tokens)

    # recognized keywords improve confidence
    self.keyword_confidence = 0.0

    if len(found_keywords) > 0:
      self.keyword_confidence = 1.0

    if len(found_identifiers) > 0:
      self.keyword_confidence = len(found_keywords) / len(found_identifiers)

    # unknown tokens reduce confidence
    self.token_confidence = 1.0

    if len(self.tokens) > 0:
      self.token_confidence = num_known_tokens / len(self.tokens)

    # unknown operators reduce confidence
    self.operator_confidence = 1.0
    num_operators = num_known_operators + num_invalid_operators

    if num_operators > 0:
      self.operator_confidence = num_known_operators / num_operators


  def confidence(self):
    return self.token_confidence * self.operator_confidence


  def confidences(self):
    return {
      'token': self.token_confidence,
      'keyword': self.keyword_confidence,
      'operator': self.operator_confidence
    }
