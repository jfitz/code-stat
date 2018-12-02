import string
import math
from Examiner import Examiner
from TokenBuilders import *
from CXTokenBuilders import *
from CppTokenBuilders import *
from Tokenizer import Tokenizer

class CppExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    wtb = WhitespaceTokenBuilder()
    nltb = NewlineTokenBuilder()

    ntb = NumberTokenBuilder()
    itb = IdentifierTokenBuilder()
    stb = StringTokenBuilder()

    sctb = SlashCommentTokenBuilder()
    ctb = CommentTokenBuilder()
    cpptb = CPreProcessorTokenBuilder()

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '&', '|', '~', '<<', '>>',
      '^',
      '(', ')', ',', '.', ':', ';',
      '[', ']',
      '++', '--', '->', '&&', '||',
      '::', '?', '{', '}'
      ]
    
    kotb = ListTokenBuilder(known_operators, 'operator', True)

    unknown_operators = set(self.common_operators()) - set(known_operators)
    uotb = ListTokenBuilder(unknown_operators, 'invalid operator', True)

    nltb = NewlineTokenBuilder()

    keywords = [
      'int', 'char', 'float', 'double',
      'signed', 'unsigned', 'short', 'long',
      'typedef', 'enum',
      'auto', 'extern', 'register', 'static',
      'for', 'while', 'do',
      'const', 'volatile', 'sizeof',
      'if', 'else', 'switch', 'case', 'default',
      'struct', 'union', 'void', 'return',
      'goto', 'continue', 'break',
      'new', 'delete', 'explicit', 'mutable',
      'private', 'protected', 'public',
      'true', 'false',
      'cin', 'cout',
      'bool', 'class', 'friend', 'operator',
      'try', 'catch', 'throw'
      ]

    ktb = ListTokenBuilder(keywords, 'keyword', True)

    power_keywords = [
      'private', 'protected', 'public',
      'true', 'false',
      'cin', 'cout',
      'bool', 'class', 'friend', 'operator',
      'try', 'catch', 'throw'
      ]
    
    tokenbuilders = [wtb, nltb, ntb, ktb, itb, stb, kotb, uotb, sctb, ctb, cpptb, nltb]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    num_keywords = 0
    found_keywords = {}
    num_power_keywords = 0
    found_power_keywords = {}

    self.tokens = tokenizer.tokenize(code)
    for token in self.tokens:
      token_lower = str(token).lower()
      
      # count keywords
      if token_lower in keywords:
        num_keywords += 1
        found_keywords[token_lower] = True

      # count power keywords
      if token_lower in power_keywords:
        num_power_keywords += 1
        found_power_keywords[token_lower] = True

    num_known_tokens = self.count_valid_tokens(self.tokens)
    num_invalid_operators = self.count_invalid_operators(self.tokens)
    num_known_operators = self.count_known_operators(self.tokens)

    # consider the number of matches for begin/end
    ok, num_begin, num_end = self.check_paired_tokens(self.tokens, '{', '}')
    num_begin_end = num_begin + num_end
    brace_match_confidence = 0
    if num_begin_end > 0:
      brace_match_confidence = (num_begin + num_end) / num_begin_end

    # recognized keywords improve confidence
    keyword_confidence = 0
    if num_keywords > 0:
      keyword_confidence = len(found_keywords) / len(keywords)

    #  unknown tokens reduce confidence
    token_confidence = 1
    if len(self.tokens) > 0:
      token_confidence = num_known_tokens / len(self.tokens)

    #  unknown operators reduce confidence
    operator_confidence = 1
    num_operators = num_known_operators + num_invalid_operators
    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    # compute confidence
    self.confidence = brace_match_confidence * token_confidence * operator_confidence
    self.confidences = {
      'keyword_confidence': keyword_confidence,
      'brace_match': brace_match_confidence,
      'token': token_confidence,
      'operator': operator_confidence
    }
