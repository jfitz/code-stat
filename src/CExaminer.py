import string
import math
from Examiner import Examiner
from TokenBuilders import *
from CTokenBuilders import *
from Tokenizer import Tokenizer

class CExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    lines = code.split('\n')

    num_tokens = 0
    num_known_tokens = 0
    num_operators = 0
    num_known_operators = 0

    wtb = WhitespaceTokenBuilder()
    nltb = NewlineTokenBuilder()

    ntb = NumberTokenBuilder()
    itb = IdentifierTokenBuilder()
    stb = StringTokenBuilder()

    sctb = SlashCommentTokenBuilder()
    ctb = CommentTokenBuilder()

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '&', '|', '~', '<<', '>>',
      '^',
      '(', ')', ',', '.', ':', ';',
      '[', ']',
      '++', '--', '->', '&&', '||',
      '?', '{', '}'
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
      'goto', 'continue', 'break',
      'struct', 'union', 'void', 'return',
      '#define', '#include'
      ]
    
    ktb = ListTokenBuilder(keywords, 'keyword', True)

    power_keywords = [
      'typedef',
      'auto', 'extern', 'register',
      'const', 'volatile', 'sizeof',
      '#define', '#include',
      'printf', 'scanf'
      ]
    
    tokenbuilders = [wtb, nltb, ntb, itb, stb, kotb, uotb, sctb, ctb, nltb, ktb]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    num_begin = 0
    num_end = 0
    num_keywords = 0
    found_keywords = {}
    num_power_keywords = 0
    found_power_keywords = {}

    tokens = tokenizer.tokenize(code)
    for token in tokens:
      token_lower = str(token).lower()
      
      num_tokens += 1
      
      if not token.group.startswith('invalid'):
        num_known_tokens += 1
        
      # count 'begin' and 'end' keywords for matches
      if token_lower == '{':
        num_begin += 1
      if token_lower == '}':
        num_end += 1

      # count operators
      if token.group == 'operator' or token.group == 'invalid operator':
        num_operators += 1
      if token.group == 'operator':
        num_known_operators += 1

      # count keywords
      if token_lower in keywords:
        num_keywords += 1
        found_keywords[token_lower] = True

      # count power keywords
      if token_lower in power_keywords:
        num_power_keywords += 1
        found_power_keywords[token_lower] = True

    # consider the number of matches for begin/end
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
    if num_tokens > 0:
      token_confidence = num_known_tokens / num_tokens

    #  unknown operators reduce confidence
    operator_confidence = 1
    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    # compute confidence
    self.confidence = brace_match_confidence * token_confidence * operator_confidence
    self.confidences = {
      'brace_match': brace_match_confidence,
      'keyword': keyword_confidence,
      'token': token_confidence,
      'operator': operator_confidence
      }
    self.tokens = tokens
