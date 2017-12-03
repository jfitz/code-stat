import string
import math
from TokenBuilders import *
from CppTokenBuilders import *
from Tokenizer import Tokenizer

class CppExaminer:
  def __init__(self, code):
    lines = code.split('\n')

    num_tokens = 0
    num_known_tokens = 0
    num_operators = 0
    num_known_operators = 0

    known_keywords = [
      'bool', 'int', 'char', 'float', 'double',
      'signed', 'unsigned', 'short', 'long',
      'class', 'typedef', 'enum', 'friend', 'operator',
      'auto', 'extern', 'register', 'static', 'const',
      'for', 'while', 'do', 'try', 'catch', 'throw',
      'const', 'volatile', 'sizeof',
      'if', 'else', 'switch', 'case', 'default',
      'true', 'false',
      'goto', 'continue', 'break',
      'struct', 'union', 'void', 'return',
      'new', 'delete', 'explicit', 'mutable',
      'private', 'protected', 'public',
      '#define', '#include'
      ]
    power_keywords = [
      'bool', 'class', 'typedef', 'friend', 'operator',
      'const', 'volatile', 'sizeof',
      '#define', '#include',
      'printf', 'scanf'
      ]
    all_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '<>', '>', '>=', '<', '<=',
      'and', 'and', 'or', 'not',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '&', '|', '~', '<<', '>>',
      ':=', '^',
      '(', ')', ',', '.', ':', ';',
      '[', ']', '..',
      '++', '--', '**', '->', '<->', '<=>', '@', '&&', '||'
      '\\', '::', '?', '{', '}'
      ]
    wtb = WhitespaceTokenBuilder()
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
      '++', '--', '->', '&&', '||'
      '::', '?', '{', '}'
      ]
    kotb = ListTokenBuilder(known_operators, 'operator')
    unknown_operators = set(all_operators) - set(known_operators)
    uotb = ListTokenBuilder(unknown_operators, 'invalid operators')
    nltb = NewlineTokenBuilder()
    
    tokenbuilders = [wtb, ntb, itb, stb, kotb, uotb, sctb, ctb, nltb]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    first_token = ''
    last_token = ''
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
        
      if first_token == '' and not token.whitespace() and not token.comment():
        first_token = token
        
      if not token.whitespace() and not token.comment():
        last_token = token
        
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
      if token_lower in known_keywords:
        num_keywords += 1
        found_keywords[token_lower] = True

      # count power keywords
      if token_lower in power_keywords:
        num_power_keywords += 1
        found_power_keywords[token_lower] = True

    # consider the number of matches for begin/end
    confidence_1 = 1
    num_begin_end = num_begin + num_end
    if num_begin_end > 0:
      confidence_1a = num_begin / num_begin_end
      confidence_1b = num_end / num_begin_end
      confidence_1 = confidence_1a + confidence_1b

    # recognized keywords improve confidence
    confidence_2 = 0
    if num_keywords > 0:
      ratio = len(found_keywords) / len(known_keywords)
      confidence_2 = math.sqrt(ratio)

    #  unknown tokens reduce confidence
    confidence_3 = 1
    if num_tokens > 0:
      confidence_3 = num_known_tokens / num_tokens

    #  unknown operators reduce confidence
    confidence_4 = 1
    if num_operators > 0:
      confidence_4 = num_known_operators / num_operators

    boost = 0
    if num_power_keywords > 0:
      boost = num_power_keywords / len(power_keywords)

    # compute confidence
    confidence = confidence_1 * confidence_2 * confidence_3 * confidence_4
    confidence = confidence + ((1 - confidence) * boost)
    self.confidence = confidence
