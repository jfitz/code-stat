import string
import math
from Examiner import Examiner
from TokenBuilders import *
from CXTokenBuilders import *
from CTokenBuilders import *
from Tokenizer import Tokenizer

class CExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False)
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"])

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    directives = (
      '#define', '#undef',
      '#ifdef', '#ifndef', '#if', '#endif', '#else', '#elif',
      '#line', '#error', '#include', '#pragma'
    )

    c_preprocessor_tb = CPreProcessorTokenBuilder(directives)

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '~', '<<', '>>',
      '^',
      '(', ')', ',', '.', ':', ';',
      '[', ']',
      '++', '--', '->', '&&', '||',
      '?', '{', '}'
      ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'int', 'char', 'float', 'double',
      'signed', 'unsigned', 'short', 'long',
      'typedef', 'enum',
      'auto', 'extern', 'register', 'static',
      'for', 'while', 'do',
      'const', 'volatile', 'sizeof',
      'if', 'else', 'switch', 'case', 'default',
      'struct', 'union', 'void', 'return',
      'goto', 'continue', 'break'
      ]
    
    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    cpp_keywords = [
      'private', 'protected', 'public',
      'true', 'false',
      'cin', 'cout',
      'bool', 'class', 'friend', 'operator',
      'try', 'catch', 'throw'
      ]
    
    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      identifier_tb,
      string_tb,
      known_operator_tb,
      self.unknown_operator_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      c_preprocessor_tb
    ]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = tokenizer.tokenize(code)

    found_keywords = self.find_keywords(self.tokens)
    found_cpp_keywords = self.find_specific_keywords(self.tokens, cpp_keywords)
    found_identifiers = self.find_identifiers(self.tokens)

    num_known_tokens = self.count_valid_tokens(self.tokens)
    num_invalid_operators = self.count_invalid_operators(self.tokens)
    num_known_operators = self.count_known_operators(self.tokens)

    # consider the number of matches for begin/end
    ok, num_begin, num_end = self.check_paired_tokens(self.tokens, '{', '}')
    num_begin_end = num_begin + num_end
    self.brace_match_confidence = 0.0

    if num_begin_end > 0:
      self.brace_match_confidence = (num_begin + num_end) / num_begin_end

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
    self.operator_confidence = 1
    num_operators = num_known_operators + num_invalid_operators

    if num_operators > 0:
      self.operator_confidence = num_known_operators / num_operators

    # C++ keywords reduce confidence
    self.cpp_keyword_confidence = 1.0 - len(found_cpp_keywords) / len(cpp_keywords)


  def confidence(self):
    return self.brace_match_confidence * self.token_confidence * self.operator_confidence * self.cpp_keyword_confidence


  def confidences(self):
    return {
      'brace_match': self.brace_match_confidence,
      'token': self.token_confidence,
      'keyword': self.keyword_confidence,
      'operator': self.operator_confidence,
      'cpp_keyword': self.cpp_keyword_confidence
    }
