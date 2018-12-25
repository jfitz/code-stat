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
    real_tb = RealTokenBuilder(False, False)
    number_tb = NumberTokenBuilder()
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"])

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()
    c_preprocessor_tb = CPreProcessorTokenBuilder()

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

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    unknown_operators = set(self.common_operators()) - set(known_operators)
    unknown_operator_tb = ListTokenBuilder(unknown_operators, 'invalid operator', True)

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
      real_tb,
      number_tb,
      keyword_tb,
      identifier_tb,
      string_tb,
      known_operator_tb,
      unknown_operator_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      c_preprocessor_tb
    ]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = tokenizer.tokenize(code)

    found_keywords = self.find_keywords(self.tokens)
    found_cpp_keywords = self.find_specific_keywords(self.tokens, cpp_keywords)

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
    keyword_confidence = len(found_keywords) / len(keywords)

    # unknown tokens reduce confidence
    token_confidence = 1
    if len(self.tokens) > 0:
      token_confidence = num_known_tokens / len(self.tokens)

    # unknown operators reduce confidence
    operator_confidence = 1
    num_operators = num_known_operators + num_invalid_operators
    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    # C++ keywords reduce confidence
    cpp_keyword_confidence = 1.0 - len(found_cpp_keywords) / len(cpp_keywords)

    # compute confidence
    self.confidence = brace_match_confidence * token_confidence * operator_confidence * cpp_keyword_confidence
    self.confidences = {
      'brace_match': brace_match_confidence,
      'keyword': keyword_confidence,
      'token': token_confidence,
      'operator': operator_confidence,
      'cpp_keyword': cpp_keyword_confidence
      }
