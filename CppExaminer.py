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
      '?', '{', '}',
      '::', '<=>', '.*', '->*'
      ]
    
    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'alignas', 'alignof', 'and', 'and_eq', 'asm', 'atomic_cancel',
      'atomic_commit', 'atomic_noexcept', 'auto', 'bitand', 'bitor',
      'bool', 'break', 'case', 'catch', 'char', 'char8_t', 'char16_t',
      'char32_t', 'class', 'compl', 'concept', 'const', 'consteval',
      'constexpr', 'const_cast', 'continue', 'co_await', 'co_return',
      'co_yield', 'decltype', 'default', 'delete', 'do', 'double',
      'dynamic_cast', 'else', 'enum', 'explicit', 'export', 'extern',
      'false', 'float', 'for', 'friend', 'goto', 'if', 'import',
      'inline', 'int', 'long', 'module', 'mutable', 'namespace', 'new',
      'noexcept', 'not', 'not_eq', 'nullptr', 'operator', 'or', 'or_eq',
      'private', 'protected', 'public', 'reflexpr', 'register',
      'reinterpret_cast', 'requires', 'return', 'short', 'signed',
      'sizeof', 'static', 'static_assert', 'static_cast', 'struct',
      'switch', 'synchronized', 'template', 'this', 'thread_local',
      'throw', 'true', 'try', 'typedef', 'typeid', 'typename', 'union',
      'unsigned', 'using', 'virtual', 'void', 'volatile', 'wchar_t',
      'while', 'xor', 'xor_eq', 'cout', 'cin',
      'override', 'axiom', 'final', 'audit', 'transaction_safe',
      'transaction_safe_dynamic'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    power_keywords = [
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
      c_preprocessor_tb,
      newline_tb
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

    # unknown tokens reduce confidence
    token_confidence = 1.0

    if len(self.tokens) > 0:
      token_confidence = num_known_tokens / len(self.tokens)

    # unknown operators reduce confidence
    operator_confidence = 1.0
    num_operators = num_known_operators + num_invalid_operators

    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    # absence of power keywords reduces confidence
    power_keywords_found = {}
    for token in self.tokens:
      if token.group == 'keyword' and token.text in power_keywords:
        power_keywords_found[token.text] = True

    ratio = len(power_keywords_found) / len(power_keywords)
    power_keyword_confidence = ratio ** (1.0 / 3.0)

    self.confidences = {
      'brace_match': brace_match_confidence,
      'token': token_confidence,
      'operator': operator_confidence,
      'power_keyword': power_keyword_confidence
    }
