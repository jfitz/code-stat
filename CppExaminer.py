import string
import math
from Token import Token
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder
)
from CXTokenBuilders import (
  IdentifierTokenBuilder,
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder,
  CPreProcessorTokenBuilder
)
from Tokenizer import Tokenizer

class CppExaminer(Examiner):
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
    string_tb = StringTokenBuilder(['"', "'"], False)

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
      '.', ':',
      '++', '--', '->', '&&', '||',
      '?', '{', '}',
      '::', '<=>', '.*', '->*',
      'new', 'delete', 'and', 'and_eq', 'bitand', 'bitor', 'compl',
      'not', 'not_eq', 'or', 'or_eq', 'xor', 'xor_eq'
    ]

    unary_operators = [
      '+', '-', '*',
      '!', '&', '~',
      '++', '--'
    ]

    postfix_operators = [
      '++', '--', '&'
    ]

    groupers = ['(', ')', ',', '[', ']']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'alignas', 'alignof', 'asm', 'atomic_cancel',
      'atomic_commit', 'atomic_noexcept', 'auto',
      'bool', 'break', 'case', 'catch', 'char', 'char8_t', 'char16_t',
      'char32_t', 'class', 'concept', 'const', 'consteval',
      'constexpr', 'const_cast', 'continue', 'co_await', 'co_return',
      'co_yield', 'decltype', 'default', 'do', 'double',
      'dynamic_cast', 'else', 'enum', 'explicit', 'export', 'extern',
      'false', 'float', 'for', 'friend', 'goto', 'if', 'import',
      'inline', 'int', 'long', 'module', 'mutable', 'namespace',
      'noexcept', 'nullptr', 'operator',
      'private', 'protected', 'public', 'reflexpr', 'register',
      'reinterpret_cast', 'requires', 'return', 'short', 'signed',
      'sizeof', 'static', 'static_assert', 'static_cast', 'struct',
      'switch', 'synchronized', 'template', 'this', 'thread_local',
      'throw', 'true', 'try', 'typedef', 'typeid', 'typename', 'union',
      'unsigned', 'using', 'virtual', 'void', 'volatile', 'wchar_t',
      'while', 'cout', 'cin',
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
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      c_preprocessor_tb,
      self.unknown_operator_tb
    ]

    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = tokenizer.tokenize(code)
      
    num_invalid_operators = self.count_invalid_operators(self.tokens)
    num_known_operators = self.count_known_operators(self.tokens)

    # consider the number of matches for begin/end
    ok, num_begin, num_end = self.check_paired_tokens(self.tokens, ['{'], ['}'])
    num_begin_end = num_begin + num_end
    brace_match_confidence = 0.0

    if num_begin_end > 0:
      brace_match_confidence = (num_begin + num_end) / num_begin_end

    if not ok:
      brace_match_confidence *= 0.75

    self.calc_token_confidence()

    # unknown operators reduce confidence
    operator_confidence_1 = 1.0
    operator_confidence_2 = 1.0
    num_operators = num_known_operators + num_invalid_operators

    if num_operators > 0:
      operator_confidence_1 = num_known_operators / num_operators
      errors = 0
      prev_token = Token('\n', 'newline')

      for token in self.tokens:
        if token.group == 'operator' and\
          prev_token.group == 'operator' and\
          prev_token.text not in postfix_operators and\
          token.text not in unary_operators:
          errors += 1

        prev_token = token

      operator_confidence_2 = 1.0 - errors / num_operators

    # absence of power keywords reduces confidence
    power_keywords_found = {}
    for token in self.tokens:
      if token.group == 'keyword' and token.text in power_keywords:
        power_keywords_found[token.text] = True

    # ratio = len(power_keywords_found) / len(power_keywords)
    # power_keyword_confidence = ratio ** (1.0 / 3.0)

    self.confidences['brace_match'] = brace_match_confidence
    self.confidences['operator_1'] = operator_confidence_1
    self.confidences['operator_2'] = operator_confidence_2
