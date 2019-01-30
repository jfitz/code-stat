import string
import math
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  PrefixedStringTokenBuilder,
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

class CsharpExaminer(Examiner):
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
    prefixed_string_tb = PrefixedStringTokenBuilder('@', ['"'])

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    directives = (
      '#if', '#else', '#elif', '#endif',
      '#define', '#undef', '#warning', '#error'
      '#line', '#region', '#endregion', '#pragma'
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
      '?', '??', '?.', '?[', '{', '}',
      '=>',
      'as', 'is', 'await', 'sizeof', 'delegate', 'default',
      'checked', 'unchecked', 'typeof', 'new'
    ]
    
    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    groupers = ['(', ')', ',', '[', ']']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'abstract', 'base', 'bool', 'break', 'byte',
      'case', 'catch', 'char', 'class', 'const',
      'continue', 'decimal', 'do', 'double',
      'else', 'enum', 'event', 'explicit', 'extern', 'false',
      'finally', 'fixed', 'float', 'for', 'foreach', 'goto',
      'if', 'implicit', 'in', 'int', 'interface', 'internal',
      'lock', 'long', 'namespace', 'null', 'object', 'operator',
      'out', 'override', 'params', 'private', 'protected', 'public',
      'readonly', 'ref', 'return', 'sbyte', 'sealed', 'short',
      'stackalloc', 'static', 'string', 'struct', 'switch',
      'this', 'throw', 'true', 'try', 'uint', 'ulong',
      'unsafe', 'ushort', 'using', 'using static',
      'virtual', 'void', 'volatile', 'while'
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
      groupers_tb,
      identifier_tb,
      string_tb,
      prefixed_string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      c_preprocessor_tb,
      self.unknown_operator_tb
    ]

    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = tokenizer.tokenize(code)

    # consider the number of matches for begin/end
    ok, num_begin, num_end = self.check_paired_tokens(self.tokens, ['{'], ['}'])
    num_begin_end = num_begin + num_end
    brace_match_confidence = 0.0

    if num_begin_end > 0:
      brace_match_confidence = (num_begin + num_end) / num_begin_end

    if not ok:
      brace_match_confidence *= 0.75

    self.calc_token_confidence()
    self.calc_operator_confidence()

    self.confidences['brace_match'] = brace_match_confidence
