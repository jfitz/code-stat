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
  IdentifierTokenBuilder,
  ListTokenBuilder,
  ParenStarCommentTokenBuilder,
  TripleQuoteCommentTokenBuilder
)
from CXTokenBuilders import (
  SlashSlashCommentTokenBuilder,
  TripleSlashCommentTokenBuilder,
  CPreProcessorTokenBuilder,
  ClassTypeTokenBuilder
)
from Tokenizer import Tokenizer

# `` as identifier delimiters
# <x> as class

class FsharpExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    identifier_tb = IdentifierTokenBuilder()
    class_type_tb = ClassTypeTokenBuilder()
    string_tb = StringTokenBuilder(['"'], False, False)
    triple_quote_string_tb = TripleQuoteCommentTokenBuilder()
    prefixed_string_tb = PrefixedStringTokenBuilder('@', False, ['"'])

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    parens_star_comment_tb = ParenStarCommentTokenBuilder()
    triple_slash_comment_tb = TripleSlashCommentTokenBuilder()

    directives = (
      '#if', '#else', '#elif', '#endif',
      '#define', '#undef', '#warning', '#error'
      '#line', '#region', '#endregion', '#pragma'
    )

    c_preprocessor_tb = CPreProcessorTokenBuilder(directives)

    known_operators = [
      'and', 'as', 'in', 'new', 'not', 'of', 'or', 'when', 
      '!', '!=', '%', '%%', '%?', '&', '&&', '&&&',
      '(|', '|)', '*', '*?', '**', '+', '+?', '-', '-?',
      '->', '..', '.', '.. ..',
      '/', '/?', ':', '::', ':=', ':/',
      '<', '<<', '<<<', '<-', '<>', '<>?', '<=', '<=?',
      '<|', '<||', '<|||',
      '<@', '@>', '<@@', '@@>',
      '=', '=?', '==', '>', '>?', '>>', '>>>', '>=', '>=?',
      '?', '||', '|||', '^^^',
      '?>=', '?>', '?<=', '?<', '?=', '?<>', '?+', '?-', '?*', '?/',
      '>=?', '>?', '<=?', '<?', '=?', '<>?', '+?', '-?', '*?', '/?',
      '?>=?', '?>?', '?<=?', '?<?', '?=?', '?<>?', '?+?', '?-?', '?*?', '?/?',
      '@', '|>', '||>', '|||>',
      '~~', '~~~', '~-', '~+',
      ':>', ':?>', "'"
    ]
    
    self.unary_operators = [
      'new', 'not', "'", '-'
    ]

    self.postfix_operators = [
      "'"
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    groupers = [
      '(', ')', ',', '[', ']', '{', '}',
      'begin', 'end', ';',
      '[|', '|]', '[<', '>]',
      '^',
      '|'
    ]

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'abstract', 'assert', 'break', 'class', 'default', 'delegate',
      'do', 'done', 'downcast', 'downto', 'elif', 'else', 'exception',
      'extern', 'finally', 'fixed', 'for', 'fun', 'function',
      'global', 'if', 'inherit', 'inline', 'interface', 'internal',
      'lazy', 'let', 'let!', 'match', 'match!', 'member', 'module',
      'mutable', 'namespace', 'open', 'override',
      'private', 'public', 'rec', 'return', 'return!', 'struct',
      'then', 'to', 'try', 'type', 'upcast', 'use', 'use!',
      'val', 'while', 'with', 'yield', 'yield!'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'bool', 'byte', 'char', 'decimal', 'double', 'float', 'int', 'long', 'object',
      'sbyte', 'short', 'string', 'uint', 'ulong', 'ushort', 'void'
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'base', 'false', 'null', 'true', '_'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      types_tb,
      values_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      class_type_tb,
      string_tb,
      triple_quote_string_tb,
      prefixed_string_tb,
      triple_slash_comment_tb,
      slash_slash_comment_tb,
      parens_star_comment_tb,
      c_preprocessor_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence()
    # self.calc_operand_confidence()
    # self.calc_value_value_confidence()
    # self.calc_value_value_different_confidence()
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
