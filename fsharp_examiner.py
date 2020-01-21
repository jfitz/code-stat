import string
import math

from codestat_tokenizer import Tokenizer
from token_builders import (
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
  TripleQuoteStringTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  TripleSlashCommentTokenBuilder,
  ClassTypeTokenBuilder
)
from fsharp_token_builders import (
  FsharpCharTokenBuilder
)
from examiner import Examiner

class FsharpExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StringTokenBuilder.__escape_z__()
    PrefixedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    ParenStarCommentTokenBuilder.__escape_z__()
    TripleQuoteStringTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    TripleSlashCommentTokenBuilder.__escape_z__()
    ClassTypeTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(True, True, None)
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E', None)
    identifier_tb = IdentifierTokenBuilder()
    class_type_tb = ClassTypeTokenBuilder()
    quotes = ['"']
    string_tb = StringTokenBuilder(quotes, False, False, False)
    triple_quote_string_tb = TripleQuoteStringTokenBuilder(quotes)
    prefixed_string_tb = PrefixedStringTokenBuilder('@', False, quotes)
    char_tb = FsharpCharTokenBuilder(["'"])

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    parens_star_comment_tb = ParenStarCommentTokenBuilder()
    triple_slash_comment_tb = TripleSlashCommentTokenBuilder()

    directives = (
      '#if', '#else', '#elif', '#endif',
      '#define', '#undef', '#warning', '#error'
      '#line', '#region', '#endregion', '#pragma'
    )

    preprocessor_tb = ListTokenBuilder(directives, 'preprocessor', True)

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

    group_ends = [')', ']', '}', '|]', '>]']

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
      newline_tb,
      whitespace_tb,
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
      char_tb,
      triple_slash_comment_tb,
      slash_slash_comment_tb,
      parens_star_comment_tb,
      preprocessor_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
