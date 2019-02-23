import string
import math
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
  IdentifierTokenBuilder,
  ListTokenBuilder
)
from CXTokenBuilders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder
)
from Tokenizer import Tokenizer

class JavaExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False, False)

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    terminators_tb = ListTokenBuilder([';'], 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '~', '<<', '>>', '>>>', '>>>=',
      '^',
      '.', ':',
      '++', '--', '&&', '||',
      '?'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-',
      '!', '~',
      '++', '--'
    ]

    self.postfix_operators = [
      '++', '--', ':'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'abstract', 'assert', 'boolean', 'break', 'byte',
      'case', 'catch', 'char', 'class', 'const',
      'continue', 'default', 'do', 'double',
      'else', 'enum', 'extends', 'false', 'final',
      'finally', 'float', 'for', 'goto',
      'if', 'implements', 'import', 'instanceof', 'int', 'interface',
      'long', 'native', 'new', 'null', 'package',
      'private', 'protected', 'public',
      'return', 'short',
      'static', 'strictfp', 'super', 'switch', 'synchronized',
      'this', 'throw', 'throws', 'true', 'transient', 'try',
      'void', 'volatile', 'while'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      terminators_tb,
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
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # do not check for two operands in a row
    self.calc_paired_blockers_confidence(['{'], ['}'])
