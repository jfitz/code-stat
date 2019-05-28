import string
import math
from Token import Token
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
  ListTokenBuilder
)
from CXTokenBuilders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder,
  CPreProcessorTokenBuilder
)
from ObjectiveCTokenBuilders import (
  DirectiveTokenBuilder
)
from Tokenizer import Tokenizer

class ObjectiveCExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    identifier_tb = IdentifierTokenBuilder()
    directive_tb = DirectiveTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False, False)
    prefixed_string_tb = PrefixedStringTokenBuilder('@', False, ['"'])

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    directives = (
      '#define', '#undef',
      '#ifdef', '#ifndef', '#if', '#endif', '#else', '#elif',
      '#import', '#line', '#error', '#include', '#pragma'
    )

    c_preprocessor_tb = CPreProcessorTokenBuilder(directives)

    terminators_tb = ListTokenBuilder([';'], 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '<<', '>>',
      '.', ':',
      '++', '--', '&&', '||', '^',
      '?'
    ]

    self.unary_operators = [
      '+', '-', '*',
      '!', '&', '^',
      '++', '--'
    ]

    self.postfix_operators = [
      '++', '--', '&'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'auto', 'break', 'case', 'char', 'const', 'continue', 'default',
      'do', 'double', 'else', 'enum', 'extern', 'float', 'for', 'goto',
      'if', 'inline', 'int', 'long', 'register', 'restrict', 'return',
      'short', 'signed', 'sizeof', 'static', 'struct', 'switch', 'typedef',
      'union', 'unsigned', 'void', 'volatile', 'while',
      '_Bool', '_Complex', '_Imaginary',
      'BOOL', 'Class', 'bycopy', 'byref', 'id', 'IMP', 'in', 'inout', 'nil',
      'NO', 'NULL', 'oneway', 'out', 'Protocol', 'SEL', 'self', 'super',
      'YES', '@interface', '@end', '@implementation', '@protocol', '@class',
      '@public', '@protected', '@private', '@property', '@try', '@throw',
      '@catch()', '@finally', '@synthesize', '@dynamic', '@selector',
      'atomic', 'nonatomic', 'retain'
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
      groupers_tb,
      known_operator_tb,
      directive_tb,
      identifier_tb,
      string_tb,
      prefixed_string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
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
    operand_types = ['number', 'string', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
