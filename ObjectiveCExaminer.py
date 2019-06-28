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
  ListTokenBuilder,
  SingleCharacterTokenBuilder
)
from CXTokenBuilders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder,
  ClassTypeTokenBuilder
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
    integer_exponent_tb = IntegerExponentTokenBuilder(False)
    real_tb = RealTokenBuilder(False, False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', False)
    identifier_tb = IdentifierTokenBuilder()
    directive_tb = DirectiveTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False, False)
    prefixed_string_tb = PrefixedStringTokenBuilder('@', False, ['"'])

    class_type_tb = ClassTypeTokenBuilder()

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    directives = (
      '#define', '#undef',
      '#ifdef', '#ifndef', '#if', '#endif', '#else', '#elif',
      '#import', '#line', '#error', '#include', '#pragma'
    )

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation')
    c_preprocessor_tb = ListTokenBuilder(directives, 'preprocessor', True)
    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '<<', '>>', '~',
      '.', ':', '->',
      '++', '--', '&&', '||', '^',
      '?', '##'
    ]

    self.unary_operators = [
      '+', '-', '*',
      '!', '&', '^',
      '++', '--'
    ]

    self.postfix_operators = [
      '++', '--', '&', '->'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'break', 'case', 'continue', 'default',
      'do', 'else', 'for', 'goto',
      'if', 'inline', 'restrict', 'return',
      'sizeof', 'switch', 'typedef', 'while',
      'bycopy', 'byref', 'id', 'IMP', 'in', 'inout',
      'oneway', 'out', 'Protocol', 'SEL',
      '@interface', '@end', '@implementation', '@protocol', '@class',
      '@public', '@protected', '@private', '@property', '@try', '@throw',
      '@catch()', '@finally', '@synthesize', '@dynamic', '@selector',
      'atomic', 'nonatomic', 'retain'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'auto', 'char', 'const', 'double', 'enum', 'extern',
      'float', 'int', 'long', 'register',
      'short', 'signed', 'static', 'struct', 'union', 'unsigned', 'void',
      'volatile', '_Bool', '_Complex', '_Imaginary', 'BOOL', 'Class', 
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'self', 'super', 'nil', 'YES', 'NO', 'NULL'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      types_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      directive_tb,
      identifier_tb,
      class_type_tb,
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
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
