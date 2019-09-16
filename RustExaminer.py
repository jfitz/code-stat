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
  CharTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  PrefixedIntegerTokenBuilder,
  IdentifierTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder
)
from CXTokenBuilders import (
  SlashSlashCommentTokenBuilder,
  NestedSlashStarCommentTokenBuilder,
  ClassTypeTokenBuilder
)
from RustTokenBuilders import (
  RustRawStringTokenBuilder,
  RustAttributeTokenBuilder
)
from Tokenizer import Tokenizer

class RustExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation')

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    real_tb = RealTokenBuilder(False, False, '_')
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', '_')
    octal_integer_tb = PrefixedIntegerTokenBuilder('0o', True, '01234567')
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', True, '0123456789ABCDEFabcdef')
    identifier_tb = IdentifierTokenBuilder()
    attribute_tb = RustAttributeTokenBuilder()
    string_tb = StringTokenBuilder(['"'], False, False)
    bstring_tb = PrefixedStringTokenBuilder('b', True, ['"', "'"])
    rstring_tb = RustRawStringTokenBuilder()
    char_tb = CharTokenBuilder("'")

    class_type_tb = ClassTypeTokenBuilder()

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = NestedSlashStarCommentTokenBuilder()

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

    known_operators = [
      '+', '-', '*', '/', '%', '^', '!', '&', '|',
      '&&', '||', '<<', '>>',
      '+=', '-=', '*=', '/=', '%=', '^=', '&=', '|-', '<<=', '>>=',
      '=', '==', '!=', '>', '<', '>=', '<=',
      '@', '.', '..', '...', '->', '#', '$', '?',
      'in'
    ]

    self.unary_operators = [
      '+', '-', '*',
      '!', '&',
    ]

    self.postfix_operators = [
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':', '::', '=>']
    group_ends = [')', ']', '}', ')|']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'as',
      'break',
      'const',
      'continue',
      'crate'
      'else',
      'enum',
      'extern',
      'fn',
      'for',
      'if',
      'impl',
      'let',
      'loop',
      'match',
      'mod',
      'move',
      'mut',
      'pub',
      'ref',
      'return',
      'static',
      'struct',
      'trait',
      'type',
      'unsafe',
      'use',
      'where',
      'while'
    ]

    keywords_2018 = [
      'dyn',
      'union',
      'static'
    ]

    keywords_future = [
      'abstract',
      'become',
      'box',
      'do',
      'final',
      'macro',
      'override',
      'priv',
      'typeof',
      'unsized',
      'virtual',
      'yield',
      'async',
      'await',
      'try'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'Self',
      'u8', 'i8',
      'u16', 'i16',
      'u32', 'i32',
      'u64', 'i64',
      'u128', 'i128',
      'usize', 'isize'
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'self', 'true', 'false', 'super', '_'
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
      octal_integer_tb,
      hex_integer_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      types_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      class_type_tb,
      attribute_tb,
      string_tb,
      bstring_tb,
      rstring_tb,
      char_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.convert_bars_to_groups()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_format_confidence()
    self.calc_statistics()


  def calc_line_format_confidence(self):
    drop_types = ['whitespace', 'comment', 'line continuation']
    tokens = self.drop_tokens(self.tokens, drop_types)

    line_bracket_count = 0
    num_bracket_count = 0
    prev2_token = Token('\n', 'newline')
    prev_token = Token('\n', 'newline')
    for token in tokens:
      if token.group == 'group' and token.text == '{':
        num_bracket_count += 1

        if prev_token.group == 'newline' and\
          (prev2_token.group != 'group' or prev2_token.text != '{'):
          line_bracket_count += 1
          self.errors.append({
            'TYPE': 'LINE FORMAT',
            'TOKEN': token.text
            })

      prev2_token = prev_token
      prev_token = token

    line_format_confidence = 1.0

    if num_bracket_count > 0:
      line_format_confidence = 1.0 - (line_bracket_count / num_bracket_count)

    self.confidences['line format'] = line_format_confidence


  def convert_bars_to_groups(self):
    converting = False
    prev_token = Token('\n', 'newline')

    for token in self.tokens:
      if token.group == 'operator' and token.text == '|':
        if not converting:
          if prev_token.group == 'group' and prev_token.text == '(':
            converting = True
            token.group = 'group'
        else:
          token.group = 'group'
          converting = False

      if token.group not in ['whitespace']:
        prev_token = token
