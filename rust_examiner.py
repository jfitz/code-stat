import string
import math

from codestat_token import Token
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
  PrefixedIntegerTokenBuilder,
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  NestedCommentTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  ClassTypeTokenBuilder
)
from rust_token_builders import (
  RustRawStringTokenBuilder,
  RustAttributeTokenBuilder
)
from examiner import Examiner

class RustExaminer(Examiner):
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
    PrefixedIntegerTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    NestedCommentTokenBuilder.__escape_z__()
    ClassTypeTokenBuilder.__escape_z__()
    RustRawStringTokenBuilder.__escape_z__()
    RustAttributeTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, block_comment_limit):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation', False)

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    real_tb = RealTokenBuilder(False, True, '_')
    real_exponent_tb = RealExponentTokenBuilder(False, True, 'E', '_')
    octal_integer_tb = PrefixedIntegerTokenBuilder('0o', True, '01234567_')
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', True, '0123456789ABCDEFabcdef_')
    binary_integer_tb = PrefixedIntegerTokenBuilder('0b', True, '01_')
    operand_types.append('number')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    attribute_tb = RustAttributeTokenBuilder()

    quotes = ['"', "'"]
    string_tb = StringTokenBuilder(quotes, 10)
    bstring_tb = PrefixedStringTokenBuilder('b', True, quotes)
    rstring_tb = RustRawStringTokenBuilder()
    operand_types.append('string')

    class_type_tb = ClassTypeTokenBuilder()
    operand_types.append('class')

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = NestedCommentTokenBuilder('/*', '*/', block_comment_limit)

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '/', '%', '^', '!', '&', '|',
      '&&', '||', '<<', '>>',
      '+=', '-=', '*=', '/=', '%=', '^=', '&=', '|-', '<<=', '>>=',
      '=', '==', '!=', '>', '<', '>=', '<=',
      '@', '.', '..', '...', '->', '#', '$', '?',
      'in', '&mut'
    ]

    self.unary_operators = [
      '+', '-', '*',
      '!', '&', '&mut'
    ]

    self.postfix_operators = [
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':', '::', '=>']
    group_starts = ['(', '[', ',', '{']
    group_mids = [',', ':', '::', '=>']
    group_ends = [')', ']', '}', ')|']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'as',
      'break',
      'const', 'continue', 'crate'
      'else', 'enum', 'extern',
      'fn', 'for',
      'if', 'impl',
      'let', 'loop',
      'match', 'mod', 'move', 'mut',
      'pub',
      'ref', 'return',
      'static', 'struct',
      'trait', 'type',
      'unsafe', 'use',
      'where', 'while'
    ]

    keywords_2018 = [
      'dyn',
      'union',
      'static'
    ]

    keywords_future = [
      'abstract',
      'become', 'box',
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

    keywords += keywords_2018
    keywords += keywords_future

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    types = [
      'Self',
      'u8', 'i8',
      'u16', 'i16',
      'u32', 'i32',
      'u64', 'i64',
      'u128', 'i128',
      'usize', 'isize',
      'f32',
      'f64'
    ]

    types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    values = [
      'self', 'true', 'false', 'super', '_'
    ]

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

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
      binary_integer_tb,
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
      slash_slash_comment_tb,
      slash_star_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    self.tokens = self.combine_numbers_and_adjacent_types(tokens)
    self.convert_operators_to_identifiers()
    self.convert_bars_to_groups()

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator'])
    if num_operators > 0:
      self.calc_operator_confidence()
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, allow_pairs)
      self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_format_confidence()


  def combine_numbers_and_adjacent_types(self, tokens):
    new_list = []

    new_token = None
    for token in tokens:
      if token.group == 'type' and new_token is not None and new_token.group == 'number':
        new_token = Token(new_token.text + token.text, 'number', True)
      else:
        if new_token is not None:
            new_list.append(new_token)
        new_token = token
    if new_token is not None:
      new_list.append(new_token)

    return new_list


  def calc_line_format_confidence(self):
    drop_types = ['whitespace', 'comment', 'line continuation']
    tokens = Examiner.drop_tokens(self.tokens, drop_types)

    line_bracket_count = 0
    num_bracket_count = 0
    prev2_token = Token('\n', 'newline', False)
    prev_token = Token('\n', 'newline', False)
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


  def convert_operators_to_identifiers(self):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.text == '*' and prev_token.text == '::':
        token.group = 'identifier'

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token


  def convert_bars_to_groups(self):
    converting = False
    prev_token = Token('\n', 'newline', False)

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
