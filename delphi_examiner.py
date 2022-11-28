import string
import math

from codestat_tokenizer import Tokenizer
from codestat_token import Token
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  EscapedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  PrefixedIntegerTokenBuilder,
  BlockTokenBuilder
)
from pascal_token_builders import (
  BraceCommentTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  ClassTypeTokenBuilder
)
from examiner import Examiner

class DelphiExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    EscapedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    BlockTokenBuilder.__escape_z__()
    BraceCommentTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    ClassTypeTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator', False)

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(True, True, None)
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E', None)
    hex_constant_tb = PrefixedIntegerTokenBuilder('$', True, '0123456789ABCDEFabcdef')
    octal_constant_tb = PrefixedIntegerTokenBuilder('&', True, '01234567')
    binary_constant_tb = PrefixedIntegerTokenBuilder('%', True, '01')
    char_constant_tb = PrefixedIntegerTokenBuilder('#', True, '0123456789')
    operand_types.append('number')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    class_tb = ClassTypeTokenBuilder()
    operand_types.append('class')

    string_tb = EscapedStringTokenBuilder(["'"], 0)
    operand_types.append('string')

    brace_comment_tb = BraceCommentTokenBuilder()
    paren_star_comment_tb = BlockTokenBuilder('(*', '*)', 'comment')
    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()

    known_operators = [
      '+', '-', '*', '/',
      '=', '<>', '>', '>=', '<', '<=',
      'and', 'or', 'not', 'xor',
      '&', '|', '~', '<<', '>>',
      ':=', '^', '~', '@',
      '.',
      '..',
      'div', 'mod', 'shl', 'shr', 'is', 'in'
    ]

    known_operator_tb = CaseInsensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-',
      'not', '@', '^', '.'
    ]

    self.postfix_operators = ['^']

    groupers = ['(', ')', ',', '[', ']', ':']
    group_starts = ['(', '[', ',']
    group_mids = [',', ':']
    group_ends = [')', ']']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'as',
      'begin', 'break',
      'case', 'class', 'const', 'constructor',
      'default', 'destructor', 'do', 'downto',
      'else', 'end', 'except',
      'finally', 'for', 'forward', 'function',
      'goto',
      'if', 'implementation', 'inherited', 'interface',
      'label',
      'object', 'of', 'on', 'otherwise', 'out',
      'packed', 'private', 'procedure', 'program', 'property', 'protected', 'public',
      'raise', 'read', 'repeat', 'resourcestring',
      'strict',
      'then', 'threadvar', 'to', 'try', 'type',
      'unit', 'until', 'uses',
      'var',
      'while', 'with', 'write'
    ]

    keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    types = [
      'array', 'boolean', 'char', 'file', 'integer', 'real', 'record', 'set', 'string'
    ]

    types_tb = CaseInsensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    values = [
      'false', 'nil', 'true'
    ]

    values_tb = CaseInsensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      hex_constant_tb,
      octal_constant_tb,
      binary_constant_tb,
      char_constant_tb,
      keyword_tb,
      types_tb,
      values_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      class_tb,
      string_tb,
      brace_comment_tb,
      paren_star_comment_tb,
      slash_slash_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = self.combine_identifier_colon(tokens, ['statement separator'], ['begin'], ['whitespace', 'comment', 'newline'])
    self.tokens = tokens
    self.convert_identifiers_to_labels()
    self.convert_identifiers_to_labels_2()

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    pair_starters = ['unit', 'class', 'begin', 'record', 'case', 'try']
    pair_enders = ['end']
    self.calc_paired_blockers_confidence(pair_starters, pair_enders)
    self.calc_line_length_confidence(code, self.max_expected_line)


  # convert identifiers after 'label' and before ';' to labels
  def convert_identifiers_to_labels_2(self):
    seen_label_keyword = False

    for token in self.tokens:
      if token.group == 'identifier' and seen_label_keyword:
        token.group = 'label'

      if token.text.lower() == 'label':
        seen_label_keyword = True

      if token.text == ';':
        seen_label_keyword = False


  # convert identifiers followed by colons to labels
  # but only if they are the first printable tokens on the line
  @staticmethod
  def combine_identifier_colon(tokens, separator_groups, separator_texts, ignore_groups):
    new_list = []

    new_token = None
    first_printable_token = True
    in_declaration = True

    for token in tokens:
      if token.text == ':' and \
        new_token is not None and new_token.group == 'identifier' and \
        first_printable_token and \
        not in_declaration:
        new_token = Token(new_token.text + token.text, 'label', False)
      else:
        if new_token is not None:
            new_list.append(new_token)
            if new_token.group in separator_groups or \
              new_token.text in separator_texts:
              first_printable_token = True
            else:
              if new_token.group not in ignore_groups:
                first_printable_token = False
        new_token = token

      if token.text.lower() in ['procedure', 'function', 'constructor', 'destructor']:
        in_declaration = True

      if token.text.lower() == 'begin':
        in_declaration = False

    if new_token is not None:
      new_list.append(new_token)

    return new_list
