import string
import math

from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  PrefixedIntegerTokenBuilder,
  ParenStarCommentTokenBuilder
)
from pascal_token_builders import (
  BraceCommentTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder
)
from examiner import Examiner

class DelphiExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    ParenStarCommentTokenBuilder.__escape_z__()
    BraceCommentTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator')

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(True, True, None)
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E', None)
    hex_constant_tb = PrefixedIntegerTokenBuilder('$', True, '0123456789ABCDEFabcdef')
    octal_constant_tb = PrefixedIntegerTokenBuilder('&', True, '01234567')
    binary_constant_tb = PrefixedIntegerTokenBuilder('%', True, '01')
    char_constant_tb = PrefixedIntegerTokenBuilder('#', True, '0123456789')
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(["'"], False)

    brace_comment_tb = BraceCommentTokenBuilder()
    paren_star_comment_tb = ParenStarCommentTokenBuilder('(*', '*)')
    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()

    known_operators = [
      '+', '-', '*', '/',
      '=', '<>', '>', '>=', '<', '<=',
      'and', 'or', 'not', 'xor',
      '&', '|', '~', '<<', '>>',
      ':=', '^', '~', '@',
      '.', ':',
      '..',
      'div', 'mod', 'shl', 'shr', 'is', 'in'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-',
      'not', '@', '^', '.'
    ]

    groupers = ['(', ')', ',', '[', ']']
    group_ends = [')', ']']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'as',
      'begin', 'break', 'case', 'class', 'const', 'constructor',
      'default', 'destructor', 'do', 'downto',
      'else', 'end', 'except',
      'file', 'finally', 'for', 'forward', 'function',
      'goto',
      'if', 'implementation', 'inherited', 'interface',
      'label',
      'object', 'of', 'on', 'out',
      'packed', 'private', 'procedure', 'program', 'property', 'protected', 'public',
      'raise', 'read', 'record', 'repeat', 'resourcestring',
      'set', 'strict',
      'then', 'threadvar', 'to', 'try', 'type',
      'unit', 'until', 'uses',
      'var', 'while', 'with', 'write'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    types = [
      'array', 'boolean', 'char', 'integer', 'real', 'string'
    ]

    types_tb = ListTokenBuilder(types, 'type', False)

    values = [
      'false', 'nil', 'true'
    ]

    values_tb = ListTokenBuilder(values, 'value', False)

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
      string_tb,
      brace_comment_tb,
      paren_star_comment_tb,
      slash_slash_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    pair_starters = ['unit', 'class', 'begin', 'record', 'case', 'try']
    pair_enders = ['end']
    self.calc_paired_blockers_confidence(pair_starters, pair_enders)

    # get the first and last meaningful tokens
    first_token = None
    last_token = None

    for token in self.tokens:
      if first_token is None and token.group not in ['whitespace', 'comment', 'newline']:
        first_token = token

      if token.group not in ['whitespace', 'comment', 'newline']:
        last_token = token

    # program should begin with 'program' or 'unit'
    program_end_confidence = 0.0
    if first_token is not None:
      if first_token.text.lower() in ['program', 'unit']:
        program_end_confidence += 0.75

    # program should end with '.'
    if last_token is not None:
      if last_token.text == '.':
        program_end_confidence += 0.25

    self.confidences['program_end'] = program_end_confidence
    self.calc_statistics()
