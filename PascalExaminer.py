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
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  PrefixedIntegerTokenBuilder,
  ParenStarCommentTokenBuilder
)
from PascalTokenBuilders import (
  BraceCommentTokenBuilder
)
from Tokenizer import Tokenizer

class PascalExaminer(Examiner):
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
    string_tb = StringTokenBuilder(["'"], False, False)

    brace_comment_tb = BraceCommentTokenBuilder()
    paren_star_comment_tb = ParenStarCommentTokenBuilder()

    known_operators = [
      '+', '-', '*', '/',
      '=', '<>', '>', '>=', '<', '<=',
      'and', 'or', 'not',
      '&', '|', '~', '<<', '>>',
      ':=', '^', '~', '@',
      '.', ':',
      '..',
      'div', 'mod', 'shl', 'shr', 'in'
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
      'begin', 'break', 'case', 'const',
      'do', 'downto', 'else', 'end', 'file', 'for', 'forward',
      'function', 'goto', 'if', 'label', 'of', 'otherwise',
      'packed', 'procedure', 'program', 'record', 'repeat', 'reset',
      'set', 'string', 'then', 'to', 'type', 'until', 'uses',
      'value', 'var', 'while', 'with'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    types = [
      'array', 'boolean', 'char', 'integer', 'real'
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
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['begin', 'record', 'case'], ['end'])

    # get the first and last meaningful tokens
    first_token = None
    last_token = None

    for token in self.tokens:
      if first_token is None and token.group not in ['whitespace', 'comment', 'newline']:
        first_token = token

      if token.group not in ['whitespace', 'comment', 'newline']:
        last_token = token

    # program should begin with 'program'
    program_end_confidence = 0.0
    if first_token is not None:
      if first_token.text.lower() == 'program':
        program_end_confidence += 0.75

    # program should end with '.'
    if last_token is not None:
      if last_token.text == '.':
        program_end_confidence += 0.25

    self.confidences['program_end'] = program_end_confidence
    self.calc_statistics()
