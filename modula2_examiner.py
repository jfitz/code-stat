import string
import math

from codestat_tokenizer import Tokenizer
from codestat_token import Token
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
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  SuffixedIntegerTokenBuilder,
  BlockTokenBuilder
)
from examiner import Examiner

class Modula2Examiner(Examiner):
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
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    SuffixedIntegerTokenBuilder.__escape_z__()
    BlockTokenBuilder.__escape_z__()
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
    hex_constant_tb = SuffixedIntegerTokenBuilder('H', True, '0123456789ABCDEFabcdef')
    octal_constant_tb = SuffixedIntegerTokenBuilder('C', True, '01234567')
    binary_constant_tb = SuffixedIntegerTokenBuilder('B', True, '01')
    operand_types.append('number')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    quotes =["'", '"']
    string_tb = StringTokenBuilder(quotes, 0)
    operand_types.append('string')

    paren_star_comment_tb = BlockTokenBuilder('(*', '*)', 'comment')

    known_operators = [
      ':=', '=', '>', '>=', '<', '<=', '#', '<>',
      '+', '-', '*', '/', 'DIV', 'MOD',
      'AND', 'OR', 'NOT', '^', '.', '..', 'IN', '&'
    ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-',
      'NOT', '@', '^', '.'
    ]

    self.postfix_operators = ['^']

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':', '|']
    group_starts = ['(', '[', ',','{']
    group_mids = [',', ':', '|']
    group_ends = [')', ']', '}']

    groupers_tb = CaseSensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'BEGIN', 'BY',
      'CASE', 'CONST',
      'DEFINITION', 'DO',
      'ELSE', 'ELSIF', 'END', 'EXCEPT', 'EXIT', 'EXPORT',
      'FINALLY', 'FOR', 'FROM',
      'IF', 'IMPLEMENTATION', 'IMPORT',
      'LOOP',
      'MODULE',
      'OF',
      'PROCEDURE',
      'QUALIFIED',
      'REPEAT',
      'THEN', 'TO', 'TYPE',
      'VAR',
      'WITH', 'WHILE'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    types = [
      'ARRAY',
      'BOOLEAN',
      'CARDINAL', 'CHAR',
      'INTEGER',
      'POINTER',
      'REAL', 'RECORD',
      'SET'
    ]

    types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    functions = [
      'ABS',
      'CHR', 'CMPLX',
      'DEC', 'DISPOSE',
      'EXCL',
      'FLOAT',
      'HALT', 'HIGH',
      'IN', 'INC', 'INCL', 'INT',
      'LENGTH', 'LFLOAT',
      'MAX', 'MIN',
      'NEW',
      'ODD', 'ORD',
      'RE',
      'SIZE',
      'TRUNC',
      'VAL',
      'WriteString', 'WriteCard', 'WriteLn', 'WriteReal'
    ]

    function_tb = CaseSensitiveListTokenBuilder(functions, 'function', True)

    values = [
      'FALSE', 'NIL', 'TRUE'
    ]

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
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
      keyword_tb,
      types_tb,
      values_tb,
      known_operator_tb,
      groupers_tb,
      function_tb,
      identifier_tb,
      string_tb,
      paren_star_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    self.tokens = tokens

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

    operand_types_2 = ['number', 'string', 'identifier', 'variable']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    self.calc_paired_blockers_confidence(['BEGIN', 'RECORD', 'CASE', 'DO', 'IF', 'WHILE'], ['END'])
    self.calc_paired_blockers_confidence(['REPEAT'], ['UNTIL'])
    self.calc_line_length_confidence(code, self.max_expected_line)
