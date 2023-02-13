import string
import math

from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  EscapedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  IdentifierTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from prolog_token_builders import (
  PrologVariableTokenBuilder
)
from examiner import Examiner

class PrologExaminer(Examiner):
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
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    PrologVariableTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()
    self.newlines_important = 'parens'

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator', False)
    stmt_terminator_tb = SingleCharacterTokenBuilder('.', 'statement terminator', False)

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    real_tb = RealTokenBuilder(False, False, '_')
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', '_')
    operand_types.append('number')

    variable_tb = PrologVariableTokenBuilder()
    operand_types.append('variable')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)

    quotes = ['"', "'"]
    string_tb = EscapedStringTokenBuilder(quotes, 0)
    operand_types.append('string')

    comment_tb = LeadToEndOfLineTokenBuilder('%', True, 'comment')

    special_symbols = ['!']
    special_symbol_tb = CaseSensitiveListTokenBuilder(special_symbols, 'identifier', True)

    known_operators = [
        '-->', ':-',
        '?-', '|',
        '->', '*->',
        ':=', '\\+',
        '<', '=', '=..', '=@=', '\\=@=', '=:=', '=<', '==', '=\\=',
        '>', '>=', '@<', '@=<', '@>', '@>=', '\\=', '\\==', 'as', 'is', '>:<',
        ':<',
        ':', '+', '-', '/\\', '\\/', 'xor',
        '?', '*', '/', '//', 'div', 'rdiv', '<<', '>>', 'mod', 'rem',
        '**', '^', '+', '-', '\\', '$'
      ]

    self.unary_operators = [
      '+', '-', ':-', '\\', '\\+'
    ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    groupers = ['(', ')', ',', '[', ']', '{', '}', '|']
    group_starts = ['(', '[', ',', '{']
    group_mids = [',', '|']
    group_ends = [')', ']', '}']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'dynamic', 'discontiguous', 'initialization', 'meta_predicate',
      'module_transparent', 'multifile', 'public', 'thread_local',
      'thread_initialization', 'volatile'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    values = ['(-)']

    value_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      stmt_separator_tb,
      stmt_terminator_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      known_operator_tb,
      special_symbol_tb,
      variable_tb,
      groupers_tb,
      identifier_tb,
      string_tb,
      value_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_parens_continued_lines(tokens)
    tokens = Examiner.join_operator_continued_lines(tokens, self.postfix_operators)

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

    # self.calc_keyword_confidence(0.7)
    self.calc_line_length_confidence(code, self.max_expected_line)
