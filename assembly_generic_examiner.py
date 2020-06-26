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
  PrefixedIntegerTokenBuilder,
  SuffixedIntegerTokenBuilder,
  RealTokenBuilder,
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from assembly_token_builders import (
  AssemblyCommentTokenBuilder
)
from examiner import Examiner

class AssemblyGenericExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StringTokenBuilder.__escape_z__()
    PrefixedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    SuffixedIntegerTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    AssemblyCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, tab_size):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    real_tb = RealTokenBuilder(True, True, None)
    hex_integer_1_tb = PrefixedIntegerTokenBuilder('$', False, '0123456789abcdefABCDEF')
    hex_integer_2_tb = PrefixedIntegerTokenBuilder('#$', False, '0123456789abcdefABCDEF')
    hex_integer_3_tb = PrefixedIntegerTokenBuilder('&', False, '0123456789abcdefABCDEF')
    hex_integer_h_tb = SuffixedIntegerTokenBuilder(['h'], False, 'abcdefABCDEF')
    binary_integer_tb = PrefixedIntegerTokenBuilder('0b', False, '01')
    suffixed_integer_tb = SuffixedIntegerTokenBuilder(['Q', 'A', 'O', 'D', 'B'], False, None)
    operand_types.append('number')

    leads = '_$#.'
    extras = '_$#.'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, 0)
    hex_string_tb = PrefixedStringTokenBuilder('X', False, quotes)
    char_string_tb = PrefixedStringTokenBuilder('C', False, quotes)
    operand_types.append('string')

    known_operators = [
      '+', '-', '*', '/', '=', '&', '#', '?'
    ]

    self.unary_operators = [
      '+', '-', '=', '&', '#', '?'
    ]

    self.postfix_operators = []

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':', '<', '>']
    group_starts = ['(', '[', ',', '{', '<']
    group_ends = [')', ']', '}', '>']
    group_mids = [',']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    # keywords = []

    # keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    # types = []

    # types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)

    values = ['*']

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    comment_tb = AssemblyCommentTokenBuilder(';*')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      integer_tb,
      integer_exponent_tb,
      hex_integer_1_tb,
      hex_integer_2_tb,
      hex_integer_3_tb,
      hex_integer_h_tb,
      binary_integer_tb,
      suffixed_integer_tb,
      real_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      string_tb,
      hex_string_tb,
      char_string_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    # get tokens and indents
    opcode_extras = '.&=,()+-*/'
    label_leads = '.&'
    label_mids = '.&'
    label_ends = ':,'
    comment_leads = '*;!'
    tokens, indents = self.tokenize_asm_code(code, tab_size, tokenizer, opcode_extras, label_leads, label_mids, label_ends, comment_leads)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['newline'], [], [])
    tokens = Tokenizer.combine_number_and_adjacent_identifier(tokens)
    self.tokens = tokens
    self.convert_identifiers_to_labels()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence([])
    self.calc_operator_confidence()

    allow_pairs = []

    self.calc_operator_2_confidence(tokens, allow_pairs)
    self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
    self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)
    self.calc_group_confidence(tokens, group_mids)
    operand_types_2 = ['number']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)
    # self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
    self.calc_indent_confidence(indents)


  def calc_indent_confidence(self, indents):
    opcode_indents = {}
    for indent_dict in indents:
      opcode = indent_dict['opcode']
      if opcode is not None:
        if opcode in opcode_indents:
          opcode_indents[opcode] += 1
        else:
          opcode_indents[opcode] = 1

    total = 0
    highest = 0

    for key in opcode_indents:
      indent = opcode_indents[key]
      total += indent
      if indent > highest:
        highest = indent

    confidence = highest / total
    
    self.confidences['opcode_indent'] = confidence
