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

    title_directive_tb = LeadToEndOfLineTokenBuilder('TITLE', False, 'directive')
    subtitle_directive_tb = LeadToEndOfLineTokenBuilder('SUBTTL', False, 'directive')
    include_directive_tb = LeadToEndOfLineTokenBuilder('INCLUDE', False, 'directive')

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
      title_directive_tb,
      subtitle_directive_tb,
      include_directive_tb,
      identifier_tb,
      string_tb,
      hex_string_tb,
      char_string_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    opcode_tokenbuilders = [
      identifier_tb,
      invalid_token_builder
    ]

    args_tokenbuilders = [
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
    opcode_tokenizer = Tokenizer(opcode_tokenbuilders)
    args_tokenizer = Tokenizer(args_tokenbuilders)

    # tokenize as free-format
    tokens1 = tokenizer.tokenize(code)
    tokens1 = Examiner.combine_adjacent_identical_tokens(tokens1, 'invalid operator')
    tokens1 = Examiner.combine_adjacent_identical_tokens(tokens1, 'invalid')
    self.tokens = tokens1
    self.convert_asm_identifiers_to_labels()

    self.calc_statistics()
    statistics1 = self.statistics
    self.statistics = {}

    self.calc_confidences(operand_types, group_starts, group_mids, group_ends, None)

    confidences1 = self.confidences
    self.confidences = {}
    errors1 = self.errors
    self.errors = []

    # tokenize as space-format
    opcode_extras = '.&=,()+-*/'
    label_leads = '.&$@'
    label_mids = '.&$#@'
    label_ends = ':,'
    comment_leads = '*;!'
    line_comment_leads = ''
    use_line_id = False
    tokens2, indents = Tokenizer.tokenize_asm_code(code, tab_size, opcode_tokenizer, opcode_extras, args_tokenizer, label_leads, label_mids, label_ends, comment_leads, line_comment_leads, use_line_id)
    tokens2 = Examiner.combine_adjacent_identical_tokens(tokens2, 'invalid operator')
    tokens2 = Examiner.combine_adjacent_identical_tokens(tokens2, 'invalid')
    tokens2 = Examiner.combine_identifier_colon(tokens2, ['newline'], [], [])
    tokens2 = Tokenizer.combine_number_and_adjacent_identifier(tokens2)
    tokens2 = Examiner.convert_asterisks_to_operators(tokens2)
    self.tokens = tokens2
    self.convert_asm_identifiers_to_labels()

    self.calc_statistics()
    statistics2 = self.statistics
    self.statistics = {}

    self.calc_confidences(operand_types, group_starts, group_mids, group_ends, indents)
    confidences2 = self.confidences
    self.confidences = {}
    errors2 = self.errors
    self.errors = []

    # select the better of free-format and spaced-format
    confidence1 = 1.0
    for key in confidences1:
      factor = confidences1[key]
      confidence1 *= factor

    confidence2 = 1.0
    for key in confidences2:
      factor = confidences2[key]
      confidence2 *= factor

    if confidence2 > confidence1:
      self.tokens = tokens2
      self.statistics = statistics2
      self.confidences = confidences2
      self.errors = errors2
    else:
      self.tokens = tokens1
      self.statistics = statistics1
      self.confidences = confidences1
      self.errors = errors1


  def calc_confidences(self, operand_types, group_starts, group_mids, group_ends, indents):
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

    operand_types_2 = ['number']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    # self.calc_keyword_confidence()

    # self.calc_paired_blockers_confidence(['{'], ['}'])

    if indents is not None:
      self.calc_indent_confidence(indents)
