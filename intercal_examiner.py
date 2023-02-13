from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  IntegerExponentTokenBuilder,
  PrefixedIntegerTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from intercal_token_builders import ParensLabelTokenBuilder
from examiner import Examiner

class IntercalExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    ParensLabelTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = PrefixedIntegerTokenBuilder('#', False, '0123456789')
    variable16_tb = PrefixedIntegerTokenBuilder('.', False, '0123456789')
    variable32_tb = PrefixedIntegerTokenBuilder(':', False, '0123456789')
    array16_tb = PrefixedIntegerTokenBuilder(',', False, '0123456789')
    array32_tb = PrefixedIntegerTokenBuilder(';', False, '0123456789')
    operand_types.append('number')

    comment_tb = LeadToEndOfLineTokenBuilder('NOTE', True, 'comment')

    label_tb = ParensLabelTokenBuilder()

    known_operators = [
      '~', '$', 'V', '?', '&',
      'SUB', '<-'
    ]

    self.unary_operators = [
      'V', '?', '&'
    ]

    self.postfix_operators = []

    groupers = ['"', "'"]
    group_starts = ['"', "'"]
    group_ends = ['"', "'"]
    group_mids = []

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'DO',
      'STASH',
      'RETRIEVE', 'RESUME', 'FORGET',
      'NEXT', 'ABSTAIN',
      'FROM', 'REINSTATE', 'IGNORE',
      'REMEMBER',
      'WRITE',
      'IN', 'READ',
      'OUT',
      'PLEASE',
      'COME', 'FROM'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      integer_tb,
      variable16_tb,
      variable32_tb,
      array16_tb,
      array32_tb,
      keyword_tb,
      groupers_tb,
      label_tb,
      known_operator_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['statement terminator', 'newline'], ['{'], ['whitespace', 'comment'])
    self.tokens = tokens
    self.convert_identifiers_after_goto_to_labels()

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence(['*', ';'])

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence(0.2)

    # self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_length_confidence(code, self.max_expected_line)
