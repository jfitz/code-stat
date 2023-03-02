import string

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  IntegerTokenBuilder,
  RealTokenBuilder,
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder
)
from flowmatic_token_builders import FlowmaticLabelTokenBuilder
from examiner import Examiner

class FlowmaticExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    FlowmaticLabelTokenBuilder.__escape_z__()
    return 'Escape ?Z'

  def __init__(self, code):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder('')
    real_tb = RealTokenBuilder(False, False, '')
    operand_types.append('number')

    leads = ''
    extras = '-'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    terminator1_tb = SingleCharacterTokenBuilder('.', 'statement terminator', False)
    terminator2_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    known_operators = [
    ]

    self.unary_operators = [
    ]

    self.postfix_operators = [
    ]

    groupers = ['(', ')', ',']
    group_starts = ['(']
    group_ends = [')']
    group_mids = [',']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'AGAINST',
      'CLOSE-OUT', 'COMPARE',
      'DATA',
      'END', 'EQUAL',
      'FILES',
      'GO', 'GREATER',
      'IF', 'INPUT',
      'JUMP',
      'OF', 'OPERATION', 'OTHERWISE', 'OUTPUT',
      'READ-ITEM', 'REWIND',
      'STOP',
      'TO', 'TRANSFER',
      'WITH', 'WRITE-ITEM',
      '(END)'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    label_tb = FlowmaticLabelTokenBuilder()

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      terminator1_tb,
      terminator2_tb,
      integer_tb,
      real_tb,
      keyword_tb,
      label_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
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
    self.convert_numbers_to_labels()
    # self.convert_identifiers_to_functions()
    # self.convert_functions_to_common_functions(functions)

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

    self.no_keyword_confidence = 0.2
    self.calc_keyword_confidence()

    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_length_confidence(code, self.max_expected_line)


  # convert identifiers after 'goto' to labels
  def convert_numbers_to_labels(self):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'number' and \
        prev_token.group == 'keyword' and prev_token.text.lower() == 'operation':
        token.group = 'label'
        token.is_operand = False

      if token.group not in ['whitespace', 'comment', 'newline', 'line identification']:
        prev_token = token
