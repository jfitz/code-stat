from codestat_tokenizer import Tokenizer
from token_builders import InvalidTokenBuilder
from pl1_examiner import PL1Examiner
from examiner import Examiner

class PL1FreeFormatExaminer(PL1Examiner):
  @staticmethod
  def __escape_z__():
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      self.newline_tb,
      self.whitespace_tb,
      self.line_continuation_tb,
      self.terminators_tb,
      self.integer_tb,
      self.integer_exponent_tb,
      self.binary_integer_tb,
      self.real_tb,
      self.real_exponent_tb,
      self.binary_real_tb,
      self.keyword_tb,
      self.function_tb,
      self.attributes_tb,
      self.options_tb,
      self.conditions_tb,
      self.subroutines_tb,
      self.types_tb,
      self.values_tb,
      self.groupers_tb,
      self.known_operator_tb,
      self.identifier_tb,
      self.string_tb,
      self.label_tb,
      self.slash_star_comment_tb,
      self.preprocessor_tb,
      self.title_tb,
      self.subtitle_tb,
      self.error_tb,
      self.warn_tb,
      self.inform_tb,
      self.jcl_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_operator_confidence()

    allow_pairs = []

    self.calc_operator_2_confidence(tokens, allow_pairs)
    self.calc_operator_3_confidence(tokens, self.group_ends, allow_pairs)
    self.calc_operator_4_confidence(tokens, self.group_starts, allow_pairs)
    self.calc_group_confidence(tokens, self.group_mids)
    operand_types_2 = ['number', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, self.operand_types, 4)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
