from codestat_tokenizer import Tokenizer
from token_builders import InvalidTokenBuilder
from pl1_examiner import PL1Examiner

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
      self.real_tb,
      self.real_exponent_tb,
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
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(self.group_ends)
    operand_types = ['number', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
