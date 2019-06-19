from PL1Examiner import PL1Examiner
from TokenBuilders import InvalidTokenBuilder
from Token import Token
from Tokenizer import Tokenizer

class PL1FixedFormatExaminer(PL1Examiner):
  def __init__(self, code, tab_size, wide):
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
      self.format_item_tb,
      self.types_tb,
      self.values_tb,
      self.groupers_tb,
      self.known_operator_tb,
      self.identifier_tb,
      self.string_tb,
      self.slash_star_comment_tb,
      self.preprocessor_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    self.tokens = self.tokenize_code(code, tab_size, tokenizer, wide)
    self.tokens = self.combine_adjacent_whitespace(self.tokens)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(self.group_ends)
    operand_types = ['number', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()


  def tokenize_line(self, line, tokenizer, wide):
    # break apart the line based on fixed format
    tokens = []

    # The fixed-format FORTRAN line format is:
    # 1: space or C or *
    # 2-6: line number or blank
    # 7: continuation character
    # 8-72: program text
    # 73-: identification, traditionally sequence number (ignored)

    line_indicator = line[0:1]
    if wide:
      line_text = line[1:]
      line_identification = ''
    else:
      line_text = line[1:72]
      line_identification = line[72:]

    # tokenize the line indicator
    if line_indicator in ['C', '*']:
      tokens.append(Token(line, 'comment'))
    else:
      if len(line_indicator) > 0 and line_indicator != ' ':
        tokens.append(Token(line, 'invalid'))
      else:
        # tokenize the code
        tokens += tokenizer.tokenize(line_text)

        # tokenize the line identification
        if len(line_identification) > 0:
          tokens.append(Token(line_identification, 'line identification'))

    tokens.append(Token('\n', 'newline'))

    return tokens


  def tokenize_code(self, code, tab_size, tokenizer, wide):
    lines = code.split('\n')

    tokens = []

    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = self.tabs_to_spaces(line, tab_size)

      line_tokens = self.tokenize_line(line, tokenizer, wide)
      tokens += line_tokens

    return tokens
