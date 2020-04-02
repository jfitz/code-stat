from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import InvalidTokenBuilder
from pl1_token_builders import (
  PL1CommentStartTokenBuilder,
  PL1CommentMiddleTokenBuilder,
  PL1CommentEndTokenBuilder
)
from pl1_examiner import PL1Examiner
from examiner import Examiner

class PL1FixedFormatExaminer(PL1Examiner):
  @staticmethod
  def __escape_z__():
    PL1CommentStartTokenBuilder.__escape_z__()
    PL1CommentMiddleTokenBuilder.__escape_z__()
    PL1CommentEndTokenBuilder.__escape_z__()
    return 'Escape ?Z'


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

    comment_start_tb = PL1CommentStartTokenBuilder()
    comment_middle_tb = PL1CommentMiddleTokenBuilder()
    comment_end_tb = PL1CommentEndTokenBuilder()

    type1_tokenbuilders = [comment_start_tb]
    tokenbuilders1 = tokenbuilders + type1_tokenbuilders + [invalid_token_builder]
    tokenizer1 = Tokenizer(tokenbuilders1)

    type2_tokenbuilders = [comment_start_tb, comment_middle_tb, comment_end_tb]
    tokenbuilders2 = tokenbuilders + type2_tokenbuilders + [invalid_token_builder]
    tokenizer2 = Tokenizer(tokenbuilders2)

    tokens = self.tokenize_code(code, tab_size, tokenizer1, tokenizer2, wide)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'whitespace')
    self.tokens = self.convert_broken_comments_to_comments(tokens)

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence(tokens)
    self.calc_operator_3_confidence(tokens, self.group_ends)
    self.calc_operator_4_confidence(tokens, self.group_starts)
    operand_types = ['number', 'symbol']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()


  def tokenize_line(self, line, tokenizer, wide):
    # break apart the line based on fixed format
    tokens = []

    # The fixed-format PL/1 line format is:
    # 1: space or C or *
    # 2-72: program text
    # 73-: identification, traditionally sequence number (ignored)
    # but if columns 1 and 2 are '//', then the line is JCL

    if line.startswith(('//', '/*')):
      tokens.append(Token(line, 'jcl'))
    else:
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
          tokens.append(Token(' ', 'whitespace'))
          # tokenize the code
          tokens += tokenizer.tokenize(line_text)

          # tokenize the line identification
          if len(line_identification) > 0:
            tokens.append(Token(line_identification, 'line identification'))

    tokens.append(Token('\n', 'newline'))

    return tokens


  def tokenize_code(self, code, tab_size, tokenizer1, tokenizer2, wide):
    lines = code.split('\n')

    tokens = []

    mode = 1

    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = self.tabs_to_spaces(line, tab_size)

      if mode == 1:
        line_tokens = self.tokenize_line(line, tokenizer1, wide)
      else:
        line_tokens = self.tokenize_line(line, tokenizer2, wide)

      for token in line_tokens:
        if token.group == 'comment-end':
          mode = 1
        if token.group == 'comment-start':
          mode = 2

      tokens += line_tokens

    return tokens


  def convert_broken_comments_to_comments(self, tokens):
    for token in tokens:
      if token.group in ['comment-start', 'comment-middle', 'comment-end']:
        token.group = 'comment'

    return tokens


  def unwrapped_code(self, lines):
    unwrapped_lines = ''

    for line in lines:
      # remove line description (if any)
      line = line[:72]
      line = line.rstrip()

      # wrap column-1 comment in slash-star, star-slash
      if len(line) > 0 and line[0] != ' ':
        line = '/*' + line + '*/'

      unwrapped_lines += line
      unwrapped_lines += '\n'

    return unwrapped_lines
