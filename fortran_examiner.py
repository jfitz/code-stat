from codestat_token import Token
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder
)
from jcl_token_builders import (
  JCLTokenBuilder
)
from examiner import Examiner


class FortranExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    super().__init__()
    self.newlines_important = 'always'

    self.whitespace_tb = WhitespaceTokenBuilder()
    self.newline_tb = NewlineTokenBuilder()

    self.integer_tb = IntegerTokenBuilder(None)
    self.integer_exponent_tb = IntegerExponentTokenBuilder(None)
    self.real_tb = RealTokenBuilder(False, False, None)
    self.real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    self.double_exponent_tb = RealExponentTokenBuilder(False, False, 'D', None)

    self.jcl_tb = JCLTokenBuilder()

    self.invalid_token_builder = InvalidTokenBuilder()


  def convert_numbers_to_lineNumbers(self):
    prev_token = Token('\n', 'newline')

    for token in self.tokens:
      if token.group == 'number' and\
        prev_token.group == 'newline':
        token.group = 'line number'

      if token.group not in ['whitespace', 'comment']:
        prev_token = token


  def convert_stars_to_io_channels(self):
    prev_tokens = [
      Token('\n', 'newline'),
      Token('\n', 'newline'),
      Token('\n', 'newline'),
      Token('\n', 'newline')
    ]

    for token in self.tokens:
      if token.group == 'operator' and token.text == '*' and\
        prev_tokens[-1].group == 'keyword' and\
        prev_tokens[-1].text.lower() in ['read', 'write', 'print']:
        token.group = 'number'

      if token.group == 'operator' and token.text == '*' and\
        prev_tokens[-1].group == 'group' and\
        prev_tokens[-1].text == '(' and\
        prev_tokens[-2].group == 'keyword' and\
        prev_tokens[-2].text.lower() in ['read', 'write', 'print']:
        token.group = 'number'

      if token.group == 'operator' and token.text == '*' and\
        prev_tokens[-1].group == 'group' and\
        prev_tokens[-1].text == ',' and\
        prev_tokens[-2].group == 'number' and\
        prev_tokens[-3].group == 'group' and\
        prev_tokens[-3].text == '(' and\
        prev_tokens[-4].group == 'keyword' and\
        prev_tokens[-4].text.lower() in ['read', 'write', 'print']:
        token.group = 'number'

      if token.group not in ['whitespace', 'comment']:
        prev_tokens.append(token)
        prev_tokens.pop(0)
