from codestat_token import Token
from token_builders import (
  TokenBuilder,
  EscapedStringTokenBuilder
)

# token reader for single-character text literal (string)
class FsharpCharTokenBuilder(EscapedStringTokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, quotes):
    super().__init__(quotes, False)


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[-1] != self.text[0]:
      return 0

    # length limited to 3 (or more with backslash)
    if '\\' in self.text:
      # at most four chars (two quotes, backslash, and one other)
      if len(self.text) > 4:
        return 0
      # backslash must be first char (and may repeat for second)
      if self.text[1] != '\\':
        return 0
    else:
      # at most three chars (two quotes, one character)
      if len(self.text) > 3:
        return 0

    # cannot follow an identifier
    if len(line_printable_tokens) > 0 and \
      line_printable_tokens[-1].group == 'identifier':
      return 0

    return len(self.text)
