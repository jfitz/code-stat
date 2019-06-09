from Token import Token
from TokenBuilders import (
  TokenBuilder,
  CharTokenBuilder
)

# token reader for single-character text literal (string)
class FsharpCharTokenBuilder(CharTokenBuilder):
  def __init__(self, quotes):
    super().__init__(quotes)

  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[-1] != self.text[0]:
      return 0

    if len(line_printable_tokens) > 0 and line_printable_tokens[-1].group == 'identifier':
      return 0

    return len(self.text)
