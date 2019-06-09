from Token import Token
from TokenBuilders import (
  TokenBuilder,
  CharTokenBuilder
)

# token reader for -- comment
class DashDashCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment')]


  def accept(self, candidate, c):
    result = False

    if candidate.startswith('--'):
      result = True

    if c == '-' and candidate == '-':
      result = True

    if c == '-' and candidate == '':
      result = True

    if c in ['\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text.startswith('--'):
      return len(self.text)
    
    return 0


# token reader for single-character text literal (string)
class AdaCharTokenBuilder(CharTokenBuilder):
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
