from token_builders import TokenBuilder

from codestat_token import Token

# token reader for line label (string)
class FlowmaticLabelTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'label', True)]


  def accept(self, candidate, c):
    # newline breaks a string
    if c in ['\n', '\r']:
      return False

    if len(candidate) == 0:
      return c == '('

    if len(candidate) == 1:
      return c.isdigit()

    if candidate[-1] == ')':
      return False

    # no quote stuffing, stop on second quote
    # assume no escaped quotes are allowed
    return c.isdigit or c == ')'


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text[-1] != ')':
      return 0

    if len(line_printable_tokens) > 0:
      return 0

    return len(self.text)
