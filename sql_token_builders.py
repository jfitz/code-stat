from codestat_token import Token
from token_builders import TokenBuilder

# token reader for user-defined operator
class SqlBracketedIdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier', True)]


  def accept(self, candidate, c):
    if c in ['\t', '\n', '\r']:
      return False

    if len(candidate) == 0:
      return c == '['

    if len(candidate) == 1:
      return True

    return candidate[-1] != ']'


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have at least two chars, start and end with brackets
    if len(self.text) < 2:
      return 0

    if self.text[0] != '[':
      return 0

    if self.text[-1] != ']':
      return 0

    return len(self.text)
