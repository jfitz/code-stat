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

    return [Token(self.text, 'identifier')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0 and c == '[':
      result = True

    if len(candidate) == 1:
      result = True

    if len(candidate) > 1 and candidate[-1] != ']':
        result = True

    if c in ['\t', '\n', '\r']:
      result = False

    return result


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
