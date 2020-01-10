from codestat_token import Token
from token_builders import TokenBuilder

# token reader for identifier
class DirectiveTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'directive')]

  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c == '@'

    if len(candidate) > 0:
      result = c.isalpha() or c.isdigit() or c == '_'

    return result
