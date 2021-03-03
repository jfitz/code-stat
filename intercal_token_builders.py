from codestat_token import Token
from token_builders import TokenBuilder

# token reader for identifier
class ParensLabelTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == '('

    if candidate[-1] == ')':
      return False

    return c.isdigit() or c == ')'


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have at least three chars
    if len(self.text) < 3:
      return 0

    if self.text[0] != '(':
      return 0

    if self.text[-1] != ')':
      return 0

    return len(self.text)
