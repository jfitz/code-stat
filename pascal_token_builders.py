from codestat_token import Token
from token_builders import TokenBuilder

# token reader for brace comment
class BraceCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment', False)]

  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == '{'

    if len(candidate) > 0:
      return candidate[-1] != '}'

    return True


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[0] != '{':
      return 0
      
    if self.text[-1] != '}':
      return 0

    return len(self.text)
