from codestat_token import Token
from token_builders import TokenBuilder

# token reader for comment
class AssemblyCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, legals):
    self.legals = legals
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment', False)]


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if candidate == '':
      return c in self.legals

    if candidate[0] in self.legals:
      return True

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(line_printable_tokens) > 0:
      return 0

    if self.text[0] in self.legals:
      return len(self.text)
    
    return 0
