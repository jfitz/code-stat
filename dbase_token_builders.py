from codestat_token import Token
from token_builders import TokenBuilder


# token reader for identifier
class DbaseFilenameTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'filename')]


  def accept(self, candidate, c):
    result = c.isalpha() or c.isdigit() or c == '.'

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    num_dots = self.text.count('.')

    if num_dots > 1:
      return 0

    return len(self.text)
