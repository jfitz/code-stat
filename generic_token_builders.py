from codestat_token import Token
from token_builders import TokenBuilder

class GenericNumberTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number')]


  def accept(self, candidate, c):

    if len(candidate) == 0:
      return c.isdigit() or c in ['+', '-']

    if candidate[-1] in ['E', 'e']:
      return c.isdigit()
    elif candidate[-1].isdigit():
      return c.isdigit() or c.isalpha() or c == '.'
    elif candidate[-1] in ['+', '-']:
      return c.isdigit()
    elif candidate[-1].isalpha():
      return c.isalpha()
    elif candidate[-1] == '.':
      return c.isdigit()

    return False
