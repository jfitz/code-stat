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

    return [Token(self.text, 'number', True)]


  def accept(self, candidate, c):

    if len(candidate) == 0:
      return c.isdigit() or c in ['+', '-']

    if candidate[-1].isdigit():
      return c.isdigit() or c.isalpha() or c == '.'

    if candidate[-1] == '.':
      return c.isdigit() or c in ['E', 'e']

    if candidate[-1] in ['E', 'e']:
      return c.isdigit() or c in ['+', '-']

    if candidate[-1] in ['+', '-']:
      return c.isdigit()
    
    if candidate[-1].isalpha():
      return c.isalpha()

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) == 0:
      return 0

    if self.text[-1].isalnum() or \
       self.text[-1] == '.':
      return len(self.text)

    return 0
