import itertools

from codestat_token import Token
from token_builders import TokenBuilder

# token reader for variable
class VisualBasicVariableTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, suffixes):
    self.text = None
    self.suffixes = suffixes


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier')]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isalpha()

    if len(candidate) > 0:
      return c.isalpha() or c.isdigit() or c in self.suffixes

    return candidate[-1] not in self.suffixes


# token reader for REMARK comment
class RemarkTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text == None:
      return None

    token1 = Token('', 'comment')
    token2 = Token('', 'comment')

    if self.text.startswith('Remark'):
      token1 = Token('Remark', 'keyword')
      token2 = Token(self.text[6:], 'comment')

    return [token1, token2]


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if candidate == '':
      return c == 'R'

    if candidate == 'R':
      return c == 'e'

    if candidate == 'Re':
      return c == 'm'

    return candidate.startswith('Rem')
