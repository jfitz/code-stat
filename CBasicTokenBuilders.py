from Token import Token
from TokenBuilders import TokenBuilder

# token reader for variable
class CBasicVariableTokenBuilder(TokenBuilder):
  def __init__(self, suffixes):
    self.token = None
    self.suffixes = suffixes


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'identifier')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isalpha()

    if len(candidate) > 0:
      result = c.isalpha() or c.isdigit() or c == '.' or c in self.suffixes

    if len(candidate) > 1 and candidate[-1] in self.suffixes:
      result = False

    return result
