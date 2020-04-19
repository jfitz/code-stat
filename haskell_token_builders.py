from codestat_token import Token
from token_builders import TokenBuilder

# token reader for identifier
class HaskellIdentifierTokenBuilder(TokenBuilder):
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
      return c.islower()

    return c.isalpha() or c.isdigit()


# token reader for class name
class HaskellClassTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'class', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isupper()

    return c.isalpha() or c.isdigit()


# token reader for user-defined operator
class HaskellOperatorTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, legals):
    self.legals = legals
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'operator', True)]


  def accept(self, _, c):
    return c in self.legals
