from Token import Token
from TokenBuilders import TokenBuilder

# token reader for identifier
class IdentifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'identifier')]

  def accept(self, candidate, c):
    result = False

    if c.isalpha():
      result = True

    if len(candidate) > 0 and c.isdigit():
      result = True

    if c == '_':
      result = True

    return result


# token reader for variable
class PrologVariableTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'variable')]

  def accept(self, candidate, c):
    result = False

    if c.isalpha() and c.isupper():
      result = True

    if len(candidate) > 0 and c.isdigit():
      result = True

    if c == '_':
      result = True

    return result
