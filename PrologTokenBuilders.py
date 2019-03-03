from Token import Token
from TokenBuilders import TokenBuilder

# token reader for variable
class PrologVariableTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = None

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'variable')]

  def accept(self, candidate, c):
    result = False

    if c.isalpha() and c.isupper():
      result = True

    if len(candidate) > 0 and c.isdigit():
      result = True

    if c == '_':
      result = True

    return result
