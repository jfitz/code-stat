from Token import Token
from TokenBuilders import TokenBuilder

# token reader for identifier
class HTMLIdentifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'identifier')]

  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isalpha() or c == '$'

    if len(candidate) == 1:
      if candidate == '$':
        result = c.isalpha() or c == '_'
      else:
        result = c.isalpha() or c in ['-', '_']

    if len(candidate) > 1:
      result = c.isalpha() or c in ['-', '_']

    return result


# token reader for attribute
class HTMLAttributeTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'symbol')]

  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c == '&'

    if len(candidate) > 0 and candidate[-1] != ';':
      result = c.isalpha() or c == ';'

    return result


  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    # must have at least three chars
    if len(self.token) < 3:
      return 0

    if self.token[0] != '&':
      return 0

    if self.token[-1] != ';':
      return 0

    return len(self.token)
