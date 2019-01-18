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


# token reader for raw triple quote string
class RawTripleQuoteCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.prefix = 'r'
    self.token = ''

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'string')]

  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0 and c == 'r':
      result = True

    if len(candidate) == 1 and c in '"\'':
      result = True

    if len(candidate) in [2, 3]:
      result = c == candidate[1]

    if len(candidate) > 3 and candidate[1:4] in ['"""', "'''"]:
      result = True

    if len(candidate) > 6 and candidate[-3:] == candidate[1:4]:
      result = False

    return result
