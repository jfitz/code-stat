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

# token reader for // comment
class HashCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''

  def get_tokens(self):
    if self.token is None:
      return None

    if self.token.startswith('#'):
      return [Token(self.token, 'comment')]

    return None

  def accept(self, candidate, c):
    result = False

    if candidate.startswith('#'):
      result = True

    if c == '#' and candidate == '':
      result = True

    if c == '\n':
      result = False

    return result

# token reader for """ """ comment
class TripleQuoteCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'comment')]

  def accept(self, candidate, c):
    result = False

    if c == '"' and len(candidate) < 3:
      result = True

    if candidate.startswith('"""'):
      result = True

    if len(candidate) > 5 and candidate[-3:] == '"""':
      result = False

    return result
