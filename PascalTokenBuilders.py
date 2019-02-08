from Token import Token
from TokenBuilders import TokenBuilder

# token reader for brace comment
class BraceCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'comment')]

  def accept(self, candidate, c):
    result = False

    if c == '{' and candidate == '':
      result = True

    if c == '}' and len(candidate) > 0:
      result = True

    if len(candidate) > 0 and candidate[-1] != '}':
      result = True

    return result

# token reader for comment
class ParenStarCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'comment')]

  def accept(self, candidate, c):
    result = False

    if c == '(' and len(candidate) == 0:
      result = True

    if c == '*' and len(candidate) == 1:
      result = True

    if c == ')' and len(candidate) > 2 and candidate[-1] == '*':
      result = True

    if candidate.startswith('(*') and (candidate[-2] != '*' or candidate[-1] != ')'):
      result = True

    return result
