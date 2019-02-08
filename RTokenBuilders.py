from Token import Token
from TokenBuilders import TokenBuilder

# token reader for user-defined operator
class ROperatorTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'operator')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0 and c == '%':
      result = True

    if len(candidate) == 1:
      result = True

    if len(candidate) > 1 and candidate[-1] != candidate[0]:
        result = True

    if c in [' ', '\t', '\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    # must have at least two chars, start and end with percent
    if len(self.token) < 2:
      return 0

    if self.token[0] != '%':
      return 0

    if self.token[-1] != '%':
      return 0

    return len(self.token)
