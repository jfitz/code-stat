from codestat_token import Token
from token_builders import TokenBuilder

# token reader for brace comment
class BraceCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment')]

  def accept(self, candidate, c):
    result = False

    if c == '{' and candidate == '':
      result = True

    if c == '}' and len(candidate) > 0:
      result = True

    if len(candidate) > 0 and candidate[-1] != '}':
      result = True

    return result
