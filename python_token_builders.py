from codestat_token import Token
from token_builders import TokenBuilder

# token reader for raw triple quote string
class RawTripleQuoteCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.prefix = 'r'
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string')]


  def accept(self, candidate, c):
    if len(candidate) > 6:
      return candidate[-3:] != candidate[1:4]

    if len(candidate) > 3:
      return candidate[1:4] in ['"""', "'''"]

    if len(candidate) > 1:
      return c == candidate[1]

    if len(candidate) == 1:
      return c in '"\''

    return c == 'r'
