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