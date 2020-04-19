from codestat_token import Token
from token_builders import TokenBuilder

# token reader for --[[ ]] comment
class LuaBlockCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment', False)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == '-'

    if len(candidate) == 1:
      return c == '-'

    if len(candidate) == 2:
      return c == '['

    if len(candidate) == 3:
      return c == '['

    return not candidate.endswith(']]')


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.text.endswith(']]'):
      return 0

    score = len(self.text)

    return score


# token reader for triple quote string
class DoubleBracketStringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.opener = '[['
    self.closer = ']]'
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string', False)]


  def accept(self, candidate, c):
    if len(candidate) < 2:
      return c == '['

    return not candidate.endswith(']]')


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 4:
      return 0

    if not self.text.startswith('[['):
      return 0

    if not self.text.endswith(']]'):
      return 0

    return len(self.text)
