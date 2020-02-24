from codestat_token import Token
from token_builders import TokenBuilder


# token reader for identifier
class DbaseIdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isalpha() or c == ':'

    if len(candidate) > 0:
      result = c.isalpha() or c.isdigit() or c == ':'

    return result


# token reader for identifier
class DbaseFilenameTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'filename')]


  def accept(self, candidate, c):
    result = c.isalpha() or c.isdigit() or c == '.'

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    num_dots = self.text.count('.')

    if num_dots > 1:
      return 0

    return len(self.text)


# accept characters to match item in list
class DbaseEndifTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    token1 = Token(self.text[:5], 'keyword')
    token2 = Token(self.text[5:], 'comment')
    tokens = [token1, token2]
    return tokens


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    token = candidate + c

    keyword = 'endif'
    if len(token) < 5:
      keyword = keyword[:len(token)]

    result = token.lower().startswith(keyword)

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    score = 0

    if self.text.lower().startswith('endif'):
      score = len(self.text)

    return score
