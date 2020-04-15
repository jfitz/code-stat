from codestat_token import Token
from token_builders import TokenBuilder

# token reader for label
class PL1LabelTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'label')]


  def accept(self, candidate, c):
    if len(candidate) > 1 and candidate[-1] == ':':
      return False

    if len(candidate) > 1:
      return c.isalpha() or c.isdigit() or c == ':'

    return c.isalpha()


# token reader for start comment
class PL1CommentStartTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment-start')]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == '/'

    if len(candidate) == 1:
      return c == '*'

    return not candidate.endswith('*/')


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text.startswith('/*') and not self.text.endswith('*/'):
      return len(self.text)

    return 0


# token reader for middle comment
class PL1CommentMiddleTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment-middle')]

  def accept(self, candidate, c):
    return not candidate.endswith('*/')


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.text.endswith('*/'):
      return len(self.text)

    return 0


# token reader for end comment
class PL1CommentEndTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment-end')]

  def accept(self, candidate, c):
    return not candidate.endswith('*/')


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text.endswith('*/'):
      return len(self.text)

    return 0
