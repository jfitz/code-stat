from Token import Token
from TokenBuilders import TokenBuilder

# token reader for start comment
class PL1CommentStartTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment-start')]

  def accept(self, candidate, c):
    result = False

    if c == '/' and len(candidate) == 0:
      result = True

    if c == '*' and len(candidate) == 1:
      result = True

    if c == '/' and len(candidate) > 2 and candidate[-1] == '*':
      result = True

    if candidate.startswith('/*') and (candidate[-2] != '*' or candidate[-1] != '/'):
      result = True

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text.startswith('/*') and not self.text.endswith('*/'):
      return len(self.text)

    return 0


# token reader for middle comment
class PL1CommentMiddleTokenBuilder(TokenBuilder):
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
