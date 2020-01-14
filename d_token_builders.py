from codestat_token import Token
from token_builders import TokenBuilder

# token reader for /+ +/ comment that allows nesting
class NestedSlashPlusCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0 and c == '/':
      result = True

    if len(candidate) == 1 and c == '+':
      result = True

    if len(candidate) == 2:
      result = True

    # walk through candidate and verify open delimiters
    if len(candidate) > 2:
      level = 0
      prev = ''
      for c in candidate:
        if prev == '/' and c == '+':
          level += 1

        if prev == '+' and c == '/':
          level -= 1

        if level < 0:
          result = False

        prev = c

      if level > 0:
        result = True

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.text.startswith('/+'):
      return 0

    if not self.text.endswith('+/'):
      return 0

    # walk through text and verify matched delimiters
    level = 0
    prev = ''
    for c in self.text:
      if prev == '/' and c == '+':
        level += 1

      if prev == '+' and c == '/':
        level -= 1

      if level < 0:
        return 0

      prev = c

    if level != 0:
      return 0

    return len(self.text)
