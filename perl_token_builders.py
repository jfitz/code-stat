from codestat_token import Token
from token_builders import TokenBuilder

# token reader for user-defined operator
class PerlIdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier', True)]


  def accept(self, candidate, c):
    if c in [' ', '\t', '\n', '\r']:
      return False

    if len(candidate) == 0:
      return c in '$@%#'

    if candidate[-1] in '$@%#':
      return c in '$@%#' or c.isalpha() or c.isalnum() or c == '_'

    return c.isalpha() or c.isalnum() or c == '_'


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have at least two chars
    if len(self.text) < 2:
      return 0

    # cannot end with special prefix char
    if self.text[-1] in '$@%#':
      return 0

    return len(self.text)


# token reader for user-defined operator
class PerlQStringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == 'q'

    if len(candidate) == 1:
      return c in 'qrwx{'

    if candidate in ['qq', 'qr', 'qw', 'qx']:
      return c == '{'

    return PerlQStringTokenBuilder.count_level(candidate) > 0


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have at least three chars
    if len(self.text) < 3:
      return 0

    # must contain { and }
    if self.text[-1] != '}':
      return 0

    # must have balanced braces
    if PerlQStringTokenBuilder.count_level(self.text) > 0:
      return 0

    return len(self.text)


  @staticmethod
  def count_level(candidate):
    level = 0
    escaped = False

    for ch in candidate:
      if ch == '{' and not escaped:
        level += 1

      if ch == '}' and not escaped and level > 0:
        level -= 1

      if ch == '\\':
        escaped = not escaped
      else:
        escaped = False
    
    return level
