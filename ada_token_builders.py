from codestat_token import Token
from token_builders import TokenBuilder

# token reader for -- comment
class DashDashCommentTokenBuilder(TokenBuilder):
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
    if c in ['\n', '\r']:
      return False

    if candidate == '':
      return c == '-'

    if candidate == '-':
      return c == '-'

    return True


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text.startswith('--'):
      return len(self.text)
    
    return 0


# token reader for single-character text literal (string)
class AdaCharTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, quotes):
    self.quotes = quotes
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string', True)]


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if len(candidate) == 0:
      return c in self.quotes

    if len(candidate) == 1:
      return True

    # no quote stuffing, stop on second quote
    # assume escaped quotes are allowed
    quote_count = 0
    escaped = False
    for ch in candidate:
      if ch == candidate[0] and not escaped:
        quote_count += 1
      if ch == '\\':
        escaped = not escaped
      else:
        escaped = False

    return quote_count < 2


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[-1] != self.text[0]:
      return 0

    if '\\' in self.text:
      # at most four chars (two quotes, backslash, and one other)
      if len(self.text) > 4:
        return 0
      # backslash must be first char (and may repeat for second)
      if self.text[1] != '\\':
        return 0
    else:
      # at most three chars (two quotes, one character)
      if len(self.text) > 3:
        return 0

    if len(line_printable_tokens) > 0 and line_printable_tokens[-1].group == 'identifier':
      return 0

    return len(self.text)
