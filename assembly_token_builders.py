from codestat_token import Token
from token_builders import TokenBuilder

# token reader for comment
class AssemblyCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, legals):
    self.legals = legals
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment', False)]


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if candidate == '':
      return c in self.legals

    if candidate[0] in self.legals:
      return True

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(line_printable_tokens) > 0:
      return 0

    if self.text[0] in self.legals:
      return len(self.text)
    
    return 0


# token reader for identifier
class LabelTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, lead_extras, extras, suffixes):
    self.lead_extras = lead_extras
    self.suffixes = suffixes
    self.extras = extras
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'label', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isalpha() or c in self.lead_extras

    if len(candidate) > 1 and candidate[-1] in self.suffixes:
      return False

    return c.isalpha() or c.isdigit() or c in self.extras or c in self.suffixes


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(line_printable_tokens) > 0:
      return 0

    if self.text[-1] in self.suffixes:
      return len(self.text)
    
    return 0


# token reader for prefixed text literal (string)
class MultilineCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.prefix = 'COMMENT'
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment', True)]


  def accept(self, candidate, c):
    if len(candidate) < len(self.prefix):
      return c == self.prefix[len(candidate)]
    
    if len(candidate) == len(self.prefix):
      return c.isspace()

    rest = candidate[len(self.prefix):].strip()

    if len(rest) < 3:
      return True

    return rest[-1] != rest[0] or rest[-2] not in ['\n', '\r']


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < len(self.prefix) + 3:
      return 0

    rest = self.text[len(self.prefix):].strip()

    if rest[-1] != rest[0] or rest[-2] not in ['\n', '\r']:
      return 0

    return len(self.text)
