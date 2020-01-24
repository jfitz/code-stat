from codestat_token import Token
from token_builders import TokenBuilder

# token reader for // comment
class SlashSlashCommentTokenBuilder(TokenBuilder):
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

    if candidate.startswith('//'):
      result = True

    if c == '/' and candidate == '/':
      result = True

    if c == '/' and candidate == '':
      result = True

    if c in ['\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text.startswith('//'):
      return len(self.text)
    
    return 0


# token reader for /// comment
class TripleSlashCommentTokenBuilder(TokenBuilder):
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

    if candidate == '':
      result = c == '/'

    if candidate == '/':
      result = c == '/'

    if candidate == '//':
      result = c == '/'

    if candidate.startswith('///'):
      result = True

    if c in ['\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text.startswith('///'):
      return len(self.text)
    
    return 0


# token reader for /* */ comment
class SlashStarCommentTokenBuilder(TokenBuilder):
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

    if self.text.startswith('/*') and self.text.endswith('*/'):
      return len(self.text)
    
    return 0


# token reader for <name> class identifier
class ClassTypeTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'type')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c == '<'

    if len(candidate) > 0 and candidate[-1] != '>':
      result = c.isalpha() or c.isdigit() or c in "/\\ ,_.:*>'"

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text[0] == '<' and self.text[-1] == '>':
      return len(self.text)
    
    return 0
