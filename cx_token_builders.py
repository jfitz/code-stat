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

    return [Token(self.text, 'comment', False)]


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if candidate.startswith('//'):
      return True

    if candidate == '/':
      return c == '/'

    if candidate == '':
      return c == '/'

    return False


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

    return [Token(self.text, 'comment', False)]


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if candidate.startswith('///'):
      return True

    if candidate == '':
      return c == '/'

    if candidate == '/':
      return c == '/'

    if candidate == '//':
      return c == '/'

    return False


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

    return [Token(self.text, 'comment', False)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == '/'

    if len(candidate) == 1:
      return c == '*'

    return not candidate.endswith('*/')


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

    return [Token(self.text, 'type', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == '<'

    level = 0
    for ch in candidate:
      if ch == '<':
        level += 1
      if ch == '>' and level > 0:
        level -= 1

    if level > 0:
      return c.isalpha() or c.isdigit() or c in "</\\ ,_.:*>'"

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    level = 0
    for ch in self.text:
      if ch == '<':
        level += 1
      if ch == '>':
        level -= 1

    if level != 0:
      return 0

    if self.text[0] == '<' and self.text[-1] == '>':
      return len(self.text)
    
    return 0


# token reader for #include <name>
class IncludeNameTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, previous):
    self.text = ''
    self.previous = previous


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == '<'

    if candidate[-1] == '>':
      return False

    return c.isalpha() or c.isdigit() or c in ">/\\ ,_.:"


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text[-1] != '>':
      return 0

    if len(line_printable_tokens) != 1:
      return 0

    prev_token = line_printable_tokens[0]

    if prev_token.group != 'preprocessor':
      return 0

    if prev_token.text != self.previous:
      return 0

    return len(self.text)
