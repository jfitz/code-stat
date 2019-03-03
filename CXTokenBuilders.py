from Token import Token
from TokenBuilders import TokenBuilder

# token reader for // comment
class SlashSlashCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''

  def get_tokens(self):
    if self.text is None:
      return None

    if self.text.startswith('//'):
      return [Token(self.text, 'comment')]

    return None

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

# token reader for /* */ comment
class SlashStarCommentTokenBuilder(TokenBuilder):
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


# token reader for preprocessor directives
class CPreProcessorTokenBuilder(TokenBuilder):
  def __init__(self, prefixes):
    self.text = ''
    self.prefixes = prefixes


  def get_tokens(self):
    if self.text is None:
      return None

    if self.text.startswith('#'):
      return [Token(self.text, 'preprocessor')]

    return None


  def accept(self, candidate, c):
    result = False

    if candidate.startswith('#'):
      result = True

    if c == '#' and candidate == '':
      result = True

    if c in ['\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text.startswith(self.prefixes):
      return len(self.text)
    
    return 0
