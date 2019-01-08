from Token import Token
from TokenBuilders import TokenBuilder

# token reader for identifier
class IdentifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'identifier')]

  def accept(self, candidate, c):
    result = False

    if c.isalpha():
      result = True

    if len(candidate) > 0 and c.isdigit():
      result = True

    if c == '_':
      result = True

    return result

# token reader for // comment
class SlashSlashCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''

  def get_tokens(self):
    if self.token is None:
      return None

    if self.token.startswith('//'):
      return [Token(self.token, 'comment')]

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
    self.token = ''

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'comment')]

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
    self.token = ''
    self.prefixes = prefixes


  def get_tokens(self):
    if self.token is None:
      return None

    if self.token.startswith('#'):
      return [Token(self.token, 'preprocessor')]

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
    if self.token is None:
      return 0

    if self.token.startswith(self.prefixes):
      return len(self.token)
    
    return 0
