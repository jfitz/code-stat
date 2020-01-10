from codestat_token import Token
from token_builders import TokenBuilder

# token reader for variable
class BasicVariableTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, suffixes):
    self.text = None
    self.suffixes = suffixes


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0 and c.isalpha():
      result = True

    if len(candidate) == 1 and c.isdigit():
      result = True

    if len(candidate) > 0 and (candidate[-1].isalpha or candidate[-1].isdigit()) and c in self.suffixes:
      result = True

    return result


# token reader for variable
class BasicLongVariableTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, suffixes):
    self.text = None
    self.suffixes = suffixes


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0 and c.isalpha():
      result = True

    if len(candidate) > 0:
      if candidate[-1] not in self.suffixes:
        result = c.isalpha() or c.isdigit() or c in self.suffixes

    return result


# token reader for REMARK comment
class RemarkTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text == None:
      return None

    token1 = Token('', 'comment')
    token2 = Token('', 'comment')

    if self.text.startswith('REM'):
      token1 = Token('REM', 'keyword')
      token2 = Token(self.text[3:], 'comment')

    if self.text.startswith('REMARK'):
      token1 = Token('REMARK', 'keyword')
      token2 = Token(self.text[6:], 'comment')

    return [token1, token2]


  def accept(self, candidate, c):
    result = False

    if c == 'R' and candidate == '':
      result = True

    if c == 'E' and candidate == 'R':
      result = True

    if c == 'M' and candidate == 'RE':
      result = True

    if candidate.startswith('REM'):
      result = True

    if c in ['\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 3:
      return 0

    if not self.text.startswith("REM"):
      return 0

    return len(self.text)
