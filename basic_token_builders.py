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

    return [Token(self.text, 'identifier', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isalpha()

    if len(candidate) == 1:
      return c.isdigit() or c in self.suffixes

    if candidate[-1] in self.suffixes:
      return False

    return c in self.suffixes


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

    return [Token(self.text, 'identifier', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isalpha()

    if candidate[-1] in self.suffixes:
      return False

    return c.isalpha() or c.isdigit() or c in self.suffixes


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

    token1 = Token('', 'comment', False)
    token2 = Token('', 'comment', False)

    if self.text.startswith('REM'):
      token1 = Token('REM', 'keyword', False)
      token2 = Token(self.text[3:], 'comment', False)

    if self.text.startswith('REMARK'):
      token1 = Token('REMARK', 'keyword', False)
      token2 = Token(self.text[6:], 'comment', False)

    return [token1, token2]


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if candidate == '':
      return c == 'R'

    if candidate == 'R':
      return c == 'E'

    if candidate == 'RE':
      return c == 'M'

    return True


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 3:
      return 0

    if not self.text.startswith("REM"):
      return 0

    return len(self.text)


# token reader for user-defined function
class UserFunctionTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, suffixes):
    self.suffixes = suffixes
    self.text = ''


  def get_tokens(self):
    if self.text == None:
      return None

    return [Token(self.text, 'function', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == 'F'

    if len(candidate) == 1:
      return c == 'N'

    if len(candidate) == 2:
      return c.isalpha()

    return c in self.suffixes


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.text.startswith("FN"):
      return 0

    if len(self.text) < 3:
      return 0

    return len(self.text)


# token reader for user-defined function
class LongUserFunctionTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, suffixes):
    self.suffixes = suffixes
    self.text = ''


  def get_tokens(self):
    if self.text == None:
      return None

    return [Token(self.text, 'function', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == 'F'

    if len(candidate) == 1:
      return c == 'N'

    if candidate[-1] in self.suffixes:
      return False

    if candidate[-1].isalpha():
      return c.isalpha() or c.isdigit() or c in self.suffixes

    return c.isdigit() or c in self.suffixes


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.text.startswith("FN"):
      return 0

    if len(self.text) < 3:
      return 0

    return len(self.text)


class HardwareFunctionTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text == None:
      return None

    return [Token(self.text, 'function', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == 'U'

    if len(candidate) == 1:
      return c == 'S'

    if len(candidate) == 2:
      return c == 'R'

    if len(candidate) == 3:
      return c.isdigit()

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.text.startswith("USR"):
      return 0

    return len(self.text)
