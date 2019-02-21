from Token import Token
from TokenBuilders import TokenBuilder

# token reader for variable
class CBasicVariableTokenBuilder(TokenBuilder):
  def __init__(self, suffixes):
    self.token = None
    self.suffixes = suffixes


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'identifier')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isalpha()

    if len(candidate) > 0:
      result = c.isalpha() or c.isdigit() or c == '.' or c in self.suffixes

    if len(candidate) > 1 and candidate[-1] in self.suffixes:
      result = False

    return result


# token reader for integer
class CBasicSuffixedIntegerTokenBuilder(TokenBuilder):
  def __init__(self, allow_chars, suffix_char):
    self.token = None
    self.allow_chars = allow_chars
    self.suffix_char = suffix_char


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'number')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isdigit()
    
    if len(candidate) > 0:
      result = c.lower() in self.allow_chars.lower() or\
               c.lower() in self.suffix_char.lower()

    if len(candidate) > 0 and candidate[-1].lower() in self.suffix_char.lower():
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    if len(self.token) < 2:
      return 0

    if self.token[-1].lower() != 'h':
      return 0

    return len(self.token)
