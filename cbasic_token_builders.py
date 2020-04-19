from codestat_token import Token
from token_builders import TokenBuilder

# token reader for variable
class CBasicVariableTokenBuilder(TokenBuilder):
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

    result = False

    if len(candidate) > 0:
      result = c.isalpha() or c.isdigit() or c == '.' or c in self.suffixes

    if len(candidate) > 1 and candidate[-1] in self.suffixes:
      result = False

    return result


# token reader for label
class CBasicLabelTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, keywords):
    self.text = None
    self.keywords = keywords

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'label', False)]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isalpha()

    if len(candidate) > 0:
      result = c.isalpha() or c.isdigit() or c == '.' or c == ':'

    if len(candidate) > 1 and candidate[-1] == ':':
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(line_printable_tokens) > 0:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[-1] != ':':
      return 0

    if self.text[:-1] in self.keywords:
      return 0

    return len(self.text)


# token reader for integer
class CBasicSuffixedIntegerTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, allow_chars, suffix_char):
    self.text = None
    self.allow_chars = allow_chars.lower()
    self.suffix_char = suffix_char.lower()


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number', True)]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isdigit()
    
    if len(candidate) > 0:
      result = c.lower() in self.allow_chars or\
               c.lower() in self.suffix_char

    if len(candidate) > 0 and candidate[-1].lower() in self.suffix_char:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[-1].lower() not in self.suffix_char:
      return 0

    return len(self.text)


# token reader for line continuation (and comment)
class CBasicLineContinuationTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    # line continuation character
    tokens = [Token(self.text[0], 'line continuation', False)]
    rest = self.text[1:]

    # whitespace before text
    whitespace = ''
    while rest.startswith(' '):
      whitespace += rest[0]
      rest = rest[1:]

    if whitespace != '':
      tokens.append(Token(whitespace, 'whitespace', False))

    # text (which may contain whitespace)
    if rest != '':
      tokens.append(Token(rest, 'comment', False))
    
    return tokens


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == '\\'

    return c not in ['\r', '\n']
