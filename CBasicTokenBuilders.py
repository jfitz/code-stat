from Token import Token
from TokenBuilders import TokenBuilder

# token reader for variable
class CBasicVariableTokenBuilder(TokenBuilder):
  def __init__(self, suffixes):
    self.text = None
    self.suffixes = suffixes


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier')]


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
    self.text = None
    self.allow_chars = allow_chars
    self.suffix_char = suffix_char


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number')]


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
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[-1].lower() != 'h':
      return 0

    return len(self.text)


# token reader for line continuation (and comment)
class CBasicLineContinuationTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    # line continuation character
    tokens = [Token(self.text[0], 'line continuation')]
    rest = self.text[1:]

    # whitespace before text
    whitespace = ''
    while rest.startswith(' '):
      whitespace += rest[0]
      rest = rest[1:]

    if whitespace != '':
      tokens.append(Token(whitespace, 'whitespace'))

    # text (which may contain whitespace)
    if rest != '':
      tokens.append(Token(rest, 'comment'))
    
    return tokens


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c == '\\'

    if len(candidate) > 0:
      result = c not in ['\r', '\n']

    return result
