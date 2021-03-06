from codestat_token import Token
from token_builders import (
  TokenBuilder,
  CaseInsensitiveListTokenBuilder
)

# token reader for identifier
class HTMLIdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isalpha()

    return c.isalpha() or c == '-'


# accept characters to match item in list
class HTMLListTokenBuilder(CaseInsensitiveListTokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have at least one preceding token
    if len(line_printable_tokens) == 0:
      return 0

    prev_token = line_printable_tokens[-1]

    if prev_token.group != 'group':
      return 0

    if prev_token.text not in ['<', '</']:
      return 0

    return len(self.text)


# token reader for attribute
class HTMLAttributeTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'symbol', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == '&'

    if len(candidate) > 0 and candidate[-1] != ';':
      return c.isalpha() or c == ';'

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have at least three chars
    if len(self.text) < 3:
      return 0

    if self.text[0] != '&':
      return 0

    if self.text[-1] != ';':
      return 0

    return len(self.text)


# token reader for unusual unicode character
class HTMLUnicodeTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def attempt(self, text, start):
    self.text = None
    if len(text) > start:
      self.text = text[start]


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'character', True)]
