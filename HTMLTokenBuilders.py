from Token import Token
from TokenBuilders import (
  TokenBuilder,
  ListTokenBuilder
)

# token reader for identifier
class HTMLIdentifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'identifier')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isalpha()

    if len(candidate) > 0:
      result = c.isalpha() or c == '-'

    return result


# accept characters to match item in list
class HTMLListTokenBuilder(ListTokenBuilder):

  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    # must have at least one preceding token
    if len(line_printable_tokens) == 0:
      return 0

    prev_token = line_printable_tokens[-1]

    if prev_token.group != 'grouper':
      return 0

    if prev_token.text not in ['<', '</']:
      return 0

    return len(self.token)


# token reader for attribute
class HTMLAttributeTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'symbol')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c == '&'

    if len(candidate) > 0 and candidate[-1] != ';':
      result = c.isalpha() or c == ';'

    return result


  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    # must have at least three chars
    if len(self.token) < 3:
      return 0

    if self.token[0] != '&':
      return 0

    if self.token[-1] != ';':
      return 0

    return len(self.token)