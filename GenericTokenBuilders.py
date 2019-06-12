from Token import Token
from TokenBuilders import (
  TokenBuilder,
  CharTokenBuilder
)

# token reader for identifier
class GenericIdentifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = None

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier')]

  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isalpha()

    if len(candidate) > 0:
      result = c.isalpha() or c.isdigit() or c == '_'

    return result


class GenericNumberTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isdigit() or c in ['+', '-']

    if len(candidate) > 0:
      if candidate[-1] in ['E', 'e']:
        result = c.isdigit()
      elif candidate[-1].isdigit():
        result = c.isdigit() or c.isalpha() or c == '.'
      elif candidate[-1] in ['+', '-']:
        result = c.isdigit()
      elif candidate[-1].isalpha():
        result = c.isalpha()
      elif candidate[-1] == '.':
        result = c.isdigit()

    return result
