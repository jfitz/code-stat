import string
from Token import Token
from TokenBuilders import TokenBuilder

# token reader for identifier
class FortranIdentifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'identifier')]


  def accept(self, candidate, c):
    result = False

    if c.isupper():
      result = True

    if len(candidate) > 0 and c.isdigit():
      result = True

    return result


# token reader for number
class LineNumberTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'line number')]


  def accept(self, candidate, c):
    return c.isdigit()


  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    boost = 0

    if len(line_printable_tokens) > 0 and line_printable_tokens[-1].group == 'newline':
      boost += 0.5

    return len(self.token) + boost


# token reader for FORMAT specifiers (but not Hollerith specifiers)
class FormatSpecifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'format')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c in 'IFEDGALX'

    if len(candidate) > 0:
      result = c.isdigit() or (c == '.' and '.' not in candidate)

    return result


  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    if self.token[-1] == '.':
      return 0

    # if line is a FORMAT line, we're OK
    for token in line_printable_tokens:
      if token.text == 'FORMAT':
        return len(self.token)

    return 0
