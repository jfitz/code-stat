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


  def get_score(self, last_printable_token):
    if self.token is None:
      return 0

    boost = 0

    if last_printable_token.group == 'newline':
      boost += 0.5

    return len(self.token) + boost


# token reader for hollerith string constant
class HollerithStringTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'string')]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isdigit()

    if len(candidate) == 1:
      return c.isdigit() or c == 'H'

    if len(candidate) == 2 and candidate[1].isdigit():
      return c == 'H'

    spec_length = 1 # for the H
    # compute length
    length = int(candidate[0])
    spec_length += 1 # for the digit

    if candidate[1].isdigit():
      length = length * 10 + int(candidate[1])
      spec_length += 1 # for the digit

    length += spec_length

    return len(candidate) < length

  def get_score(self, last_printable_token):
    if self.token is None:
      return 0

    boost = 0

    if last_printable_token.group == 'newline':
      boost += 0.5

    return len(self.token) + boost
