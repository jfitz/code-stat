import string
from Token import Token
from TokenBuilders import TokenBuilder

# token reader for Hollerith string constant
class HollerithStringTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string')]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isdigit()

    if len(candidate) == 1:
      return c.isdigit() or c == 'H'

    if len(candidate) == 2 and candidate[1].isdigit():
      return c == 'H'

    if c not in string.printable:
      return False

    spec_length = 1 # for the H
    # compute length
    length = int(candidate[0])
    spec_length += 1 # for the digit

    if candidate[1].isdigit():
      length = length * 10 + int(candidate[1])
      spec_length += 1 # for the digit

    length += spec_length

    return len(candidate) < length


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 3:
      return 0

    return len(self.text)
