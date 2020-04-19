from codestat_token import Token
from token_builders import TokenBuilder

# token reader for real with exponent
class HexRealExponentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.prefix = '0x'
    self.letter = 'p'
    self.extra_char = '_'
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number', True)]


  def accept(self, candidate, c):
    result = False

    hex_digits = ['abcdefABCDEF']

    if len(candidate) < len(self.prefix):
      result = c.lower() == self.prefix[len(candidate)]

    if c.isdigit():
      result = True

    if c in hex_digits:
      result = self.letter not in candidate.lower()

    if self.extra_char is not None and c == self.extra_char:
      result = len(candidate) > 0 and (candidate[-1].isdigit() or c in hex_digits)

    if c.lower() == self.letter\
      and len(candidate) > 0 and\
      self.letter not in candidate.lower():
      result = True

    if c in ['+', '-'] and\
      len(candidate) > 0 and\
      candidate[-1].lower() == self.letter:
      result = True

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have prefix, digit, 'P', and digit
    if len(self.text) <= len(self.prefix) + 3:
      return 0

    if not self.letter in self.text.lower():
      return 0

    if self.text[-1].lower() == self.letter:
      return 0

    return len(self.text)
