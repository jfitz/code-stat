from codestat_token import Token
from token_builders import TokenBuilder

# token reader for identifier
class RustRawStringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string')]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == 'r'

    if len(candidate) == 1:
      return c in ['#', '"']

    if '"' in candidate:
      # count hashes
      parts = candidate.split('"')
      num_hashes = len(parts[0]) - 1
      # build suffix
      suffix = '"' + '#' * num_hashes
      return not candidate.endswith(suffix)

    return c in ['#', '"']


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 3:
      return 0

    if '"' not in self.text:
      return 0

    # count number of hashes (may be zero)
    parts = self.text.split('"')
    num_hashes = len(parts[0]) - 1
    # build prefix and suffix
    prefix = 'r' + '#' * num_hashes + '"'
    suffix = '"' + '#' * num_hashes

    if not self.text.startswith(prefix):
      return 0

    if not self.text.endswith(suffix):
      return 0

    return len(self.text)


# token reader for attribute (inner or outer)
class RustAttributeTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'attribute')]


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if len(candidate) == 0:
      return c == '#'

    if len(candidate) == 1:
      return c in ['!', '[']

    if candidate == '#!':
      return c == '['

    return candidate[-1] != ']'


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 3:
      return 0

    if self.text[-1] != ']':
      return 0

    return len(self.text)
