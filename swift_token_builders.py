from codestat_token import Token
from token_builders import (
  TokenBuilder,
  PrefixedIdentifierTokenBuilder
)


# token reader for integer
class SwiftArgumentTokenBuilder(TokenBuilder):
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
    result = False

    if len(candidate) == 0:
      result = c == '$'
    
    if len(candidate) > 0:
      result = c in '0123456789'

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    return len(self.text)


class SwiftSymbolTokenBuilder(PrefixedIdentifierTokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, prefix, group, is_operand):
    super().__init__(prefix, group, is_operand)


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    types = ['identifier', 'number']
    if len(line_printable_tokens) > 0 and line_printable_tokens[-1].group in types:
      return 0

    if not self.text.startswith(self.prefix):
      return 0

    return len(self.text)
