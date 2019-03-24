import itertools
from Token import Token
from TokenBuilders import TokenBuilder

# token reader for variable
class VisualBasicVariableTokenBuilder(TokenBuilder):
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
      result = c.isalpha() or c.isdigit() or c in self.suffixes

    if len(candidate) > 1 and candidate[-1] in self.suffixes:
      result = False

    return result


# token reader for REMARK comment
class RemarkTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text == None:
      return None

    token1 = Token('', 'comment')
    token2 = Token('', 'comment')

    if self.text.startswith('Remark'):
      token1 = Token('Remark', 'keyword')
      token2 = Token(self.text[6:], 'comment')

    return [token1, token2]


  def accept(self, candidate, c):
    result = False

    if c == 'R' and candidate == '':
      result = True

    if c == 'e' and candidate == 'R':
      result = True

    if c == 'm' and candidate == 'Re':
      result = True

    if candidate.startswith('Rem'):
      result = True

    if c in ['\n', '\r']:
      result = False

    return result
