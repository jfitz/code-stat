from Token import Token
from TokenBuilders import TokenBuilder

# token reader for identifier
class IdentifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def get_token(self):
    if self.token is None:
      return None
    return Token(self.token, 'identifier')

  def accept(self, candidate, c):
    result = False
    if c.isalpha():
      result = True
    if len(candidate) > 0 and c.isdigit():
      result = True
    if len(candidate) > 0 and c == '-':
      result = True
    return result

# token reader for text literal (string)
class StringTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''

  def get_token(self):
    if self.token is None:
      return None
    return Token(self.token, 'string')

  def accept(self, candidate, c):
    result = False
    if (c == '"' or c == "'") and candidate == '':
      result = True
    if (c == '"' or c == "'") and len(candidate) > 0 and c == candidate[0]:
      result = True
    if len(candidate) == 1 and c != candidate[0]:
      result = True
    if len(candidate) > 1 and candidate[-1] != candidate[0]:
      result = True
    return result

# token reader for PIC descriptor
class PictureTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''

  def get_token(self):
    if self.token is None:
      return None
    return Token(self.token, 'picture')

  def accept(self, candidate, c):
    result = False
    num_lparens = candidate.count('(')
    num_rparens = candidate.count(')')

    if num_rparens == num_lparens:
      if c == '9' or c == 'X' or c == 'V':
        result = True
      if c == '(':
        result = True

    if num_rparens == num_lparens - 1:
      if c.isdigit():
        result = True
      if c == ')':
        result = True

    return result
