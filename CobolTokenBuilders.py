from Token import Token

# token reader for identifier
class IdentifierTokenBuilder:
  def __init__(self):
    self.token = None

  def attempt(self, text):
    self.token = None
    candidate = ''
    i = 0
    accepted = True
    while i < len(text) and accepted:
      c = text[i]
      accepted = self.accept(candidate, c)
      if accepted:
        candidate += c
      i += 1

    if len(candidate) > 0:
      self.token = candidate

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
class StringTokenBuilder:
  def __init__(self):
    self.token = ''

  def attempt(self, text):
    self.token = None
    candidate = ''
    i = 0
    accepted = True
    while i < len(text) and accepted:
      c = text[i]
      accepted = self.accept(candidate, c)
      if accepted:
        candidate += c
      i += 1

    if len(candidate) > 0:
      self.token = candidate

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
