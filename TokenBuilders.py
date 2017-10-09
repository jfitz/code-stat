from Token import Token

# accept any character (but only one)
class InvalidTokenBuilder:
  def __init__(self):
    self.token = None

  def attempt(self, text):
    self.token = None
    if len(text) > 0:
      self.token = text[0]

  def get_token(self):
    if self.token is None:
      return None
    return Token(self.token, 'invalid')
  
# token reader for whitespace
class WhitespaceTokenBuilder:
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
    return Token(self.token, 'whitespace')

  def accept(self, candidate, c):
    return c.isspace()

# token reader for number
class NumberTokenBuilder:
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
    return Token(self.token, 'number')

  def accept(self, candidate, c):
    return c.isdigit()

# accept characters to match item in list
class ListTokenBuilder:
  def __init__(self, legals, group):
    self.legals = legals
    self.group = group
    self.token = ''

  def attempt(self, text):
    self.token = None
    best_candidate = ''
    candidate = ''
    i = 0
    accepted = True
    while i < len(text) and accepted:
      c = text[i]
      accepted = self.accept(candidate, c)
      if accepted:
        candidate += c
        i += 1
        if candidate in self.legals:
          best_candidate = candidate

    if len(best_candidate) > 0:
      self.token = best_candidate

  def get_token(self):
    if self.token is None:
      return None
    return Token(self.token, self.group)

  def accept(self, candidate, c):
    token = candidate + c
    count = len(token)
    result = False

    for legal in self.legals:
      if legal[:count] == token:
        result = True

    return result
