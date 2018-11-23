from Token import Token

# generic TokenBuilder (to hold common functions)
class TokenBuilder:
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

  def accept(self, candidate, c):
    return False

  def get_count(self):
    if self.token is None:
      return 0

    return len(self.token)

# accept any character (but only one)
class InvalidTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def attempt(self, text):
    self.token = None
    if len(text) > 0:
      self.token = text[0]

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'invalid')]
  
# token reader for whitespace
class WhitespaceTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'whitespace')]

  def accept(self, candidate, c):
    result = c.isspace() and c != '\n' and c != '\r'

    return result

# token reader for newline
class NewlineTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'newline')]

  def accept(self, candidate, c):
    result = False

    if candidate == '':
      result = c == '\r' or c == '\n'

    if candidate == '\r' and c == '\n':
      result = True

    return result

# token reader for number
class NumberTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'number')]

  def accept(self, candidate, c):
    return c.isdigit()

# accept characters to match item in list
class ListTokenBuilder(TokenBuilder):
  def __init__(self, legals, group, case_sensitive):
    self.legals = legals
    self.group = group
    self.case_sensitive = case_sensitive
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
        if self.case_sensitive:
          if candidate in self.legals:
            best_candidate = candidate
        else:
          if candidate.lower() in self.legals:
            best_candidate = candidate

    if len(best_candidate) > 0:
      self.token = best_candidate

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, self.group)]

  def accept(self, candidate, c):
    token = candidate + c
    count = len(token)
    result = False

    if self.case_sensitive:
      for legal in self.legals:
        if legal[:count] == token:
          result = True
    else:
      for legal in self.legals:
        if legal[:count] == token.lower():
          result = True

    return result
