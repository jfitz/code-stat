# accept any character (but only one)
class InvalidTokenBuilder:
  def __init__(self):
    self.token = ''

  def attempt(self, text):
    self.token = ''
    if len(text) > 0:
      self.token = text[0]

# token reader for whitespace
class WhitespaceTokenBuilder:
  def __init__(self):
    self.token = ''

  def attempt(self, text):
    candidate = ''
    i = 0
    accepted = True
    while i < len(text) and accepted:
      c = text[i]
      accepted = self.accept(candidate, c)
      if accepted:
        candidate += c
      i += 1

    self.token = candidate

  def accept(self, candidate, c):
    return c.isspace()

# token reader for number
class NumberTokenBuilder:
  def __init__(self):
    self.token = ''

  def attempt(self, text):
    candidate = ''
    i = 0
    accepted = True
    while i < len(text) and accepted:
      c = text[i]
      accepted = self.accept(candidate, c)
      if accepted:
        candidate += c
      i += 1

    self.token = candidate

  def accept(self, candidate, c):
    return c.isdigit()

# token reader for identifier
class IdentifierTokenBuilder:
  def __init__(self):
    self.token = ''

  def attempt(self, text):
    candidate = ''
    i = 0
    accepted = True
    while i < len(text) and accepted:
      c = text[i]
      accepted = self.accept(candidate, c)
      if accepted:
        candidate += c
      i += 1

    self.token = candidate

  def accept(self, candidate, c):
    result = None
    if result is None and c.isalpha():
      result = True
    if result is not None and c.isdigit():
      result = True
    if result is not None and (c == '$' or c == '%'):
      result = True
    return result

# token reader for text literal (string)
class StringTokenBuilder:
  def __init__(self):
    self.token = ''

  def attempt(self, text):
    candidate = ''
    i = 0
    accepted = True
    while i < len(text) and accepted:
      c = text[i]
      accepted = self.accept(candidate, c)
      if accepted:
        candidate += c
      i += 1

    self.token = candidate

  def accept(self, candidate, c):
    result = False
    if c == '"' and candidate == '':
      result = True
    if c == '"' and len(candidate) > 0:
      result = True
    if c != '"' and len(candidate) == 1:
      result = True
    if c != '"' and len(candidate) > 1 and candidate[-1] != '"':
      result = True
    return result

# accept characters to match item in list
class ListTokenBuilder:
  def __init__(self, legals):
    self.legals = legals

  def attempt(self, text):
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

    self.token = best_candidate

  def accept(self, candidate, c):
    token = candidate + c
    count = len(token)
    result = False

    for legal in self.legals:
      if legal[:count] == token:
        result = True

    return result
