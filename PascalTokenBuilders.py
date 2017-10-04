class Token:
  def __init__(self, text, invalid):
    self.text = text
    self.invalid = invalid

  def __str__(self):
    return self.text

  def count(self):
    return len(self.text)

  def whitespace(self):
    return self.text[0].isspace()

  def comment(self):
    return self.text.startswith('(*') or self.text[0] == '{'

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
    return Token(self.token, True)
  
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
    return Token(self.token, False)

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
    return Token(self.token, False)

  def accept(self, candidate, c):
    return c.isdigit()

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
    return Token(self.token, False)

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
    return Token(self.token, False)

  def accept(self, candidate, c):
    result = False
    if c == "'" and candidate == '':
      result = True
    if c == "'" and len(candidate) > 0:
      result = True
    if c != "'" and len(candidate) == 1:
      result = True
    if c != "'" and len(candidate) > 1 and candidate[-1] != "'":
      result = True
    return result

# accept characters to match item in list
class ListTokenBuilder:
  def __init__(self, legals):
    self.legals = legals
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
    return Token(self.token, False)

  def accept(self, candidate, c):
    token = candidate + c
    count = len(token)
    result = False

    for legal in self.legals:
      if legal[:count] == token:
        result = True

    return result

# token reader for brace comment
class BraceCommentTokenBuilder:
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
    return Token(self.token, False)

  def accept(self, candidate, c):
    result = False
    if c == '{' and candidate == '':
      result = True
    if c == '}' and len(candidate) > 0:
      result = True
    if len(candidate) > 0 and candidate[-1] != '}':
      result = True
    return result

# token reader for comment
class CommentTokenBuilder:
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
    return Token(self.token, False)

  def accept(self, candidate, c):
    result = False
    if c == '(' and candidate == '':
      result = True
    if c == '*' and len(candidate) == 1:
      result = True
    if c == ')' and len(candidate) > 2 and candidate[-1] == '*':
      result = True
    if candidate.startswith('(*') and candidate[-1] != ')':
      result = True
    return result

# token reader for newline
class NewlineTokenBuilder:
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
    return Token(self.token, False)

  def accept(self, candidate, c):
    result = False
    if c == '\n' and candidate == '':
      result = True
    return result
