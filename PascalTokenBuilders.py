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
    result = None
    if result is None and c.isalpha():
      result = True
    if result is not None and c.isdigit():
      result = True
    if result is not None and c == '_':
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
    if c == "'" and candidate == '':
      result = True
    if c == "'" and len(candidate) > 0:
      result = True
    if c != "'" and len(candidate) == 1:
      result = True
    if c != "'" and len(candidate) > 1 and candidate[-1] != "'":
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
    return Token(self.token, 'comment')

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
    return Token(self.token, 'comment')

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
    return Token(self.token, 'newline')

  def accept(self, candidate, c):
    result = False
    if c == '\n' and candidate == '':
      result = True
    return result
