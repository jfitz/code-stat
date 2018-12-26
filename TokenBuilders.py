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


  def get_score(self, last_printable_token):
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


# token reader for text literal (string)
class StringTokenBuilder(TokenBuilder):
  def __init__(self, quotes):
    self.quotes = quotes
    self.token = ''


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'string')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0 and c in self.quotes:
      result = True

    if len(candidate) == 1:
      result = True

    if len(candidate) > 1 and candidate[-1] != candidate[0]:
      result = True

    if c == '\n' or c == '\r':
      result = False

    return result


# token reader for integer
class IntegerTokenBuilder(TokenBuilder):
  def __init__(self, allow_underscore):
    self.token = None
    self.allow_underscore = allow_underscore


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'number')]


  def accept(self, candidate, c):
    result = False

    if c.isdigit():
      result = True
    
    if c == '_' and self.allow_underscore:
      result = True

    return result


# token reader for integer with exponent
class IntegerExponentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'number')]


  def accept(self, candidate, c):
    result = False

    if c.isdigit():
      result = True
    
    if c.lower() == 'e' and len(candidate) > 0 and 'e' not in candidate.lower():
      result = True

    return result


# token reader for real (no exponent)
class RealTokenBuilder(TokenBuilder):
  def __init__(self, require_before, require_after):
    self.token = None
    self.require_before = require_before
    self.require_after = require_after


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'number')]


  def accept(self, candidate, c):
    result = False

    if c.isdigit():
      result = True
    
    if c == '.' and '.' not in candidate:
      result = True

    return result


  def get_score(self, last_printable_token):
    if self.token is None:
      return 0

    if len(self.token) < 2:
      return 0

    if '.' not in self.token:
      return 0

    if self.require_before and not self.token[0].isdigit():
      return 0

    if self.require_after and not self.token[-1].isdigit():
      return 0

    return len(self.token)


# token reader for real with exponent
class RealExponentTokenBuilder(TokenBuilder):
  def __init__(self, require_before, require_after):
    self.token = None
    self.require_before = require_before
    self.require_after = require_after


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'number')]


  def accept(self, candidate, c):
    result = False

    if c.isdigit():
      result = True

    if c == '.' and '.' not in candidate and 'e' not in candidate.lower():
      result = True

    if c.lower() == 'e' and len(candidate) > 0 and 'e' not in candidate.lower():
      result = True

    if c in ['+', '-'] and len(candidate) > 0 and candidate[-1].lower() == 'e':
      result = True

    return result


  def get_score(self, last_printable_token):
    if self.token is None:
      return 0

    # must have digit, decimal, 'E', and digit (or decimal, digit, 'E', digit)
    if len(self.token) < 4:
      return 0

    if '.' not in self.token:
      return 0

    if not 'e' in self.token.lower():
      return 0

    if not self.token[-1].isdigit():
      return 0

    if self.require_before and not self.token[0].isdigit():
      return 0

    point_position = self.token.find('.')

    if self.require_after and not self.token[point_position + 1].isdigit():
      return 0

    return len(self.token)


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
          if candidate.lower() in (legal.lower() for legal in self.legals):
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
        if legal[:count].lower() == token.lower():
          result = True

    return result
