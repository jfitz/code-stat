from codestat_token import Token
from token_builders import TokenBuilder


# token reader for identifier
class DbaseIdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isalpha() or c in [':', '_']

    if len(candidate) > 0:
      result = c.isalpha() or c.isdigit() or c in [':', '_']

    return result


# token reader for identifier
class DbaseFilenameTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'filename')]


  def accept(self, candidate, c):
    result = c.isalpha() or c.isdigit() or c == '.'

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    num_dots = self.text.count('.')

    if num_dots > 1:
      return 0

    return len(self.text)


# accept characters to match item in list
class DbaseEndifTokenBuilder(TokenBuilder):
  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    score = 0

    if self.text.lower().startswith('endif'):
      score = len(self.text)

    return score


# accept characters to match item in list
class KeywordCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, legals, case_sensitive):
    if case_sensitive:
      self.legals = legals
    else:
      self.legals = list(map(str.lower, legals))

    self.abbrevs = {}
    for legal in self.legals:
      for i in range(len(legal)):
        self.abbrevs[legal[:i+1]] = 1

    self.case_sensitive = case_sensitive
    self.token1 = None
    self.token2 = ''


  def attempt(self, text):
    self.text = None
    self.token1 = None
    self.token2 = ''
    best_candidate = None
    candidate = ''
    i = 0

    while i < len(text):
      c = text[i]

      # match a keyword
      if not self.accept(candidate, c):
        break

      candidate += c
      i += 1

      if self.case_sensitive:
        if candidate in self.legals:
          best_candidate = candidate
      else:
        if candidate.lower() in self.legals:
          best_candidate = candidate

    if best_candidate is not None:
      self.token1 = best_candidate

      # absorb all characters until newline (or end of text)
      while i < len(text):
        c = text[i]

        if c in ['\n', '\r']:
          break

        self.token2 += c
        i += 1
  
    if self.token1 is not None:
      self.text = self.token1 + self.token2


  def get_tokens(self):
    if self.token1 is None:
      return None

    if self.token2 is None:
      token1 = Token(self.token1, 'keyword')
      tokens = [token1]
    else:
      token1 = Token(self.token1, 'keyword')
      token2 = Token(self.token2, 'comment')
      tokens = [token1, token2]
    return tokens


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    token = candidate + c
    result = False

    if self.case_sensitive:
      result = token in self.abbrevs
    else:
      result = token.lower() in self.abbrevs

    return result


  def get_score(self, line_printable_tokens):
    if self.token1 is None:
      return 0

    score = 0
    if self.case_sensitive:
      if self.token1 in self.legals:
        score = len(self.text)
    else:
      if self.token1.lower() in self.legals:
        score = len(self.text)

    return score
