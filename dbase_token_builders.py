import re

from codestat_token import Token
from token_builders import TokenBuilder


# token reader for deleted record function
class DbaseSpecialFunctionTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    return 'Escape ?Z'


  def __init__(self, chars, previous):
    self.chars = chars
    self.previous = previous
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'function')]


  def accept(self, candidate, c):
    result = len(candidate) == 0 and c in self.chars

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(line_printable_tokens) == 0:
      return 0

    if line_printable_tokens[-1].text.lower() not in self.previous:
      return 0

    return len(self.text)


# token reader for identifier
class DbaseIdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, extra_chars):
    self.extra_chars = extra_chars
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isalpha()

    if len(candidate) > 0:
      result = c.isalpha() or c.isdigit() or c in self.extra_chars

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


# token reader for text literal (string)
class BracketedStringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string')]


  def accept(self, candidate, c):
    # newline breaks a string
    if c in ['\n', '\r']:
      return False

    if len(candidate) == 0:
      return c == '['

    if len(candidate) == 1:
      return True

    return candidate[-1] != ']'


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[-1] != ']':
      return 0

    return len(self.text)


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


# accept characters to match item in list
class KeywordComment2TokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, legals, case_sensitive):
    if case_sensitive:
      self.legals = legals
    else:
      self.legals = list(map(str.lower, legals))

    self.case_sensitive = case_sensitive

    if case_sensitive:
      self.regex = re.compile('(DO)( +)(CASE)(.*)')
    else:
      self.regex = re.compile('(DO)( +)(CASE)(.*)', re.IGNORECASE)

    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    m = re.match(self.regex, self.text)
    if m is not None:
      g = m.groups()
      if len(g) != 4:
        return None

    token1 = Token(g[0], 'keyword')
    token2 = Token(g[1], 'whitespace')
    token3 = Token(g[2], 'keyword')
    token4 = Token(g[3], 'comment')

    tokens = [token1, token2, token3, token4]

    return tokens


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if self.case_sensitive:
      if re.match(self.regex, candidate) is not None:
        return True
    else:
      if re.match(self.regex, candidate.lower()) is not None:
        return True

    if len(candidate) == 0:
      return c.lower() == 'd'

    if len(candidate) == 1:
      return c.lower() == 'o'

    if candidate[-1].lower() == 'o':
      return c.lower() == ' '

    if candidate[-1].lower() == ' ':
      return c.lower() in [' ', 'c']

    if candidate[-1].lower() == 'c':
      return c.lower() == 'a'

    if candidate[-1].lower() == 'a':
      return c.lower() == 's'

    if candidate[-1].lower() == 's':
      return c.lower() == 'e'

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    score = 0
    if self.case_sensitive:
      if re.match('do +case.+', self.text) is not None:
        score = len(self.text)
    else:
      if re.match('do +case.+', self.text.lower()) is not None:
        score = len(self.text)

    return score


# token reader for identifier
class TextBlockTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, start_keyword, end_keyword):
    self.text = None
    self.start_keyword = start_keyword.lower()
    self.end_keyword = end_keyword.lower()

  def get_tokens(self):
    if self.text is None:
      return None

    # split the text into 'TEXT', content, and 'ENDTEXT tokens
    len_start = len(self.start_keyword)
    len_end = len(self.end_keyword)

    starter_token = Token(self.text[:len_start], 'keyword')
    ender_token = Token(self.text[-len_end:], 'keyword')
    content = Token(self.text[len_start:-len_end], 'string')

    return [
      starter_token,
      content,
      ender_token
    ]

  def accept(self, candidate, c):
    result = False
 
    if len(candidate) < len(self.start_keyword):
      result = self.start_keyword.startswith(candidate.lower())
    else:
      if candidate.lower().startswith(self.start_keyword):
        result = True

        # if the text ends with the end keyword
        # stop accepting characters
        if candidate.lower().endswith(self.end_keyword):
          result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.text.lower().startswith(self.start_keyword):
      return 0

    if not self.text.lower().endswith(self.start_keyword):
      return 0

    return len(self.text)
