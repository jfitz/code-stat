import re

from codestat_token import Token
from token_builders import (
  TokenBuilder,
  IdentifierTokenBuilder,
  BlockTokenBuilder
)

# token reader for text literal (string)
class DbaseStringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, quotes):
    self.quotes = quotes
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string', True)]


  def accept(self, candidate, c):
    # newline breaks a string
    if len(candidate) == 0:
      return c in self.quotes

    if c == '\r':
      return candidate.strip()[-1] == ';'

    if c == '\n':
      return candidate.strip()[-1] == ';' or candidate[-1] == '\r'

    if len(candidate) == 1:
      return True

    return candidate[-1] != candidate[0]


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[-1] != self.text[0]:
      return 0

    return len(self.text)


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

    return [Token(self.text, 'filename', True)]


  def accept(self, candidate, c):
    return c.isalpha() or c.isdigit() or c in '.-'


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # a file name can have at most one dot (for the extension)
    num_dots = self.text.count('.')

    if num_dots > 1:
      return 0

    # must follow DO, [SET ... TO, USE, INDEX, LOAD, CALL
    if len(line_printable_tokens) == 0:
      return 0

    # file names always follow these keywords
    predecessors = ['call', 'do', 'from', 'index', 'load', 'to', 'use']
    if line_printable_tokens[-1].text.lower() not in predecessors:
      return 0

    # TO is a special case; line must start with SET (not STORE)
    if line_printable_tokens[-1].text.lower() == 'to' and \
      line_printable_tokens[0].text.lower() not in ['set', 'copy']:
      return 0
  
    # some keywords look like file names but are not
    if self.text.lower() in ['screen', 'print', 'file']:
      return 0

    return len(self.text)


# token reader for LIKE wildcards
class WildCardIdentifierTokenBuilder(IdentifierTokenBuilder):

  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(line_printable_tokens) == 0:
      return 0

    if line_printable_tokens[-1].text.lower() != 'like':
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

    return [Token(self.text, 'string', True)]


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

    if len(line_printable_tokens) > 0:
      if line_printable_tokens[-1].group == 'identifier':
        return 0

    return len(self.text)


# keywords that follow the 'SET' keyword
class SetCaseInsensitiveListTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, legals, group, is_operand):
    self.legals = list(map(str.lower, legals))

    self.abbrevs = {}
    for legal in self.legals:
      for i in range(len(legal)):
        self.abbrevs[legal[:i+1]] = 1

    self.group = group
    self.is_operand = is_operand
    self.text = ''


  def attempt(self, text, start):
    self.text = None
    best_candidate = None
    candidate = ''
    i = start

    while i < len(text):
      c = text[i]

      if not self.accept(candidate, c):
        break

      candidate += c
      i += 1

      if candidate.lower() in self.legals:
        best_candidate = candidate

    self.text = best_candidate


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, self.group, self.is_operand)]


  def accept(self, candidate, c):
    token = candidate + c
    return token.lower() in self.abbrevs


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    set_token = Token('set', 'keyword', False)
    if not set_token in line_printable_tokens:
      return 0

    if self.text.lower() in self.legals:
      return len(self.text)

    return 0


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


  def attempt(self, text, start):
    self.text = None
    self.token1 = None
    self.token2 = ''
    best_candidate = None
    candidate = ''
    i = start

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
      token1 = Token(self.token1, 'keyword', False)
      tokens = [token1]
    else:
      token1 = Token(self.token1, 'keyword', False)
      token2 = Token(self.token2, 'comment', False)
      tokens = [token1, token2]
    return tokens


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    token = candidate + c

    if self.case_sensitive:
      return token in self.abbrevs

    return token.lower() in self.abbrevs


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
class KeywordDoCaseCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self,  case_sensitive):
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

    token1 = Token(g[0], 'keyword', False)
    token2 = Token(g[1], 'whitespace', False)
    token3 = Token(g[2], 'keyword', False)
    token4 = Token(g[3], 'comment', False)

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
class TextBlockTokenBuilder(BlockTokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, prefix, suffix):
    super().__init__(prefix, suffix, 'string')


  def get_tokens(self):
    if self.text is None:
      return None

    # split the text into 'TEXT', content, and 'ENDTEXT' tokens
    len_start = len(self.prefix)
    len_end = len(self.suffix)

    starter_token = Token(self.text[:len_start], 'keyword', False)
    ender_token = Token(self.text[-len_end:], 'keyword', False)
    content = Token(self.text[len_start:-len_end], 'string', True)

    return [
      starter_token,
      content,
      ender_token
    ]
