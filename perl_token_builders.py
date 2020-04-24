import re

from codestat_token import Token
from token_builders import TokenBuilder

# count characters in string
def count_not_escaped(c, candidate):
  count = 0
  escaped = False
  for ch in candidate:
    if ch == c and not escaped:
      count += 1
    if ch == '\\':
      escaped = not escaped
    else:
      escaped = False
  
  return count


# token reader for Perl variable
class PerlIdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.leads = '$@%#'
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier', True)]


  def accept(self, candidate, c):
    if c in [' ', '\t', '\n', '\r']:
      return False

    if len(candidate) == 0:
      return c in self.leads

    if candidate[-1] in self.leads:
      return c in self.leads or c.isalnum() or c == '_'

    return c.isalnum() or c == '_'


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have at least two chars
    if len(self.text) < 2:
      return 0

    # cannot end with special prefix char
    if self.text[-1] in self.leads:
      return 0

    return len(self.text)


# token reader for Perl dollar-caret variable
class PerlDollarCaretIdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier', True)]


  def accept(self, candidate, c):
    if c in [' ', '\t', '\n', '\r']:
      return False

    if len(candidate) == 0:
      return c == '$'

    if len(candidate) == 1:
      return c == '^'

    return c.isalnum() or c == '_'


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have at least three chars
    if len(self.text) < 3:
      return 0

    return len(self.text)


# token reader for q{} and related strings
class PerlQStringTokenBuilder(TokenBuilder):
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
    if len(candidate) == 0:
      return c == 'q'

    if len(candidate) == 1:
      return c in 'qrwx{'

    if candidate in ['qq', 'qr', 'qw', 'qx']:
      return c == '{'

    return PerlQStringTokenBuilder.count_level(candidate) > 0


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have at least three chars
    if len(self.text) < 3:
      return 0

    # must contain { and }
    if self.text[-1] != '}':
      return 0

    # must have balanced braces
    if PerlQStringTokenBuilder.count_level(self.text) > 0:
      return 0

    return len(self.text)


  @staticmethod
  def count_level(candidate):
    level = 0
    escaped = False

    for ch in candidate:
      if ch == '{' and not escaped:
        level += 1

      if ch == '}' and not escaped and level > 0:
        level -= 1

      if ch == '\\':
        escaped = not escaped
      else:
        escaped = False
    
    return level


# token reader for regular expression m//
class MRegexTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.pattern = re.compile(r'\Am/.+/[a-z]*\Z')
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'regex', True)]


  def accept(self, candidate, c):
    if c in ['\r', '\n']:
      return False

    if len(candidate) == 0:
      return c == 'm'

    if len(candidate) == 1:
      return c == '/'

    if len(candidate) == 2:
      return True

    slash_count = count_not_escaped('/', candidate)

    if slash_count < 2:
      return True

    return c.isalpha()


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.pattern.match(self.text):
      return 0

    return len(self.text)


# token reader for regular expression s///
class SRegexTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.pattern = re.compile(r'\As/.+/[a-z]*\Z')
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'regex', True)]


  def accept(self, candidate, c):
    if c in ['\r', '\n']:
      return False

    if len(candidate) == 0:
      return c == 's'

    if len(candidate) == 1:
      return c == '/'

    if len(candidate) == 2:
      return True

    slash_count = count_not_escaped('/', candidate)

    if slash_count < 3:
      return True

    return c.isalpha()


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.pattern.match(self.text):
      return 0

    return len(self.text)


# token reader for regular expression s///
class YRegexTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.pattern = re.compile(r'\Ay/.+/[a-z]*\Z')
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'regex', True)]


  def accept(self, candidate, c):
    if c in ['\r', '\n']:
      return False

    if len(candidate) == 0:
      return c == 'y'

    if len(candidate) == 1:
      return c == '/'

    if len(candidate) == 2:
      return True

    slash_count = count_not_escaped('/', candidate)

    if slash_count < 3:
      return True

    return c.isalpha()


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.pattern.match(self.text):
      return 0

    return len(self.text)


# token reader for regular expression s///
class TrRegexTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.pattern = re.compile(r'\Atr/.+/[a-z]*\Z')
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'regex', True)]


  def accept(self, candidate, c):
    if c in ['\r', '\n']:
      return False

    if len(candidate) == 0:
      return c == 't'

    if len(candidate) == 1:
      return c == 'r'

    if len(candidate) == 2:
      return c == '/'

    if len(candidate) == 3:
      return True

    slash_count = count_not_escaped('/', candidate)

    if slash_count < 3:
      return True

    return c.isalpha()


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.pattern.match(self.text):
      return 0

    return len(self.text)


# token reader for Perl prototypes
class PerlPrototypeTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'prototype', False)]


  def accept(self, candidate, c):
    if c in [' ', '\t', '\n', '\r']:
      return False

    return c in '$@%\\'


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have at least three previous tokens
    if len(line_printable_tokens) < 3:
      return 0

    if line_printable_tokens[-3].text != 'sub':
      return 0

    if line_printable_tokens[-2].group != 'identifier':
      return 0

    if line_printable_tokens[-1].text != '(':
      return 0

    return len(self.text)
