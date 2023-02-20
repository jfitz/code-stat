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


# count groups for regex operators
def count_regex_group(bch, ech, candidate):
  group_count = 0
  level = 0
  escaped = False
  for ch in candidate:
    if ch == bch and not escaped:
      level += 1

    if ch == ech and not escaped:
      level -= 1
      if level == 0:
        group_count += 1
    
    if level < 0:
      level = 0

    if ch == '\\':
      escaped = not escaped
    else:
      escaped = False

  return group_count


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
      return c in 'qrwx{('

    if candidate in ['qq', 'qr', 'qw', 'qx']:
      return c in '{('

    bch = None
    ech = None

    if len(candidate) > 1:
      if candidate[1] == '{':
        bch = '{'
        ech = '}'
      if candidate[1] == '(':
        bch = '('
        ech = ')'

    if len(candidate) > 2 and bch is None:
      if candidate[2] == '{':
        bch = '{'
        ech = '}'
      if candidate[2] == '(':
        bch = '('
        ech = ')'

    return PerlQStringTokenBuilder.count_level(candidate, bch, ech) > 0


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have at least three chars
    if len(self.text) < 3:
      return 0

    # must contain { and } or ( and )
    bch = None
    ech = None

    if len(self.text) > 1:
      if self.text[1] == '{':
        bch = '{'
        ech = '}'
      if self.text[1] == '(':
        bch = '('
        ech = ')'

    if len(self.text) > 2 and bch is None:
      if self.text[2] == '{':
        bch = '{'
        ech = '}'
      if self.text[2] == '(':
        bch = '('
        ech = ')'

    if self.text[-1] != ech:
      return 0

    # must have balanced braces
    if PerlQStringTokenBuilder.count_level(self.text, bch, ech) > 0:
      return 0

    return len(self.text)


  @staticmethod
  def count_level(candidate, bch, ech):
    level = 0
    escaped = False

    for ch in candidate:
      if ch == bch and not escaped:
        level += 1

      if ch == ech and not escaped and level > 0:
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
      return c in '/{(!#'

    if len(candidate) == 2:
      return True

    bch = candidate[1]
    ech = bch

    if bch == '{':
      ech = '}'
    if bch == '(':
      ech = ')'

    if ech == bch:
      count = count_not_escaped(bch, candidate)
      if count < 2:
        return True

    else:
      count = count_regex_group(bch, ech, candidate)
      if count < 1:
        return True


    return c.isalpha()


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    bch = self.text[1]
    ech = self.text[1]

    if bch == '{':
      bch = '\\{'
      ech = '\\}'
    if bch == '(':
      bch = '\\('
      ech = '\\)'

    regex = r'\Am' + bch + '.+' + ech + r'[a-z]*\Z'
    pattern = re.compile(regex)

    if not pattern.match(self.text):
      return 0

    return len(self.text)


# token reader for regular expression s///
class SRegexTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
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
      return c in '/{(!#'

    if len(candidate) == 2:
      return True

    bch = candidate[1]
    ech = bch

    if bch == '{':
      ech = '}'
    if bch == '(':
      ech = ')'

    if ech == bch:
      count = count_not_escaped(bch, candidate)
      if count < 3:
        return True
    else:
      count = count_regex_group(bch, ech, candidate)
      if count < 2:
        return True

    return c.isalpha()


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    bch = self.text[1]
    ech = self.text[1]

    if bch == '{':
      bch = '\\{'
      ech = '\\}'
    if bch == '(':
      bch = '\\('
      ech = '\\)'

    regex = r'\As' + bch + '.+' + ech + r'[a-z]*\Z'
    pattern = re.compile(regex)

    if not pattern.match(self.text):
      return 0

    return len(self.text)


# token reader for regular expression s///
class YRegexTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
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
      return c in '/{(!#'

    if len(candidate) == 2:
      return True

    bch = candidate[1]
    ech = bch

    if bch == '{':
      ech = '}'
    if bch == '(':
      ech = ')'

    if ech == bch:
      count = count_not_escaped(bch, candidate)
      if count < 3:
        return True
    else:
      count = count_regex_group(bch, ech, candidate)
      if count < 2:
        return True

    return c.isalpha()


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    bch = self.text[1]
    ech = self.text[1]

    if bch == '{':
      bch = '\\{'
      ech = '\\}'
    if bch == '(':
      bch = '\\('
      ech = '\\)'

    regex = r'\Ay' + bch + '.+' + ech + r'[a-z]*\Z'
    pattern = re.compile(regex)

    if not pattern.match(self.text):
      return 0

    return len(self.text)


# token reader for regular expression s///
class TrRegexTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
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
      return c in '/{(!#'

    if len(candidate) == 3:
      return True

    bch = candidate[2]
    ech = bch

    if bch == '{':
      ech = '}'
    if bch == '(':
      ech = ')'

    if ech == bch:
      count = count_not_escaped(bch, candidate)
      if count < 3:
        return True
    else:
      count = count_regex_group(bch, ech, candidate)
      if count < 2:
        return True

    return c.isalpha()


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 3:
      return 0

    bch = self.text[2]
    ech = self.text[2]

    if bch == '{':
      bch = '\\{'
      ech = '\\}'
    if bch == '(':
      bch = '\\('
      ech = '\\)'

    regex = r'\Atr' + bch + '.+' + ech + r'[a-z]*\Z'
    pattern = re.compile(regex)

    if not pattern.match(self.text):
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


# token reader for Perl prototypes
class PerlSigilBraceTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    if len(self.text) != 2:
      return None

    return [
      Token(self.text[0], 'identifier', True),
      Token(self.text[1], 'group', False)
    ]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c in '$@%'

    return len(candidate) == 1 and c == '{'


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) != 2:
      return 0

    return len(self.text)


# token reader for Perl prototypes
class PerlEndTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    tokens = []

    lines = self.text.split('\n')

    for line in lines:
      if line.endswith('\r'):
        tokens.append(Token(line[:-1], 'comment', False))
        tokens.append(Token('\r\n', 'newline', False))
      else:
        tokens.append(Token(line, 'comment', False))
        tokens.append(Token('\n', 'newline', False))

    return tokens


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == '_'

    if len(candidate) == 1:
      return c == '_'

    if len(candidate) == 2:
      return c == 'E'

    if len(candidate) == 3:
      return c == 'N'

    if len(candidate) == 4:
      return c == 'D'

    if len(candidate) == 5:
      return c == '_'

    if len(candidate) == 6:
      return c == '_'

    return True


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.text.startswith('__END__'):
      return 0

    return len(self.text)
