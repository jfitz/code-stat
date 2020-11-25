import re
import itertools

from codestat_token import Token

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


# generic TokenBuilder (to hold common functions)
class TokenBuilder:
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def attempt(self, text, start):
    self.text = None
    candidate = ''
    i = start

    while i < len(text):
      c = text[i]

      if not self.accept(candidate, c):
        break

      candidate += c
      i += 1

    if len(candidate) > 0:
      self.text = candidate


  def accept(self, candidate, c):
    return False


  def get_count(self):
    if self.text is None:
      return 0

    return len(self.text)


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    return len(self.text)


# refuse all characters
class NullTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def attempt(self, text, start):
    return False


  def get_tokens(self):
    return None


# accept any character (but only one)
class InvalidTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def attempt(self, text, start):
    self.text = None

    if len(text) > start:
      self.text = text[start]
      return True

    return False


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'invalid', False)]


# token reader for whitespace
class WhitespaceTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'whitespace', False)]


  def accept(self, candidate, c):
    return c.isspace() and c != '\n' and c != '\r'


# token reader for newline
class NewlineTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'newline', False)]


  def accept(self, candidate, c):
    if c not in ['\n', '\r']:
      return False

    if candidate == '':
      return c == '\r' or c == '\n'

    if candidate == '\r':
      return c == '\n'

    return False


# token reader for text literal (string)
class StringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, quotes, newline_limit=0):
    self.quotes = quotes
    self.newline_limit = newline_limit
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string', True)]


  def accept(self, candidate, c):
    # newline breaks a string
    if c in ['\n', '\r']:
      count = candidate.count('\n') + candidate.count('\r')
      if count >= self.newline_limit:
        return False

    if len(candidate) == 0:
      return c in self.quotes

    if len(candidate) == 1:
      return True

    # no quote stuffing, stop on second quote
    # assume no escaped quotes are allowed
    return candidate[-1] != candidate[0]


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[-1] != self.text[0]:
      return 0

    return len(self.text)


# token reader for text literal (string)
class EscapedStringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, quotes, newline_limit=0):
    self.quotes = quotes
    self.newline_limit = newline_limit
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string', True)]


  def accept(self, candidate, c):
    # newline breaks a string
    if c in ['\n', '\r']:
      count = candidate.count('\n') + candidate.count('\r')
      if count >= self.newline_limit:
        return False

    if len(candidate) == 0:
      return c in self.quotes

    if len(candidate) == 1:
      return True

    # no quote stuffing, stop on second quote
    # assume escaped quotes are allowed
    quote_count = count_not_escaped(candidate[0], candidate)

    return quote_count < 2


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[-1] != self.text[0]:
      return 0

    return len(self.text)


# token reader for text literal (string)
class StuffedQuoteStringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, quotes, allow_unterm):
    self.quotes = quotes
    self.allow_unterm = allow_unterm
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string', True)]


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if len(candidate) == 0:
      return c in self.quotes

    if len(candidate) == 1:
      return True

    # a quote character is accepted, even after closing quote
    if c == candidate[0]:
      return True

    # if number of quotes is even, string is closed
    count = candidate.count(candidate[0])

    return count % 2 == 1


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if not self.allow_unterm and self.text[-1] != self.text[0]:
      return 0

    return len(self.text)


# token reader for prefixed text literal (string)
class PrefixedStringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, prefix, case_sensitive, quotes):
    if case_sensitive:
      self.prefix = prefix
    else:
      self.prefix = prefix.lower()
    self.case_sensitive = case_sensitive
    self.quotes = quotes
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string', True)]


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if len(candidate) < len(self.prefix):
      if self.case_sensitive:
        return c == self.prefix[len(candidate)]
      else:
        return c.lower() == self.prefix[len(candidate)]
    
    if len(candidate) == len(self.prefix):
      return c in self.quotes

    if len(candidate) == len(self.prefix) + 1:
      return True

    # no quote stuffing, stop on second quote
    # assume escaped quotes are allowed
    quote_count = count_not_escaped(candidate[len(self.prefix)], candidate)

    return quote_count < 2


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < len(self.prefix) + 2:
      return 0

    if self.text[-1] != self.text[len(self.prefix)]:
      return 0

    return len(self.text)


# token reader for prefixed text literal (string)
class PrefixedRawStringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, prefix, case_sensitive, quotes):
    if case_sensitive:
      self.prefix = prefix
    else:
      self.prefix = prefix.lower()
    self.case_sensitive = case_sensitive
    self.quotes = quotes
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string', True)]


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if len(candidate) < len(self.prefix):
      if self.case_sensitive:
        return c == self.prefix[len(candidate)]
      else:
        return c.lower() == self.prefix[len(candidate)]
    
    if len(candidate) == len(self.prefix):
      return c in self.quotes

    if len(candidate) == len(self.prefix) + 1:
      return True

    # no quote stuffing, stop on second quote
    return candidate[-1] != candidate[len(self.prefix)]


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < len(self.prefix) + 2:
      return 0

    if self.text[-1] != self.text[len(self.prefix)]:
      return 0

    return len(self.text)


# token reader for text literal (string)
class SuffixedStringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, quotes, suffixes, allow_newline):
    self.quotes = quotes
    self.suffixes = suffixes
    self.allow_newline = allow_newline
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string', True)]


  def accept(self, candidate, c):
    # newline breaks a string
    if c in ['\n', '\r'] and not self.allow_newline:
      return False

    if len(candidate) == 0:
      return c in self.quotes

    if len(candidate) == 1:
      return True

    # no quote stuffing, stop on second quote
    # assume escaped quotes are allowed
    quote_count = count_not_escaped(candidate[0], candidate)

    if quote_count < 2:
      return True

    if candidate[-1] in self.quotes:
      return c in self.suffixes

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 3:
      return 0

    if self.text[-2] != self.text[0]:
      return 0

    if self.text[-1] not in self.suffixes:
      return 0

    return len(self.text)


# token reader for integer
class IntegerTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, extra_char):
    self.extra_char = extra_char
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number', True)]


  def accept(self, candidate, c):
    if c.isdigit():
      return True
    
    if len(candidate) > 0 and candidate[-1].isdigit():
      return self.extra_char is not None and c == self.extra_char

    return False


# token reader for integer with exponent
class IntegerExponentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, extra_char):
    self.extra_char = extra_char
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isdigit()
    
    if self.extra_char is not None and c == self.extra_char:
      return candidate[-1].isdigit()

    return c.isdigit() or (
      c.lower() == 'e' and 'e' not in candidate.lower()
    ) or (
      c in '+-' and candidate[-1].lower() == 'e'
    )


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have digit, 'E', and digit
    if len(self.text) < 3:
      return 0

    if 'e' not in self.text.lower():
      return 0

    if not self.text[-1].isdigit():
      return 0

    return len(self.text)


# token reader for number
class SuffixedIntegerTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, suffixes, case_sensitive, extra_chars):
    if case_sensitive:
      self.suffixes = suffixes
    else:
      self.suffixes = list(map(str.lower, suffixes))

    self.abbrevs = {}
    for suffix in self.suffixes:
      for i in range(len(suffix)):
        self.abbrevs[suffix[:i+1]] = 1

    self.case_sensitive = case_sensitive
    self.extra_chars = extra_chars

    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number', True)]


  def digit_or_underscore(self, c):
    if self.extra_chars is not None and c in self.extra_chars:
      return True

    return c.isdigit()


  def accept(self, candidate, c):
    groups = ["".join(x) for _, x in itertools.groupby(candidate, key=self.digit_or_underscore)]

    if len(groups) == 0:
      return c.isdigit()

    if len(groups) == 1:
      if self.extra_chars is not None:
        if self.case_sensitive:
          return c.isdigit() or c in self.extra_chars or c in self.abbrevs
        else:
          return c.isdigit() or c in self.extra_chars or c.lower() in self.abbrevs
      else:
        if self.case_sensitive:
          return c.isdigit() or c in self.abbrevs
        else:
          return c.isdigit() or c.lower() in self.abbrevs

    if len(groups) == 2:
      suffix = groups[1] + c
      if self.case_sensitive:
        return suffix in self.abbrevs
      else:
        return suffix.lower() in self.abbrevs

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    groups = ["".join(x) for _, x in itertools.groupby(self.text, key=self.digit_or_underscore)]

    if len(groups) != 2:
      return 0

    if self.case_sensitive:
      if groups[1] not in self.suffixes:
        return 0
    else:
      if groups[1].lower() not in self.suffixes:
        return 0

    return len(self.text)


# token reader for real (no exponent)
class RealTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, require_before, require_after, extra_char):
    self.require_before = require_before
    self.require_after = require_after
    self.extra_char = extra_char
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number', True)]


  def accept(self, candidate, c):
    if c.isdigit():
      return True
    
    if c == '.' and '.' not in candidate:
      return True

    if len(candidate) > 0 and candidate[-1].isdigit():
      return self.extra_char is not None and c == self.extra_char

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if '.' not in self.text:
      return 0

    if self.require_before and not self.text[0].isdigit():
      return 0

    point_position = self.text.find('.')

    if point_position == -1 or point_position == len(self.text) - 1:
      return 0

    if self.require_after and not self.text[point_position + 1].isdigit():
      return 0

    if self.extra_char is not None:
      if not self.text[-1].isdigit() and self.text[-1] != self.extra_char:
        return 0
    else:
      if not self.text[-1].isdigit():
        return 0

    return len(self.text)


# token reader for real (no exponent)
class SuffixedRealTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, require_before, require_after, suffixes, case_sensitive, extra_char):
    self.require_before = require_before
    self.require_after = require_after

    if case_sensitive:
      self.suffixes = suffixes
    else:
      self.suffixes = list(map(str.lower, suffixes))

    self.case_sensitive = case_sensitive
    self.extra_char = extra_char
    self.digits = '0123456789.'
    if extra_char is not None:
      self.digits += extra_char

    self.abbrevs = {}
    for suffix in self.suffixes:
      for i in range(len(suffix)):
        self.abbrevs[suffix[:i+1]] = 1

    self.regex = re.compile(r'[A-Za-z]+$')

    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number', True)]


  def accept(self, candidate, c):
    if c.isdigit():
      if len(candidate) == 0:
        return True

      return candidate[-1] in self.digits

    if c == '.' and '.' not in candidate:
      if len(candidate) > 0:
        return candidate[-1].isdigit()

      return True

    if self.extra_char is not None and c == self.extra_char:
      return len(candidate) > 0 and candidate[-1].isdigit()

    # if any digits before or after decimal, check suffix
    if any(str.isdigit(ch) for ch in candidate):
      if c not in self.digits and len(candidate) > 0:
        # get current (possibly partial) suffix
        m = self.regex.search(candidate)

        # append c to suffix in process
        if m is None:
          trailings = c
        else:
          trailings = m.group(0) + c

        # check proposed suffix is in list of abbrevs
        if self.case_sensitive:
          return trailings in self.abbrevs

        return trailings.lower() in self.abbrevs

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have decimal
    if '.' not in self.text:
      return 0

    # must have at least one digit
    if not any(str.isdigit(ch) for ch in self.text):
      return 0

    if self.require_before and not self.text[0].isdigit():
      return 0

    point_position = self.text.find('.')

    if self.require_after:
      if point_position == len(self.text) - 1:
        return 0

      if not self.text[point_position + 1].isdigit():
        return 0

    # check suffix is known
    m = self.regex.search(self.text)
    if m is not None:
      trailings = m.group(0)
      if self.case_sensitive:
        if trailings not in self.suffixes:
          return 0
      else:
        if trailings.lower() not in self.suffixes:
          return 0

    return len(self.text)


# token reader for real with exponent
class RealExponentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, require_before, require_after, letter, extra_char):
    self.require_before = require_before
    self.require_after = require_after
    self.letter = letter.lower()
    self.extra_char = extra_char
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number', True)]


  def accept(self, candidate, c):
    if c.isdigit():
      return True

    if c == '.':
      return '.' not in candidate and self.letter not in candidate.lower()

    if c == self.extra_char:
      return len(candidate) > 0 and candidate[-1].isdigit()

    if c.lower() == self.letter:
      return len(candidate) > 0 and self.letter not in candidate.lower()

    if c in ['+', '-']:
      return len(candidate) > 0 and candidate[-1].lower() == self.letter

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # must have digit, decimal, 'E', and digit (or decimal, digit, 'E', digit)
    if len(self.text) < 4:
      return 0

    if '.' not in self.text:
      return 0

    if not self.letter in self.text.lower():
      return 0

    if self.text[-1].lower() == self.letter:
      return 0

    if self.extra_char is not None:
      if not self.text[-1].isdigit() and self.text[-1] != self.extra_char:
        return 0
    else:
      if not self.text[-1].isdigit():
        return 0

    if self.require_before and not self.text[0].isdigit():
      return 0

    point_position = self.text.find('.')

    if point_position == -1 or point_position == len(self.text) - 1:
      return 0

    if self.require_after and not self.text[point_position + 1].isdigit():
      return 0

    return len(self.text)


# token reader for identifier
class IdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, lead_extras, extras):
    self.lead_extras = lead_extras
    self.extras = extras
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isalpha() or c in self.lead_extras

    return c.isalpha() or c.isdigit() or c in self.extras


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # find positions of first alpha and first digit
    pos_a = -1
    pos_n = -1
    i = 0
    for c in self.text:
      if pos_a == -1 and (c.isalpha() or c == '_'):
        pos_a = i

      if pos_n == -1 and c.isdigit():
        pos_n = i

      i += 1

    # must have at least one alpha
    if pos_a == -1:
      return 0

    # first alphanum must be alpha
    if pos_n > -1 and pos_n < pos_a:
      return 0

    return len(self.text)


# token reader for identifier
class PrefixedIdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, prefix, group, is_operand):
    self.prefix = prefix
    self.group = group
    self.is_operand = is_operand
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, self.group, self.is_operand)]


  def accept(self, candidate, c):
    if len(candidate) < len(self.prefix):
      return self.prefix.startswith(candidate + c)

    return c.isalpha() or c.isdigit() or c == '_'


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if not self.text.startswith(self.prefix):
      return 0

    return len(self.text)


# token reader for identifier
class SuffixedIdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, lead_extras, extras, suffixes):
    self.lead_extras = lead_extras
    self.suffixes = suffixes
    self.extras = extras
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isalpha() or c in self.lead_extras

    if len(candidate) > 1 and candidate[-1] in self.suffixes:
      return False

    return c.isalpha() or c.isdigit() or c in self.extras or c in self.suffixes


class SingleCharacterTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, legals, group, is_operand):
    self.legals = legals
    self.group = group
    self.is_operand = is_operand
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, self.group, self.is_operand)]


  def accept(self, candidate, c):
    return len(candidate) == 0 and c in self.legals


class KeywordTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, keyword, group):
    self.keyword = keyword
    self.group = group
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, self.group, False)]


  def accept(self, candidate, c):
    return len(candidate) < len(self.keyword) and \
       c == self.keyword[len(candidate)]


# accept characters to match item in list
class CaseInsensitiveListTokenBuilder(TokenBuilder):
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

    if self.text.lower() in self.legals:
      return len(self.text)

    return 0


# accept characters to match item in list
class CaseSensitiveListTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, legals, group, is_operand):
    self.legals = legals

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

      if candidate in self.legals:
        best_candidate = candidate

    self.text = best_candidate


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, self.group, self.is_operand)]


  def accept(self, candidate, c):
    token = candidate + c
    return token in self.abbrevs


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text in self.legals:
      return len(self.text)

    return 0


# token reader for integer
class PrefixedIntegerTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, prefix, case_sensitive, allowed_chars):
    if case_sensitive:
      self.prefix = prefix
    else:
      self.prefix = prefix.lower()
    self.case_sensitive = case_sensitive
    self.allowed_chars = allowed_chars
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number', True)]


  def accept(self, candidate, c):
    if len(candidate) < len(self.prefix):
      if self.case_sensitive:
        return c == self.prefix[len(candidate)]
      else:
        return c.lower() == self.prefix[len(candidate)]
    
    return c in self.allowed_chars


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) <= len(self.prefix):
      return 0

    return len(self.text)


# token reader for ! comment
class LeadToEndOfLineTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, lead, case_sensitive, group):
    if case_sensitive:
      self.lead = lead
    else:
      self.lead = lead.lower()
    self.case_sensitive = case_sensitive
    self.group = group
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    if self.case_sensitive:
      if self.text.startswith(self.lead):
        return [Token(self.text, self.group, False)]
    else:
      if self.text.lower().startswith(self.lead):
        return [Token(self.text, self.group, False)]

    return None


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if len(candidate) < len(self.lead):
      if self.case_sensitive:
        return c == self.lead[len(candidate)]
      else:
        return c.lower() == self.lead[len(candidate)]

    if self.case_sensitive:
      return candidate.startswith(self.lead)

    return candidate.lower().startswith(self.lead)


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < len(self.lead):
      return 0

    return len(self.text)


# token reader for #! comment
class SheBangTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    if self.text == '':
      return None

    return [Token(self.text, 'comment', False)]


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if candidate == '':
      return c == '#'

    if candidate == '#':
      return c == '!'

    return True


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    return len(self.text)


# token reader for triple quote string
class TripleQuoteStringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, quotes):
    # convert single quotes to triples
    self.quote_set = []
    for quote in quotes:
      triple = quote * 3
      self.quote_set.append(triple)
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string', True)]


  def accept(self, candidate, c):
    if len(candidate) > 5:
      return candidate[-3:] != candidate[:3]

    if len(candidate) > 2:
      return candidate[:3] in self.quote_set

    if len(candidate) > 0:
      return c == candidate[0]

    return c in '"\''


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 6:
      return 0

    if self.text[:3] != self.text[-3:]:
      return 0
  
    return len(self.text)


# token reader for regular expression
class RegexTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.pattern = re.compile(r'\A/.+/[a-z]*\Z')
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'regex', True)]


  def accept(self, candidate, c):
    if c in ['\r', '\n']:
      return False

    if len(candidate) == 0:
      return c == '/'

    if len(candidate) == 1:
      return True

    slash_count = count_not_escaped('/', candidate)

    if slash_count < 2:
      return True

    return c.isalpha()


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 3:
      return 0

    if self.text[0] != '/':
      return 0

    if not self.pattern.match(self.text):
      return 0

    invalids = ['number', 'identifier', 'variable', 'function', 'symbol']
    if len(line_printable_tokens) > 0 and \
      line_printable_tokens[-1].group in invalids:
      return 0

    return len(self.text)


# token reader for /* */ comment that allows nesting
class NestedCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, opener, closer, limit):
    self.opener = opener
    self.closer = closer
    self.limit = limit
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment', False)]


  def accept(self, candidate, c):
    if len(candidate) > self.limit:
      return False

    if len(candidate) == 0:
      return c == self.opener[0]

    if len(candidate) == 1:
      return c == self.opener[1]

    if len(candidate) == 2:
      return True

    # walk through candidate and verify open delimiters
    level = 0
    prev = ''
    for c in candidate:
      if prev == self.opener[0] and c == self.opener[1]:
        level += 1

      if prev == self.closer[0] and c == self.closer[1]:
        level -= 1

      if level < 0:
        return False

      prev = c

    return level > 0


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.text.startswith(self.opener):
      return 0

    if not self.text.endswith(self.closer):
      return 0

    # walk through text and verify matched delimiters
    level = 0
    prev = ''
    for c in self.text:
      if prev == self.opener[0] and c == self.opener[1]:
        level += 1

      if prev == self.closer[0] and c == self.closer[1]:
        level -= 1

      if level < 0:
        return 0

      prev = c

    if level != 0:
      return 0

    return len(self.text)


# token reader for prefix/suffix block
class BlockTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, prefix, suffix, tokentype):
    self.prefix = prefix.lower()
    self.suffix = suffix.lower()
    self.texttype = tokentype
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, self.texttype, False)]


  def attempt(self, text, start):
    self.text = None

    n1 = len(self.prefix)
    n2 = n1 + start
    t3 = text[start:n2].lower()
    if t3 != self.prefix:
      return

    end1 = text.find(self.suffix.upper(), start)
    end2 = text.find(self.suffix.lower(), start)

    end = min(end1, end2)
    if end1 == -1:
      end = end2
    if end2 == -1:
      end = end1

    if end == -1:
      return

    end += len(self.suffix)

    self.text = text[start:end]


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.text.lower().startswith(self.prefix):
      return 0

    if not self.text.lower().endswith(self.suffix):
      return 0

    return len(self.text)
