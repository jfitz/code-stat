import re
import itertools

from codestat_token import Token

# generic TokenBuilder (to hold common functions)
class TokenBuilder:
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def attempt(self, text):
    self.text = None
    candidate = ''
    i = 0

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


# accept any character (but only one)
class InvalidTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def attempt(self, text):
    self.text = None
    if len(text) > 0:
      self.text = text[0]


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'invalid')]


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

    return [Token(self.text, 'whitespace')]


  def accept(self, candidate, c):
    result = c.isspace() and c != '\n' and c != '\r'

    return result


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

    return [Token(self.text, 'newline')]


  def accept(self, candidate, c):
    result = False

    if candidate == '':
      result = c == '\r' or c == '\n'

    if candidate == '\r' and c == '\n':
      result = True

    return result


# token reader for text literal (string)
class StringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, quotes, quote_stuffing, allow_newline, allow_unterm):
    self.quotes = quotes
    self.quote_stuffing = quote_stuffing
    self.allow_newline = allow_newline
    self.allow_unterm = allow_unterm
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0 and c in self.quotes:
      result = True

    if len(candidate) == 1:
      result = True

    if len(candidate) > 1:
      if self.quote_stuffing:
          # a quote character is accepted, even after closing quote
        if c == candidate[0]:
          result = True
        else:
          # if number of quote is even, string is closed
          count = candidate.count(candidate[0])
          result = count % 2 == 1
      else:
        # no quote stuffing, stop on second quote
        # assume escaped quotes are allowed
        quote_count = 0
        escaped = False
        for ch in candidate:
          if ch == candidate[0] and not escaped:
            quote_count += 1
          if ch == '\\':
            escaped = not escaped
          else:
            escaped = False

        result = quote_count < 2

    # newline breaks a string
    if c in ['\n', '\r'] and not self.allow_newline:
      result = False

    return result


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

    return [Token(self.text, 'string')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) < len(self.prefix):
      if self.case_sensitive:
        result = c == self.prefix[len(candidate)]
      else:
        result = c.lower() == self.prefix[len(candidate)]
    
    if len(candidate) == len(self.prefix):
      result = c in self.quotes

    if len(candidate) == len(self.prefix) + 1:
      result = True

    if len(candidate) > len(self.prefix) + 1:
      # no quote stuffing, stop on second quote
      # assume escaped quotes are allowed
      quote_count = 0
      escaped = False
      for ch in candidate:
        if ch == candidate[len(self.prefix)] and not escaped:
          quote_count += 1
        if ch == '\\':
          escaped = not escaped
        else:
          escaped = False

      result = quote_count < 2

    if c in ['\n', '\r']:
      result = False

    return result


# token reader for single-character text literal (string)
class CharTokenBuilder(TokenBuilder):
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

    return [Token(self.text, 'string')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0 and c in self.quotes:
      result = True

    if len(candidate) == 1:
      result = True

    if len(candidate) > 1:
      # no quote stuffing, stop on second quote
      # assume escaped quotes are allowed
      quote_count = 0
      escaped = False
      for ch in candidate:
        if ch == candidate[0] and not escaped:
          quote_count += 1
        if ch == '\\':
          escaped = not escaped
        else:
          escaped = False

      result = quote_count < 2

    # newline breaks a string
    if c in ['\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[-1] != self.text[0]:
      return 0

    if '\\' in self.text:
      # at most four chars (two quotes, backslash, and one other)
      if len(self.text) > 4:
        return 0
      # backslash must be first char (and may repeat for second)
      if self.text[1] != '\\':
        return 0
    else:
      # at most three chars (two quotes, one character)
      if len(self.text) > 3:
        return 0

    return len(self.text)


# token reader for single-character text literal (string)
class PrefixedCharTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, prefix, case_sensitive, quotes):
    self.quotes = quotes
    self.case_sensitive = case_sensitive
    if case_sensitive:
      self.prefix = prefix
    else:
      self.prefix = prefix.lower()
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) < len(self.prefix):
      if self.case_sensitive:
        result = c == self.prefix[len(candidate)]
      else:
        result = c.lower() == self.prefix[len(candidate)]
    
    if len(candidate) == len(self.prefix):
      result = c in self.quotes

    if len(candidate) == len(self.prefix) + 1:
      result = True

    if len(candidate) > len(self.prefix) + 1:
      # no quote stuffing, stop on second quote
      # assume escaped quotes are allowed
      quote_count = 0
      escaped = False
      for ch in candidate:
        if ch == candidate[len(self.prefix)] and not escaped:
          quote_count += 1
        if ch == '\\':
          escaped = not escaped
        else:
          escaped = False

      result = quote_count < 2

    # newline breaks a string
    if c in ['\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[-1] != self.text[0]:
      return 0

    if '\\' in self.text:
      # at most four chars (two quotes, backslash, and one other)
      if len(self.text) > 4:
        return 0
      # backslash must be first char (and may repeat for second)
      if self.text[1] != '\\':
        return 0
    else:
      # at most three chars (two quotes, one character)
      if len(self.text) > 3:
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

    return [Token(self.text, 'number')]


  def accept(self, candidate, c):
    result = False

    if c.isdigit():
      result = True
    
    if self.extra_char is not None and c == self.extra_char:
      result = len(candidate) > 0 and candidate[-1].isdigit()

    return result


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

    return [Token(self.text, 'number')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isdigit()
    
    if self.extra_char is not None and c == self.extra_char:
      result = len(candidate) > 0 and candidate[-1].isdigit()

    if len(candidate) > 0:
      result = c.isdigit() or (
        c.lower() == 'e' and 'e' not in candidate.lower()
      ) or (
        c in '+-' and candidate[-1].lower() == 'e'
      )

    return result


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


  def __init__(self, suffixes, extra_char):
    self.text = None
    self.suffixes = suffixes

    self.abbrevs = {}
    for suffix in self.suffixes:
      for i in range(len(suffix)):
        self.abbrevs[suffix[:i+1]] = 1

    self.extra_char = extra_char


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number')]


  def digit_or_underscore(self, c):
    if self.extra_char is not None and c == self.extra_char:
      return True

    return c.isdigit()


  def accept(self, candidate, c):
    result = False

    groups = ["".join(x) for _, x in itertools.groupby(candidate, key=self.digit_or_underscore)]

    if len(groups) == 0:
      result = c.isdigit()

    if len(groups) == 1:
      if self.extra_char is not None:
        result = c.isdigit() or c == self.extra_char or c in self.abbrevs
      else:
        result = c.isdigit() or c in self.abbrevs

    if len(groups) == 2:
      suffix = groups[1] + c
      result = suffix in self.abbrevs

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    groups = ["".join(x) for _, x in itertools.groupby(self.text, key=self.digit_or_underscore)]

    if len(groups) != 2:
      return 0

    if groups[1] not in self.suffixes:
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

    return [Token(self.text, 'number')]


  def accept(self, candidate, c):
    result = False

    if c.isdigit():
      result = True
    
    if c == '.' and '.' not in candidate:
      result = True

    if self.extra_char is not None and c == self.extra_char:
      result = len(candidate) > 0 and candidate[-1].isdigit()

    return result


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


  def __init__(self, require_before, require_after, suffixes, extra_char):
    self.text = None
    self.require_before = require_before
    self.require_after = require_after
    self.suffixes = suffixes
    self.extra_char = extra_char


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number')]


  def accept(self, candidate, c):
    result = False

    if c.isdigit():
      result = True
    
    if c == '.' and '.' not in candidate:
      result = True

    if self.extra_char is not None and c == self.extra_char:
      result = len(candidate) > 0 and candidate[-1].isdigit()

    if len(candidate) > 0 and c in self.suffixes:
      result = True

    if len(candidate) > 0 and candidate[-1] in self.suffixes:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.require_before and not self.text[0].isdigit():
      return 0

    point_position = self.text.find('.')

    if point_position == -1 or point_position == len(self.text) - 1:
      return 0

    if self.require_after and not self.text[point_position + 1].isdigit():
      return 0

    if self.text[-1] not in self.suffixes:
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

    return [Token(self.text, 'number')]


  def accept(self, candidate, c):
    result = False

    if c.isdigit():
      result = True

    if c == '.' and\
      '.' not in candidate and\
      self.letter not in candidate.lower():
      result = True

    if self.extra_char is not None and c == self.extra_char:
      result = len(candidate) > 0 and candidate[-1].isdigit()

    if c.lower() == self.letter\
      and len(candidate) > 0 and\
      self.letter not in candidate.lower():
      result = True

    if c in ['+', '-'] and\
      len(candidate) > 0 and\
      candidate[-1].lower() == self.letter:
      result = True

    return result


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


  def __init__(self):
    self.text = None

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier')]

  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isalpha() or c == '_'

    if len(candidate) > 0:
      result = c.isalpha() or c.isdigit() or c == '_'

    return result


class SingleCharacterTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, legals, group):
    self.legals = legals
    self.group = group
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, self.group)]


  def accept(self, candidate, c):
    return len(candidate) == 0 and c in self.legals


# accept characters to match item in list
class ListTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, legals, group, case_sensitive):
    if case_sensitive:
      self.legals = legals
    else:
      self.legals = list(map(str.lower, legals))

    self.abbrevs = {}
    for legal in self.legals:
      for i in range(len(legal)):
        self.abbrevs[legal[:i+1]] = 1

    self.group = group
    self.case_sensitive = case_sensitive
    self.text = ''


  def attempt(self, text):
    self.text = None
    best_candidate = None
    candidate = ''
    i = 0

    while i < len(text):
      c = text[i]

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

    self.text = best_candidate


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, self.group)]


  def accept(self, candidate, c):
    token = candidate + c
    result = False

    if self.case_sensitive:
      result = token in self.abbrevs
    else:
      result = token.lower() in self.abbrevs

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    score = 0
    if self.case_sensitive:
      if self.text in self.legals:
        score = len(self.text)
    else:
      if self.text.lower() in self.legals:
        score = len(self.text)

    return score


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

    return [Token(self.text, 'number')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) < len(self.prefix):
      if self.case_sensitive:
        result = c == self.prefix[len(candidate)]
      else:
        result = c.lower() == self.prefix[len(candidate)]
    
    if len(candidate) >= len(self.prefix):
      result = c in self.allowed_chars

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) <= len(self.prefix):
      return 0

    return len(self.text)


# token reader for ! comment
class LeadCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, lead):
    self.lead = lead
    self.text = ''

  def get_tokens(self):
    if self.text is None:
      return None

    if self.text.startswith(self.lead):
      return [Token(self.text, 'comment')]

    return None


  def accept(self, candidate, c):
    result = False

    if len(candidate) < len(self.lead):
      result = c == self.lead[len(candidate)]
    
    if candidate.startswith(self.lead):
      result = True

    if c in ['\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < len(self.lead):
      return 0

    return len(self.text)


# token reader for (* *) comment
class ParenStarCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment')]

  def accept(self, candidate, c):
    result = False

    if c == '(' and len(candidate) == 0:
      result = True

    if c == '*' and len(candidate) == 1:
      result = True

    if c == ')' and len(candidate) > 2 and candidate[-1] == '*':
      result = True

    if candidate.startswith('(*') and (candidate[-2] != '*' or candidate[-1] != ')'):
      result = True

    return result


# token reader for triple quote string
class TripleQuoteStringTokenBuilder(TokenBuilder):
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
    result = False

    if len(candidate) == 0 and c in '"\'':
      result = True

    if len(candidate) in [1, 2]:
      result = c == candidate[0]

    if len(candidate) > 2 and candidate[:3] in ['"""', "'''"]:
      result = True

    if len(candidate) > 5 and candidate[-3:] == candidate[:3]:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 6:
      return 0

    if self.text[:3] != self.text[-3:]:
      return 0
  
    return len(self.text)


# token reader for identifier
class PrefixedIdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, prefix, group):
    self.prefix = prefix
    self.group = group
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, self.group)]


  def accept(self, candidate, c):
    result = False

    if len(candidate) < len(self.prefix):
      result = self.prefix.startswith(candidate + c)
    else:
      result = c.isalpha() or c.isdigit() or c == '_'

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if not self.text.startswith(self.prefix):
      return 0

    return len(self.text)


# token reader for identifier
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

    return [Token(self.text, 'regex')]


  def accept(self, candidate, c):
    result = False
 
    if len(candidate) == 0:
      result = c == '/'

    if len(candidate) == 1:
      result = True

    if len(candidate) > 1:
      slash_count = 0
      escaped = False
      for ch in candidate:
        if ch == '/' and not escaped:
          slash_count += 1
        if ch == '\\':
          escaped = not escaped
        else:
          escaped = False

      if slash_count < 2:
        result = True
      else:
        result = c.isalpha()

    if c in ['\r', '\n']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 3:
      return 0

    if self.text[0] != '/':
      return 0

    if not self.pattern.match(self.text):
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

    return [Token(self.text, self.texttype)]


  def accept(self, candidate, c):
    result = False

    if len(candidate) < len(self.prefix):
      result = c.lower() == self.prefix[len(candidate)]

    if len(candidate) >= len(self.prefix):
      result = not candidate.lower().endswith(self.suffix)

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.text.lower().startswith(self.prefix):
      return 0

    if not self.text.lower().endswith(self.suffix):
      return 0

    return len(self.text)