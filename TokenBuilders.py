import re
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


  def get_score(self, line_printable_tokens):
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
  def __init__(self, quotes, quote_stuffing, allow_unterm):
    self.quotes = quotes
    self.quote_stuffing = quote_stuffing
    self.allow_unterm = allow_unterm
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

    if len(candidate) > 1:
      if candidate[-1] != candidate[0]:
        result = True
      else:
        if self.quote_stuffing:
          # string is terminated unless next is also a matching quote
          if c == candidate[0]:
            result = True
          else:
            if candidate[-2:] == candidate[0] * 2:
              result = True
            if candidate[-3:] == candidate[0] * 3:
              result = False
            if candidate[-4:] == candidate[0] * 2:
              result = True

    if c in ['\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    if not self.allow_unterm and self.token[-1] != self.token[0]:
      return 0

    return len(self.token)


# token reader for prefixed text literal (string)
class PrefixedStringTokenBuilder(TokenBuilder):
  def __init__(self, prefix, case_sensitive, quotes):
    self.prefix = prefix
    self.case_sensitive = case_sensitive
    self.quotes = quotes
    self.token = ''


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'string')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) < len(self.prefix):
      if self.case_sensitive:
        result = c == self.prefix[len(candidate)]
      else:
        result = c.lower() == self.prefix[len(candidate)].lower()
    
    if len(candidate) == len(self.prefix):
      result = c in self.quotes

    if len(candidate) == len(self.prefix) + 1:
      result = True

    if len(candidate) > len(self.prefix) + 1:
      result = candidate[-1] != candidate[len(self.prefix)]

    if c in ['\n', '\r']:
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


  def get_score(self, line_printable_tokens):
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
  def __init__(self, require_before, require_after, letter):
    self.token = None
    self.require_before = require_before
    self.require_after = require_after
    self.letter = letter


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'number')]


  def accept(self, candidate, c):
    result = False

    if c.isdigit():
      result = True

    if c == '.' and\
      '.' not in candidate and\
      self.letter.lower() not in candidate.lower():
      result = True

    if c.lower() == self.letter.lower()\
      and len(candidate) > 0 and\
      self.letter.lower() not in candidate.lower():
      result = True

    if c in ['+', '-'] and\
      len(candidate) > 0 and\
      candidate[-1].lower() == self.letter.lower():
      result = True

    return result


  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    # must have digit, decimal, 'E', and digit (or decimal, digit, 'E', digit)
    if len(self.token) < 4:
      return 0

    if '.' not in self.token:
      return 0

    if not self.letter.lower() in self.token.lower():
      return 0

    if not self.token[-1].isdigit():
      return 0

    if self.require_before and not self.token[0].isdigit():
      return 0

    point_position = self.token.find('.')

    if self.require_after and not self.token[point_position + 1].isdigit():
      return 0

    return len(self.token)


# token reader for identifier
class IdentifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'identifier')]

  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isalpha() or c == '_'

    if len(candidate) > 0:
      result = c.isalpha() or c.isdigit() or c == '_'

    return result


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
          break
    else:
      for legal in self.legals:
        if legal[:count].lower() == token.lower():
          result = True
          break

    return result


# token reader for integer
class PrefixedIntegerTokenBuilder(TokenBuilder):
  def __init__(self, prefix, case_sensitive, allowed_chars):
    self.token = None
    self.prefix = prefix
    self.case_sensitive = case_sensitive
    self.allowed_chars = allowed_chars


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'number')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) < len(self.prefix):
      if self.case_sensitive:
        result = c == self.prefix[len(candidate)]
      else:
        result = c.lower() == self.prefix[len(candidate)].lower()
    
    if len(candidate) >= len(self.prefix):
      result = c in self.allowed_chars

    return result


  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    if len(self.token) <= len(self.prefix):
      return 0

    return len(self.token)


# token reader for ! comment
class LeadCommentTokenBuilder(TokenBuilder):
  def __init__(self, lead):
    self.token = ''
    self.lead = lead

  def get_tokens(self):
    if self.token is None:
      return None

    if self.token.startswith(self.lead):
      return [Token(self.token, 'comment')]

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
    if self.token is None:
      return 0

    if len(self.token) < len(self.lead):
      return 0

    return len(self.token)


# token reader for triple quote string
class TripleQuoteCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'string')]

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


# token reader for identifier
class PrefixedIdentifierTokenBuilder(TokenBuilder):
  def __init__(self, prefix, group):
    self.token = None
    self.prefix = prefix
    self.group = group

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, self.group)]

  def accept(self, candidate, c):
    result = False

    if len(candidate) < len(self.prefix):
      result = self.prefix.startswith(candidate + c)
    else:
      result = c.isalpha() or c.isdigit() or c == '_'

    return result


# token reader for identifier
class RegexTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None
    self.pattern = re.compile('\\A/.+/[a-z]*\\Z')


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'regex')]


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
    if self.token is None:
      return 0

    if len(self.token) < 3:
      return 0

    if self.token[0] != '/':
      return 0

    if not self.pattern.match(self.token):
      return 0

    return len(self.token)
