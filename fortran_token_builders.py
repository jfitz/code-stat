import string
import re

from codestat_token import Token
from token_builders import TokenBuilder

# token reader for identifier
class FortranIdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier', True)]


  def accept(self, candidate, c):
    if c.isupper():
      return True

    if len(candidate) > 0 and c.isdigit():
      return True

    return False


# token reader for FORMAT specifiers (but not Hollerith specifiers)
class FormatSpecifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None
    # 'p' patterns are approved, 't' patterns are for accept()
    self.t1 = re.compile(r'\A\d+\Z')
    self.p2 = re.compile(r'\A\d*[IFEDGAL]\Z')
    self.p3 = re.compile(r'\A\d*[IFEDG]\d+\Z')
    self.t4 = re.compile(r'\A\d*[IFEDG]\d+\.\Z')
    self.p5 = re.compile(r'\A\d*[FEDG]\d+\.\d+\Z')
    self.t6 = re.compile(r'\A\d*[FEDG]\d+\.\d+E\Z')
    self.p7 = re.compile(r'\A\d*[FEDG]\d+\.\d+E\d+\Z')

    self.approveds = [
      self.p2,
      self.p3,
      self.p5,
      self.p7
    ]


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'format', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isdigit() or c in 'IFEDGALXP'

    if self.t1.match(candidate):
      return c.isdigit() or c in 'IFEDGALXP'

    if self.p2.match(candidate):
      return c.isdigit()

    if self.p3.match(candidate):
      return c.isdigit() or c == '.'

    if self.t4.match(candidate):
      return c.isdigit()

    if self.p5.match(candidate):
      return c == 'E'

    if self.t6.match(candidate):
      return c.isdigit()

    if self.p7.match(candidate):
      return c.isdigit()

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    in_format = False
    for token in line_printable_tokens:
      if token.text == 'FORMAT':
        in_format = True

    if not in_format:
      return 0

    score = 0

    # check token matches an approved pattern
    for p in self.approveds:
      if p.match(self.text):
        score = len(self.text)

    return score


# token reader for Hollerith string constant
class HollerithStringTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'string', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isdigit()

    if len(candidate) == 1:
      return c.isdigit() or c == 'H'

    if len(candidate) == 2 and candidate[1].isdigit():
      return c == 'H'

    if c not in string.printable:
      return False

    spec_length = 1 # for the H
    # compute length
    length = int(candidate[0])
    spec_length += 1 # for the digit

    if candidate[1].isdigit():
      length = length * 10 + int(candidate[1])
      spec_length += 1 # for the digit

    length += spec_length

    return len(candidate) < length


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 3:
      return 0

    return len(self.text)


# token reader for user-defined operator
class UserDefinedOperatorTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'line number', False)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c == '.'

    if len(candidate) == 1:
      return c.isalpha() or c.isdigit()

    if candidate[-1] != candidate[0]:
      return c.isalpha() or c.isdigit() or c == '.'

    return False


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 3:
      return 0

    if self.text[-1] != self.text[0]:
      return 0

    return len(self.text)


# token reader for integer
class KindIntegerTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number', True)]


  def accept(self, candidate, c):
    if c.isdigit():
      return True
    
    if len(candidate) > 0:
      if candidate.count('_') == 0:
        return c.isdigit() or c == '_'

      return c.isdigit() or c.isalpha() or c == '_'

    return False


# token reader for real (no exponent)
class KindRealTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number', True)]


  def accept(self, candidate, c):
    if c.isdigit():
      return True
    
    result = False

    if len(candidate) > 0:

      if candidate.count('_') == 0:
        result = c.isdigit() or c == '.' or c == '_'

        if c == '.' and '.' not in candidate:
          return True

      if candidate.count('_') > 0:
        return c.isdigit() or c.isalpha() or c == '_'

    return result
