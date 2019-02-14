import string
import re
from Token import Token
from TokenBuilders import TokenBuilder

# token reader for identifier
class FortranIdentifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'identifier')]


  def accept(self, candidate, c):
    result = False

    if c.isupper():
      result = True

    if len(candidate) > 0 and c.isdigit():
      result = True

    return result


# token reader for number
class LineNumberTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'line number')]


  def accept(self, candidate, c):
    return c.isdigit()


  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    boost = 0

    if len(line_printable_tokens) > 0 and line_printable_tokens[-1].group == 'newline':
      boost += 0.5

    return len(self.token) + boost


# token reader for FORMAT specifiers (but not Hollerith specifiers)
class FormatSpecifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None
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
    if self.token is None:
      return None

    return [Token(self.token, 'format')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isdigit() or c in 'IFEDGALXP'

    if self.t1.match(candidate):
      result = c.isdigit() or c in 'IFEDGALXP'

    if self.p2.match(candidate):
      result = c.isdigit()

    if self.p3.match(candidate):
      result = c.isdigit() or c == '.'

    if self.t4.match(candidate):
      result = c.isdigit()

    if self.p5.match(candidate):
      result = c == 'E'

    if self.t6.match(candidate):
      result = c.isdigit()

    if self.p7.match(candidate):
        result = c.isdigit()

    return result


  def get_score(self, line_printable_tokens):
    if self.token is None:
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
      if p.match(self.token):
        score = len(self.token)

    return score


# token reader for user-defined operator
class UserDefinedOperatorTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'line number')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c == '.'

    if len(candidate) == 1:
      result = c.isalpha() or c.isdigit()

    if len(candidate) > 1:
      if candidate[-1] != candidate[0]:
        result = c.isalpha() or c.isdigit() or c == '.'

    return result


  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    if len(self.token) < 3:
      return 0

    if self.token[-1] != self.token[0]:
      return 0

    return len(self.token)


# token reader for integer
class KindIntegerTokenBuilder(TokenBuilder):
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
    
    if len(candidate) > 0:
      if candidate.count('_') == 0:
        result = c.isdigit() or c == '_'

      if candidate.count('_') > 0:
        result = c.isdigit() or c.isalpha() or c == '_'

    return result


# token reader for real (no exponent)
class KindRealTokenBuilder(TokenBuilder):
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
    
    if len(candidate) > 0:
      if candidate.count('_') == 0:
        result = c.isdigit() or c == '.' or c == '_'
        if c == '.' and '.' not in candidate:
          result = True

      if candidate.count('_') > 0:
        result = c.isdigit() or c.isalpha() or c == '_'

    return result
