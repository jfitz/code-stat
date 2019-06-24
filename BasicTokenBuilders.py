import itertools
from Token import Token
from TokenBuilders import TokenBuilder

# token reader for number
class BasicSuffixedIntegerTokenBuilder(TokenBuilder):
  def __init__(self, suffixes, allow_underscore):
    self.text = None
    self.suffixes = suffixes
    self.allow_underscore = allow_underscore


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number')]


  def accept(self, candidate, c):
    result = False

    if c.isdigit():
      result = True

    if c == '_':
      result = self.allow_underscore and\
        len(candidate) > 0 and candidate[-1].isdigit()

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

    if self.text[-1] not in self.suffixes:
      return 0

    return len(self.text)


# token reader for number
class BasicSuffixed2IntegerTokenBuilder(TokenBuilder):
  def __init__(self, suffix):
    self.text = None
    self.suffix = suffix


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number')]


  def accept(self, candidate, c):
    result = False

    groups = ["".join(x) for _, x in itertools.groupby(candidate, key=str.isdigit)]

    if len(groups) < 2:
      result = c.isdigit()

    if len(groups) == 1 and not c.isdigit():
      result = c == self.suffix[0]

    if len(groups) == 2 and len(groups[1]) < len(self.suffix):
      result = c == self.suffix[len(groups[1])]
    
    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 3:
      return 0

    groups = ["".join(x) for _, x in itertools.groupby(self.text, key=str.isdigit)]

    if len(groups) != 2:
      return 0

    if groups[1] != self.suffix:
      return 0

    return len(self.text)


# token reader for real (no exponent)
class BasicSuffixedRealTokenBuilder(TokenBuilder):
  def __init__(self, require_before, require_after, suffixes, allow_underscore):
    self.text = None
    self.require_before = require_before
    self.require_after = require_after
    self.suffixes = suffixes
    self.allow_underscore = allow_underscore


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

    if c == '_':
      result = self.allow_underscore and\
        len(candidate) > 0 and candidate[-1].isdigit()

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

    if point_position == -1:
      return 0

    if self.require_after and not self.text[point_position + 1].isdigit():
      return 0

    if self.text[-1] not in self.suffixes:
      return 0

    return len(self.text)


# token reader for variable
class BasicVariableTokenBuilder(TokenBuilder):
  def __init__(self, suffixes):
    self.text = None
    self.suffixes = suffixes


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0 and c.isalpha():
      result = True

    if len(candidate) == 1 and c.isdigit():
      result = True

    if len(candidate) > 0 and (candidate[-1].isalpha or candidate[-1].isdigit()) and c in self.suffixes:
      result = True

    return result


# token reader for REMARK comment
class RemarkTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text == None:
      return None

    token1 = Token('', 'comment')
    token2 = Token('', 'comment')

    if self.text.startswith('REM'):
      token1 = Token('REM', 'keyword')
      token2 = Token(self.text[3:], 'comment')

    if self.text.startswith('REMARK'):
      token1 = Token('REMARK', 'keyword')
      token2 = Token(self.text[6:], 'comment')

    return [token1, token2]


  def accept(self, candidate, c):
    result = False

    if c == 'R' and candidate == '':
      result = True

    if c == 'E' and candidate == 'R':
      result = True

    if c == 'M' and candidate == 'RE':
      result = True

    if candidate.startswith('REM'):
      result = True

    if c in ['\n', '\r']:
      result = False

    return result
