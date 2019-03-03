from Token import Token
from TokenBuilders import TokenBuilder

# token reader for number
class BasicSuffixedIntegerTokenBuilder(TokenBuilder):
  def __init__(self, suffix):
    self.text = None
    self.suffix = suffix


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'number')]


  def accept(self, candidate, c):
    result = False

    if c.isdigit():
      result = True

    if len(candidate) > 0 and c == self.suffix:
      result = True

    if candidate.endswith(self.suffix):
      result = False
    
    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if not self.text.endswith(self.suffix):
      return 0

    return len(self.text)


# token reader for real (no exponent)
class BasicSuffixedRealTokenBuilder(TokenBuilder):
  def __init__(self, require_before, require_after, suffix):
    self.text = None
    self.require_before = require_before
    self.require_after = require_after
    self.suffix = suffix


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

    if len(candidate) > 0 and c == self.suffix:
      result = True

    if candidate.endswith(self.suffix):
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

    if not self.text.endswith(self.suffix):
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


# token reader for number
class LineNumberTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'line number')]


  def accept(self, candidate, c):
    return c.isdigit()


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    boost = 0

    if len(line_printable_tokens) == 0:
      boost += 0.5

    return len(self.text) + boost
