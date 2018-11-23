from Token import Token
from TokenBuilders import TokenBuilder

# token reader for number
class BasicNumberTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'number')]

  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0 and c.isdigit():
      result = True

    if len(candidate) > 0 and not candidate.endswith('%') and (c.isdigit() or c == '%'):
      result = True
    
    return result

# token reader for variable
class VariableTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'identifier')]

  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0 and c.isalpha():
      result = True

    if len(candidate) == 1 and c.isdigit():
      result = True

    if len(candidate) > 0 and (c == '$' or c == '%'):
      result = True

    return result

# token reader for text literal (string)
class StringTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'string')]

  def accept(self, candidate, c):
    result = False

    if candidate == '' and c == '"':
      result = True

    if len(candidate) == 1:
      result = True

    if len(candidate) > 1 and candidate[-1] != candidate[0]:
      result = True

    if c == '\n' or c == '\r':
      result = False

    return result

# token reader for REMARK comment
class RemarkTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''

  def get_tokens(self):
    if self.token == None:
      return None

    token1 = Token('', 'comment')
    token2 = Token('', 'comment')

    if self.token.startswith('REM'):
      token1 = Token('REM', 'keyword')
      token2 = Token(self.token[3:], 'comment')

    if self.token.startswith('REMARK'):
      token1 = Token('REMARK', 'keyword')
      token2 = Token(self.token[6:], 'comment')

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

    return result
