from Token import Token
from TokenBuilders import TokenBuilder

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

    if c.isalpha():
      result = True

    if len(candidate) > 0 and c.isdigit():
      result = True

    if c == '_':
      result = True

    return result


# token reader for identifier
class SymbolTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None

  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'symbol')]

  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c == ':'

    if len(candidate) > 0:
      result = c.isalpha() or c.isdigit() or c == '_'

    return result


# token reader for identifier
class HereDocTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None
    self.oper = '<<-'

  def get_tokens(self):
    if self.token is None:
      return None

    # split the text into operator, marker, content, and marker tokens
    lines = self.token.split('\n')
    oper = lines[0][:3]
    marker = lines[-1]
    content = Token('\n'.join(lines[1:-1]), 'here doc')
    op_token = Token(oper, 'operator')
    mark_token = Token(marker, 'doc marker')
    newline_token = Token('\n', 'newline')

    # the marker token is used twice - once at beginning and once at end
    return [
      op_token,
      mark_token,
      newline_token,
      content,
      newline_token,
      mark_token
    ]

  def accept(self, candidate, c):
    result = False
 
    if len(candidate) < len(self.oper):
      result = self.oper.startswith(candidate)
    else:
      result = True

      # if the last line begins with the marker from the first line
      # stop accepting characters
      lines = candidate.split('\n')
      if len(lines) > 1:
        first_line = lines[0]
        last_line = lines[-1]
        marker = first_line[len(self.oper):].rstrip()
        if last_line.startswith(marker):
          result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    lines = self.token.split('\n')

    if len(lines) < 2:
      return 0

    line0 = lines[0].rstrip()
    if len(line0) < 4:
      return 0

    marker = lines[0][3:].rstrip()

    last_line = lines[-1].rstrip()

    if last_line != marker:
      return 0
    
    return len(self.token)
