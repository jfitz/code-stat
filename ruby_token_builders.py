from codestat_token import Token
from token_builders import TokenBuilder

# token reader for identifier
class RubyIdentifierTokenBuilder(TokenBuilder):
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
    if len(candidate) == 0:
      return c.isalpha() or c == '_' or c == '@'

    if len(candidate) == 1:
      if candidate == '@':
        return c.isalpha() or c == '_' or c == '@'
      else:
        return c.isalpha() or c.isdigit() or c in ['_', '?', '!']

    if candidate[-1] in ['?', '!']:
      return False

    return c.isalpha() or c.isdigit() or c in ['_', '?', '!']


# token reader for identifier
class HereDocTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, operator):
    self.text = None
    self.operator = operator

  def get_tokens(self):
    if self.text is None:
      return None

    # split the text into operator, marker, content, and marker tokens
    lines = self.text.split('\n')
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
    if len(candidate) < len(self.operator):
      return self.operator.startswith(candidate)

    result = False

    if candidate.startswith(self.operator):
      result = True

      # if the last line begins with the marker from the first line
      # stop accepting characters
      lines = candidate.split('\n')
      if len(lines) > 1:
        first_line = lines[0]
        last_line = lines[-1]
        marker = first_line[len(self.operator):].rstrip()
        if last_line.startswith(marker):
          result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    lines = self.text.split('\n')

    if len(lines) < 2:
      return 0

    line0 = lines[0].rstrip()
    if len(line0) < 4:
      return 0

    marker = lines[0][3:].rstrip()

    last_line = lines[-1].rstrip()

    if last_line != marker:
      return 0
    
    return len(self.text)
