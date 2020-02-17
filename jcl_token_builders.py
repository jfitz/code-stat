from codestat_token import Token
from token_builders import TokenBuilder

# token reader for JCL
class JCLTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'jcl')]


  def accept(self, candidate, c):
    result = False

    if candidate == '' and c == '/':
      result = True

    if candidate == '/' and c == '/':
      result = True

    if candidate.startswith('//'):
      result = True

    if c in ['\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # print('TEXT ' + self.text)
    if len(line_printable_tokens) > 0:
      return 0

    if not self.text.startswith('//'):
      return 0

    return len(self.text)
