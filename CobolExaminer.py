from Token import Token
from Examiner import Examiner


class CobolExaminer(Examiner):
  def __init__(self):
    super().__init__()

  def TokenizeLineNumber(self, line_number):
    token = None

    if len(line_number) > 0:
      if line_number.isspace():
        token = Token(line_number, 'whitespace')
      else:
        if line_number.isdigit():
          token = Token(line_number, 'line number')
        else:
          token = Token(line_number, 'line identification')

    return token


  def TokenizeAltLine(self, line, line_indicator):
    token = None

    if line_indicator in ['*', '/', 'D', 'd']:
      # the entire line is a comment (including DEBUG lines)
      token = Token(line[6:], 'comment')
    if line_indicator == '$':
      token = Token(line[6:], 'preprocessor')

    return token


  def TokenizeLineIndicator(self, line_indicator):
    token = None

    if line_indicator == ' ':
      self.tokens.append(Token(' ', 'whitespace'))
    else:
      if line_indicator != '':
        self.tokens.append(Token(line_indicator, 'invalid'))

    return token