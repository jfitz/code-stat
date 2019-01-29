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
      token = Token(' ', 'whitespace')
    else:
      if line_indicator != '':
        token = Token(line_indicator, 'invalid')

    return token


  def TokenizeLine(self, line, tokenizer):
    # break apart the line based on fixed format
    tokens = []

    # The COBOL line format is:
    # 1-6: line number or blank (ignored)
    # 7: space or one of *, /, D, d, $, -
    # 8-71: program text
    # 72-: identification, traditionally sequence number (ignored)

    line_number = line[:6]
    line_indicator = line[6:7]
    line_text = line[7:71]
    line_identification = line[72:]

    token = self.TokenizeLineNumber(line_number)
    if token is not None:
      tokens.append(token)

    # tokenize the line indicator
    if line_indicator in ['*', '/', 'D', 'd', '$']:
      token = self.TokenizeAltLine(line, line_indicator)
      if token is not None:
        tokens.append(token)
    else:
      token = self.TokenizeLineIndicator(line_indicator)
      if token is not None:
        tokens.append(token)

      # tokenize the code
      tokens += tokenizer.tokenize(line_text)

    # tokenize the line identification
    if len(line_identification) > 0:
      tokens.append(Token(line_identification, 'line identification'))

    tokens.append(Token('\n', 'newline'))

    return tokens


  def TokenizeCode(self, code, tab_size, tokenizer):
    lines = code.split('\n')

    tokens = []

    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = self.tabs_to_spaces(line, tab_size)

      line_tokens = self.TokenizeLine(line, tokenizer)
      tokens += line_tokens

    return tokens
