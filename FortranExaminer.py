from Token import Token
from Examiner import Examiner


class FortranExaminer(Examiner):
  def __init__(self):
    super().__init__()

  def TokenizeLineNumber(self, line_number):
    tokens = []

    if len(line_number) > 0:
      if line_number.isspace():
        tokens.append(Token(line_number, 'whitespace'))
      else:
        if line_number.isdigit():
          tokens.append(Token(line_number, 'line number'))
        else:
          ln_2 = line_number.lstrip()
          ln_3 = ln_2.rstrip()

        front_space = ''
        front_count = len(line_number) - len(ln_2)
        if front_count > 0:
          front_space = ' ' * front_count
          tokens.append(Token(front_space, 'whitespace'))

        if ln_3.strip().isdigit():
          tokens.append(Token(ln_3, 'line number'))
        else:
          tokens.append(Token(line_number, 'invalid'))

        back_space = ''
        back_count = len(ln_2) - len(ln_3)
        if back_count > 0:
          back_space = ' ' * back_count
          tokens.append(Token(back_space, 'whitespace'))

    return tokens


  def TokenizeLineIndicator(self, line_indicator):
    token = None

    if line_indicator == ' ':
      token = Token(' ', 'whitespace')
    else:
      if line_indicator in 'C*':
        token = Token(line_indicator, 'comment')
      else:
        if line_indicator != '':
          token = Token(line_indicator, 'invalid')

    return token


  def TokenizeLine(self, line, tokenizer, wide):
    # break apart the line based on fixed format
    tokens = []

    # The fixed-format FORTRAN line format is:
    # 1: space or C or *
    # 2-6: line number or blank
    # 7: continuation character
    # 8-72: program text
    # 73-: identification, traditionally sequence number (ignored)

    line_indicator = line[0:1]
    line_number = line[1:5]
    line_continuation = line[5:6]
    if wide:
      line_text = line[6:]
      line_identification = ''
    else:
      line_text = line[6:72]
      line_identification = line[72:]

    # tokenize the line indicator
    if line_indicator in ['C', '*']:
      tokens.append(Token(line, 'comment'))
    else:
      if len(line_indicator) > 0 and line_indicator != ' ':
        tokens.append(Token(line, 'invalid'))
      else:
        tokens += self.TokenizeLineNumber(line_number)

        # tokenize line continuation character
        if len(line_continuation) > 0:
          if line_continuation.isspace():
            tokens.append(Token(line_continuation, 'whitespace'))
          else:
            tokens.append(Token(line_continuation, 'line continuation'))

        # tokenize the code
        tokens += tokenizer.tokenize(line_text)

        # tokenize the line identification
        if len(line_identification) > 0:
          tokens.append(Token(line_identification, 'line identification'))

    tokens.append(Token('\n', 'newline'))

    return tokens


  def TokenizeCode(self, code, tab_size, tokenizer, wide):
    lines = code.split('\n')

    tokens = []

    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = self.tabs_to_spaces(line, tab_size)

      line_tokens = self.TokenizeLine(line, tokenizer, wide)
      tokens += line_tokens

    return tokens
