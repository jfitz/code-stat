from Token import Token
from Examiner import Examiner


class FortranExaminer(Examiner):
  def __init__(self):
    super().__init__()
    self.newlines_important = 'always'

  def tokenize_line_number(self, line_number):
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


  def tokenize_line(self, line, tokenizer, wide):
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
        tokens += self.tokenize_line_number(line_number)

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


  def tokenize_code(self, code, tab_size, tokenizer, wide):
    lines = code.split('\n')

    tokens = []

    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = self.tabs_to_spaces(line, tab_size)

      line_tokens = self.tokenize_line(line, tokenizer, wide)
      tokens += line_tokens

    return tokens


  def convert_numbers_to_lineNumbers(self):
    prev_token = Token('\n', 'newline')

    for token in self.tokens:
      if token.group == 'number' and\
        prev_token.group == 'newline':
        token.group = 'line number'

      if token.group not in ['whitespace', 'comment']:
        prev_token = token


  def convert_stars_to_io_channels(self):
    prev_tokens = [
      Token('\n', 'newline'),
      Token('\n', 'newline'),
      Token('\n', 'newline'),
      Token('\n', 'newline')
    ]

    for token in self.tokens:
      if token.group == 'operator' and token.text == '*' and\
        prev_tokens[-1].group == 'keyword' and\
        prev_tokens[-1].text.lower() in ['read', 'write', 'print']:
        token.group = 'number'

      if token.group == 'operator' and token.text == '*' and\
        prev_tokens[-1].group == 'group' and\
        prev_tokens[-1].text == '(' and\
        prev_tokens[-2].group == 'keyword' and\
        prev_tokens[-2].text.lower() in ['read', 'write', 'print']:
        token.group = 'number'

      if token.group == 'operator' and token.text == '*' and\
        prev_tokens[-1].group == 'group' and\
        prev_tokens[-1].text == ',' and\
        prev_tokens[-2].group == 'number' and\
        prev_tokens[-3].group == 'group' and\
        prev_tokens[-3].text == '(' and\
        prev_tokens[-4].group == 'keyword' and\
        prev_tokens[-4].text.lower() in ['read', 'write', 'print']:
        token.group = 'number'

      if token.group not in ['whitespace', 'comment']:
        prev_tokens.append(token)
        prev_tokens.pop(0)
