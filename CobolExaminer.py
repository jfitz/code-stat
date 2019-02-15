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
      if line_indicator == '-':
        token = Token(line_indicator, 'continuation line')
      else:
        if line_indicator != '':
          token = Token(line_indicator, 'invalid')

    return token


  def TokenizeLine(self, line, tokenizer, wide):
    # break apart the line based on fixed format
    tokens = []

    # The COBOL line format is:
    # 1-6: line number or blank (ignored)
    # 7: space or one of *, /, D, d, $, -
    # 8-71: program text
    # 72-: identification, traditionally sequence number (ignored)

    line_number = line[:6]
    line_indicator = line[6:7]
    if wide:
      line_text = line[7:]
      line_identification = ''
    else:
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


  def CheckExpectedKeywords(self):
    # check expected keywords
    counts = {
      'IDENTIFICATION': 0,
      'ENVIRONMENT': 0,
      'DATA': 0,
      'PROCEDURE': 0
    }

    tokens = self.drop_whitespace(self.tokens)
    tokens = self.drop_comments(tokens)

    prev_text = ''
    for token in tokens:
      text = token.text

      if text == 'DIVISION' and prev_text in ['IDENTIFICATION', 'ID']:
        counts['IDENTIFICATION'] += 1
      if text == 'DIVISION' and prev_text == 'ENVIRONMENT':
        counts['ENVIRONMENT'] += 1
      if text == 'DIVISION' and prev_text == 'DATA':
        counts['DATA'] += 1
      if text == 'DIVISION' and prev_text == 'PROCEDURE':
        counts['PROCEDURE'] += 1

      prev_text = text

    expected_keyword_confidence = 0.50
    if counts['IDENTIFICATION'] == 1:
      expected_keyword_confidence += 0.125
    if counts['ENVIRONMENT'] == 1:
      expected_keyword_confidence += 0.125
    if counts['DATA'] == 1:
      expected_keyword_confidence += 0.125
    if counts['PROCEDURE'] == 1:
      expected_keyword_confidence += 0.125

    return expected_keyword_confidence


  def calc_line_format_confidence(self):
    # check PICTURE keywords are followed by a picture element
    # and picture elements are preceded by a PICTURE keyword
    tokens = self.drop_whitespace(self.tokens)
    tokens = self.drop_comments(tokens)
    tokens = self.drop_tokens(tokens, ['newline'])

    errors = 0
    prev_token = Token('\n', 'newline')
    for token in tokens:

      if prev_token.group == 'keyword' and prev_token.text in ['PIC', 'PICTURE']:
        if token.group != 'picture':
          errors += 1
          self.errors.append({
            'TYPE': 'PICTURE',
            'FIRST': prev_token.text,
            'SECOND': token.text
            })

      if token.group == 'picture':
        if prev_token.group != 'keyword' or prev_token.text not in ['PIC', 'PICTURE']:
          errors += 1
          self.errors.append({
            'TYPE': 'PICTURE',
            'FIRST': prev_token.text,
            'SECOND': token.text
            })
  
    picture_confidence = 1.0

    if len(self.tokens) > 0:
      picture_confidence = errors / len(self.tokens)

    self.confidences['line_format'] = picture_confidence
