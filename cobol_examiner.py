from codestat_token import Token
from examiner import Examiner


def all_nines(s):
  for c in s:
    if c != '9':
      return False
  
  return True


class CobolExaminer(Examiner):
  def __init__(self):
    super().__init__()


  def convert_numbers_to_pictures(self):
    prev_token = Token('newline', '\n')

    for token in self.tokens:
      if token.group == 'number' and all_nines(token.text) and\
        prev_token.group == 'keyword' and prev_token.text in ['PIC', 'PICTURE']:
        token.group = 'picture'

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token


  def convert_numbers_to_levels(self):
    prev_token = Token('newline', '\n')

    for token in self.tokens:
      if token.group == 'number' and token.text.isdigit() and len(token.text) <= 2 and\
        prev_token.group == 'newline':
        token.group = 'level'

      if token.group not in ['whitespace', 'line number']:
        prev_token = token


  def check_expected_keywords(self):
    counts = {
      'IDENTIFICATION': 0,
      'ENVIRONMENT': 0,
      'DATA': 0,
      'PROCEDURE': 0
    }

    drop_types = ['newline', 'whitespace', 'comment', 'line continuation']

    tokens = Examiner.drop_tokens(self.tokens, drop_types)

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

    expected_keyword_confidence = 1.00
    if counts['IDENTIFICATION'] != 1:
      expected_keyword_confidence -= 0.01
      self.errors.append({
        'TYPE': 'EXPECTED KEYWORD',
        'MISSING': 'IDENTIFICATION or ID DIVISION'
      })

    if counts['ENVIRONMENT'] != 1:
      expected_keyword_confidence != 0.01
      self.errors.append({
        'TYPE': 'EXPECTED KEYWORD',
        'MISSING': 'ENVIRONMENT DIVISION'
      })

    if counts['DATA'] != 1:
      expected_keyword_confidence -= 0.01
      self.errors.append({
        'TYPE': 'EXPECTED KEYWORD',
        'MISSING': 'DATA DIVISION'
      })

    if counts['PROCEDURE'] != 1:
      expected_keyword_confidence -= 0.01
      self.errors.append({
        'TYPE': 'EXPECTED KEYWORD',
        'MISSING': 'PROCEDURE DIVISION'
      })

    return expected_keyword_confidence


  def calc_line_format_confidence(self):
    # check PICTURE keywords are followed by a picture element
    # and picture elements are preceded by a PICTURE keyword
    drop_types = ['newline', 'whitespace', 'comment', 'line continuation']

    tokens = Examiner.drop_tokens(self.tokens, drop_types)

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

    self.confidences['line format'] = picture_confidence
