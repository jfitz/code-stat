import string
from Examiner import Examiner

class BasicExaminer(Examiner):
  def __init__(self, code):
    lines = code.split('\r\n')

    # Pass 1 - all lines begin with numbers
    num_lines_start_num = 0
    for line in lines:
      if len(line) > 0 and line[0].isdigit():
        num_lines_start_num += 1
    confidence_1 = num_lines_start_num / len(lines)

    # Pass 2 - reasonable tokens
    num_tokens = 0
    num_known_tokens = 0
    operators = [
      '(', ')', '=', '+', '-', '*', '/', '^', '<=', '<', '>', '>=',
      ',', ';', ':', '#', '%', '$', '.'
      ]
    functions = [ 'INT', 'TAB', 'EXP', 'SIN', 'COS', 'SQR' ]
    user_functions = [
      'FNA', 'FNB', 'FNC', 'FND', 'FNE', 'FNF', 'FNG', 'FNH', 'FNI', 'FNJ',
      'FNK', 'FNL', 'FNM', 'FNN', 'FNO', 'FNP', 'FNQ', 'FNR', 'FNS', 'FNT',
      'FNU', 'FNV', 'FNW', 'FNX', 'FNY', 'FNZ'
      ]
    keywords = [
      'READ', 'DATA', 'DEF', 'DIM', 'FOR', 'TO', 'STEP', 'NEXT',
      'IF', 'THEN', 'ELSE',
      'CHANGE', 'LET',
      'PRINT', 'USING', 'INPUT', 'LINE', 'LINPUT',
      'GOTO', 'GOSUB', 'ON', 'RETURN', 'END', 'STOP',
      'OPEN', 'CLOSE'
      ]
    defined_tokens = keywords + functions + user_functions + operators

    for line in lines:
      #  if line begins with number, remove number
      line = line.lstrip(string.digits)
      # remove leading and trailing blanks
      line = line.strip()
      # remove comments (REM and apostrophe)
      if line.startswith('REM'):
        line = ''
      #  consider only lines with text
      if len(line) > 0:
        line = self.remove_string_literals(line)
        #  simple lexer
        tokens = self.split_to_tokens(line)
        #  merge adjacent tokens when compatible
        #  drop all numbers
        tokens = self.drop_numbers(tokens)
        #  unknown operators reduce confidence
        #  unknown identifiers (text of two or more, not FNx) reduce confidence
        for token in tokens:
          num_tokens += 1
          if token in defined_tokens:
            num_known_tokens += 1
          elif self.is_variable(token):
            num_known_tokens += 1

    confidence_2 = 0
    if num_tokens > 0:
      confidence_2 = num_known_tokens / num_tokens

    # compute confidence
    self.confidence = confidence_1 * confidence_2

  def confidence(self):
    return self.confidence

  def is_variable(self, token):
    if len(token) == 1 and token[0].isalpha():
      return True
    if len(token) == 2 and token[0].isalpha() and token[1].isdigit():
      return True
    return False
