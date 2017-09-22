from Examiner import Examiner

class PascalExaminer(Examiner):
  def __init__(self, code):
    confidence_1 = 0
    confidence_2 = 0
    first_token = ''
    last_token = ''
    num_begin = 0
    num_end = 0

    lines = code.split('\r\n')
    for line in lines:
      line = self.remove_pascal_comments(line)
      # remove leading and trailing blanks
      line = line.strip()
      #  consider only lines with text
      if len(line) > 0:
        line = self.remove_string_literals(line)
        #  simple lexer
        tokens = self.split_to_tokens(line)
        #  merge adjacent tokens when compatible
        #  drop all numbers
        tokens = self.drop_numbers(tokens)
        #  count 'begin' and 'end' tokens
        for token in tokens:
          if first_token == '':
            first_token = token
          last_token = token
          if token == 'begin':
            num_begin += 1
          if token == 'end':
            num_end += 1

    if first_token == 'program':
      confidence_1 += 0.5

    if last_token == '.':
      confidence_1 += 0.5

    if num_begin == num_end and num_begin > 0:
      confidence_2 = 1.0

    # compute confidence
    self.confidence = confidence_1 * confidence_2

  def confidence(self):
    return self.confidence

  def remove_pascal_comments(self, line):
    result = ''
    in_brace_comment = False
    for c in line:
      if c == '{' and not in_brace_comment:
        in_brace_comment = True
        c = ''

      if not in_brace_comment:
        result += c

      if c == '}':
        in_brace_comment = False

    return result
