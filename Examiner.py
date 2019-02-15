from Token import Token
from TokenBuilders import ListTokenBuilder

class Examiner:
  def __init__(self):
    operators = [
      '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
      '-', '+', '=',
      '[', ']', '{', '}', '/', '|', '\\',
      ',', '.', ':', ';',
      '>', '<',
      '?'
    ]

    self.tokens = []
    self.unknown_operator_tb = ListTokenBuilder(operators, 'invalid operator', True)
    self.unary_operators = []
    self.postfix_operators = []
    self.confidences = {}
    self.errors = []


  def confidence(self):
    value = 1.0

    for name in self.confidences:
      value *= self.confidences[name]

    return value


  def tabs_to_spaces(self, text, tab_size):
    if tab_size is None:
      tab_size = 8

    column = 0
    detabbed_text = ''

    for c in text:
      if c == '\n':
        detabbed_text += c
        column = 0
      else:
        if c == '\t':
          next_tab_stop = int((column + tab_size) / tab_size) * tab_size
          while column < next_tab_stop:
            detabbed_text += ' '
            column += 1
        else:
          detabbed_text += c
          column += 1
    
    return detabbed_text


  def count_valid_tokens(self):
    num = 0
    for token in self.tokens:
      if not token.group.startswith('invalid'):
        num += 1

    return num


  def invalid_operators(self):
    tokens = []
    for token in self.tokens:
      if token.group == 'invalid operator':
        tokens.append(token)

    return tokens


  def count_invalid_operators(self):
    num = 0
    for token in self.tokens:
      if token.group == 'invalid operator':
        num += 1

    return num


  def count_known_operators(self):
    num = 0
    for token in self.tokens:
      if token.group == 'operator':
        num += 1

    return num


  def find_keywords(self):
    found_keywords = set()

    for token in self.tokens:
      if token.group == 'keyword':
        found_keywords.add(str(token))

    return found_keywords


  def find_identifiers(self):
    found_identifiers = set()

    for token in self.tokens:
      if token.group in ['identifier', 'function']:
        found_identifiers.add(str(token))

    return found_identifiers


  def check_paired_tokens(self, tokens, open_tokens, close_tokens):
    level = 0
    min_level = 0
    num_open = 0
    num_close = 0

    for token in tokens:
      token_lower = token.text.lower()

      if token_lower in open_tokens:
        num_open += 1
        level += 1

      if token_lower in close_tokens:
        num_close += 1
        level -= 1
        if level < min_level:
          min_level = level

    ok = level == 0 and min_level == 0
    return ok, num_open, num_close


  def split_tokens(self, tokens):
    token_groups = []

    token_group = []

    for token in tokens:
      if token.group == 'newline':
        if len(token_group) > 0:
          token_groups.append(token_group)
          token_group = []
      else:
        token_group.append(token)
    
    if len(token_group) > 0:
      token_groups.append(token_group)

    return token_groups


  def drop_whitespace(self, tokens):
    new_list = []

    for token in tokens:
      if token.group != 'whitespace':
        new_list.append(token)
    
    return new_list


  def drop_comments(self, tokens):
    new_list = []

    for token in tokens:
      if token.group != 'comment':
        new_list.append(token)
    
    return new_list


  def drop_tokens(self, tokens, types):
    new_list = []

    for token in tokens:
      if token.group not in types:
        new_list.append(token)
    
    return new_list


  def combineAdjacentWhitespace(self, tokens):
    new_list = []

    new_token = None
    for token in tokens:
      if token.group == 'whitespace' and new_token is not None and new_token.group == 'whitespace':
        new_token = Token(new_token.text + token.text, 'whitespace')
      else:
        if new_token is not None:
            new_list.append(new_token)
        new_token = token
    if new_token is not None:
      new_list.append(new_token)

    return new_list


  def calc_token_confidence(self):
    # unknown tokens reduce confidence
    token_confidence = 1.0

    if len(self.tokens) > 0:
      num_known_tokens = self.count_valid_tokens()
      token_confidence = num_known_tokens / len(self.tokens)

    self.confidences['token'] = token_confidence


  def calc_operator_confidence(self):
    #  unknown operators reduce confidence
    invalid_operators = self.invalid_operators()
    num_invalid_operators = self.count_invalid_operators()
    num_known_operators = self.count_known_operators()
    num_operators = num_known_operators + num_invalid_operators

    for operator in invalid_operators:
      self.errors.append({
        'TYPE': 'OPERATOR',
        'INVALID': operator.text
      })

    operator_confidence = 1.0

    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    self.confidences['operator'] = operator_confidence


  def calc_operator_2_confidence(self):
    # binary operators that follow operators reduce confidence
    num_invalid_operators = self.count_invalid_operators()
    num_known_operators = self.count_known_operators()
    num_operators = num_known_operators + num_invalid_operators

    operator_confidence_2 = 1.0

    if num_operators > 0:
      errors = 0
      prev_token = Token('\n', 'newline')

      for token in self.tokens:
        if token.group == 'operator' and\
          prev_token.group == 'operator' and\
          prev_token.text not in self.postfix_operators and\
          token.text not in self.unary_operators:
          errors += 1
          self.errors.append({
            'TYPE': 'OPERATOR',
            'FIRST': prev_token.text,
            'SECOND': token.text
            })

        prev_token = token

      operator_confidence_2 = 1.0 - errors / num_operators

    self.confidences['operator_2'] = operator_confidence_2


  def calc_paired_blockers_confidence(self, openers, closers):
    # consider the number of matches for begin/end
    ok, num_begin, num_end = self.check_paired_tokens(self.tokens, openers, closers)
    num_begin_end = num_begin + num_end
    paired_blocker_confidence = 0.0

    if num_begin_end > 0:
      paired_blocker_confidence = (num_begin + num_end) / num_begin_end

    if not ok:
      paired_blocker_confidence *= 0.75

    self.confidences['paired_blockers_match'] = paired_blocker_confidence


  def calc_line_format_confidence(self):
    self.confidences['line_format'] = 1.0


  def calc_operand_confidence(self):
    # two operands in a row decreases confidence
    tokens = self.drop_whitespace(self.tokens)
    tokens = self.drop_comments(tokens)

    operands = ['number', 'string', 'identifier', 'variable']

    two_operand_count = 0
    prev_token = Token('\n', 'newline')
    for token in tokens:
      if token.group in operands and prev_token.group in operands:
        two_operand_count += 1
        self.errors.append({
          'TYPE': 'OPERAND',
          'FIRST': prev_token.text,
          'SECOND': token.text
          })

      prev_token = token

    operand_confidence = 1.0
    if len(tokens) > 0:
      operand_confidence = 1.0 - (two_operand_count / len(tokens))

    self.confidences['operand'] = operand_confidence


  def calc_picture_confidence(self):
    self.confidences['picture'] = 1.0
