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
    self.confidences = {}


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


  def count_valid_tokens(self, tokens):
    num = 0
    for token in tokens:
      if not token.group.startswith('invalid'):
        num += 1

    return num


  def count_invalid_operators(self, tokens):
    num = 0
    for token in tokens:
      if token.group == 'invalid operator':
        num += 1

    return num


  def count_known_operators(self, tokens):
    num = 0
    for token in tokens:
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


  def find_specific_keywords(self, tokens, keywords):
    found_keywords = set()

    for token in tokens:
      if token.group == 'keyword':
        token_lower = str(token).lower()
        if token_lower in keywords:
          found_keywords.add(str(token))

    return found_keywords


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
      num_known_tokens = self.count_valid_tokens(self.tokens)
      token_confidence = num_known_tokens / len(self.tokens)

    self.confidences['token'] = token_confidence
