class Examiner:
  def __init__(self):
    self.operators = [
      '+', '-', '*', '/', '%', '&', '|', '~', '^', '\\', '#',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=',
       '<<', '>>',
       '<<=', '>>=',
      '=', '==', '!=', '<>', '>', '>=', '<', '<=',
      '++', '--', '**', '//'
      'and', 'or', 'not',
      '{', '}', '(', ')', '[', ']',
      ':=', '..',
      ',', '.', ':', ';',
      '<-', '->', '<->', '<=>',
      '@',
      '&&', '||',
      '::', '?',
      ':-',
      '%>%'
      ]


  def common_operators(self):
    return self.operators


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


  def find_keywords(self, tokens):
    found_keywords = set()

    for token in tokens:
      if token.group == 'keyword':
        found_keywords.add(str(token))

    return found_keywords


  def find_specific_keywords(self, tokens, keywords):
    found_keywords = set()

    for token in tokens:
      if token.group == 'keyword':
        token_lower = str(token).lower()
        if token_lower in keywords:
          found_keywords.add(str(token))

    return found_keywords


  def check_paired_tokens(self, tokens, open_token, close_token):
    level = 0
    min_level = 0
    num_open = 0
    num_close = 0

    for token in tokens:
      token_lower = str(token).lower()

      if token_lower == open_token:
        num_open += 1
        level += 1

      if token_lower == close_token:
        num_close += 1
        level -= 1
        if level < min_level:
          min_level = level

    ok = level == 0 and min_level == 0
    return ok, num_open, num_close
