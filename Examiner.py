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