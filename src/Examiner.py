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