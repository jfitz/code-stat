import string
from Token import Token
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder
)
from FortranXTokenBuilders import (
  FortranIdentifierTokenBuilder,
  LineNumberTokenBuilder,
  FormatSpecifierTokenBuilder
)
from Fortran66TokenBuilders import (
  HollerithStringTokenBuilder
)
from Tokenizer import Tokenizer

class Fortran66Examiner(Examiner):
  def __init__(self, code, tab_size):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False)
    identifier_tb = FortranIdentifierTokenBuilder()
    string_tb = HollerithStringTokenBuilder()
    format_tb = FormatSpecifierTokenBuilder()

    known_operators = [
      '=', '+', '-', '*', '/', '**',
      '.EQ.', '.GT.', '.GE.', '.LT.', '.LE.', '.NE.',
      '.AND.', '.OR.', '.NOT.'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-', '*'
    ]

    groupers = ['(', ')', ',']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'IF', 'GO', 'TO', 'GOTO', 'ASSIGN',
      'READ', 'WRITE', 'BACKSPACE', 'REWIND', 'ENDFILE', 'FORMAT',
      'DO', 'CONTINUE',
      'SUBROUTINE', 'FUNCTION', 'BLOCK DATA',
      'INTEGER', 'REAL', 'COMPLEX', 'DOUBLE PRECISION', 'LOGICAL',
      'COMMON', 'DIMENSION', 'EQUIVALENCE',
      'DATA',
      'EXTERNAL',
      'CALL', 'RETURN', 'PAUSE', 'STOP', 'END'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      format_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      string_tb,
      self.unknown_operator_tb
    ]

    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    lines = code.split('\n')
    num_ok_lines = 0

    self.tokens = []
    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = self.tabs_to_spaces(line, tab_size)

      if len(line) <= 80:
        num_ok_lines += 1

      line = line[:72]

      # The fixed-format FORTRAN line format is:
      # 1: space or C
      # 2-6: line number or blank
      # 7: continuation character
      # 8-72: program text
      # 73-: identification, traditionally sequence number (ignored)
      line_indicator = ''
      if len(line) > 0:
        line_indicator = line[0]

      if len(line_indicator) > 0:
        if line_indicator.isspace():
          self.tokens.append(Token(line_indicator, 'whitespace'))
        else:
          if line_indicator == 'C':
            self.tokens.append(Token(line, 'comment'))
          else:
            self.tokens.append(Token(line_indicator, 'invalid'))

      if line_indicator != 'C':
        line_number = ''
        if len(line) > 2:
          line_number = line[2:5]

          if line_number.isspace():
            self.tokens.append(Token(line_number, 'whitespace'))
          else:
            ln_2 = line_number.lstrip()
            ln_3 = ln_2.rstrip()

            front_space = ''
            front_count = len(line_number) - len(ln_2)
            if front_count > 0:
              front_space = ' ' * front_count
              self.tokens.append(Token(front_space, 'whitespace'))
  
            if ln_3.strip().isdigit():
              self.tokens.append(Token(ln_3, 'line number'))
            else:
              self.tokens.append(Token(line_number, 'invalid'))

            back_space = ''
            back_count = len(ln_2) - len(ln_3)
            if back_count > 0:
              back_space = ' ' * back_count
              self.tokens.append(Token(back_space, 'whitespace'))


        line_continuation = ''
        if len(line) > 5:
          line_continuation = line[5]
          if line_continuation.isspace():
            self.tokens.append(Token(line_continuation, 'whitespace'))
          else:
            self.tokens.append(Token(line_continuation, 'line continuation'))

        line_text = ''
        if len(line) > 6:
          line_text = line[6:]
          self.tokens += tokenizer.tokenize(line_text)

      self.tokens.append(Token('\n', 'newline'))

    self.tokens = self.combineAdjacentWhitespace(self.tokens)

    line_length_confidence = 1.0
    if len(lines) > 0:
      line_length_confidence = num_ok_lines / len(lines)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operand_confidence()
    self.confidences['line_length'] = line_length_confidence
