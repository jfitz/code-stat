import string
from Examiner import Examiner
from TokenBuilders import *
from FortranXTokenBuilders import *
from Fortran66TokenBuilders import *
from Tokenizer import Tokenizer

class Fortran66Examiner(Examiner):
  def __init__(self, code, tab_size):
    super().__init__()

    num_known_tokens = 0
    num_invalid_operators = 0
    num_known_operators = 0

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False)
    identifier_tb = FortranIdentifierTokenBuilder()
    string_tb = HollerithStringTokenBuilder()

    known_operators = [
      '=', '+', '-', '*', '/', '**',
      '.EQ.', '.GT.', '.GE.', '.LT.', '.LE.', '.NE.',
      '.AND.', '.OR.', '.NOT.',
      '(', ')', ','
      ]
    
    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

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
      identifier_tb,
      known_operator_tb,
      self.unknown_operator_tb,
      string_tb
    ]

    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    lines = code.split('\n')

    self.tokens = []
    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      line = self.tabs_to_spaces(line, tab_size)

      # The fixed-format FORTRAN line format is:
      # 1: space or C
      # 2-6: line number or blank
      # 7: continuation character
      # 8-71: program text
      # 72-: identification, traditionally sequence number (ignored)
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
          line_text = line[6:71]
          self.tokens += tokenizer.tokenize(line_text)

      self.tokens.append(Token('\n', 'newline'))

    num_known_tokens = self.count_valid_tokens(self.tokens)
    num_invalid_operators = self.count_invalid_operators(self.tokens)
    num_known_operators = self.count_known_operators(self.tokens)

    # unknown tokens reduce confidence
    token_confidence = 1.0
    if len(self.tokens) > 0:
      token_confidence = num_known_tokens / len(self.tokens)

    #  unknown operators reduce confidence
    operator_confidence = 1.0
    num_operators = num_known_operators + num_invalid_operators
    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    # compute confidence
    self.confidence = token_confidence * operator_confidence
    self.confidences = {
      'token': token_confidence,
      'operator': operator_confidence
      }
