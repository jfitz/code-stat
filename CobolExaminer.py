import string
from TokenBuilders import *
from CobolTokenBuilders import *
from Tokenizer import Tokenizer

class CobolExaminer:
  def __init__(self, code):
    lines = code.split('\n')

    num_tokens = 0
    num_known_tokens = 0
    num_operators = 0
    num_known_operators = 0

    wtb = WhitespaceTokenBuilder()

    all_operators = [
      '+', '-', '*', '/', '%',
      '=', '<>', '>', '>=', '<', '<=',
      'and', 'and', 'or', 'not',
      '&', '|', '~', '<<', '>>',
      ':=', '^',
      '(', ')', ',', ':', ';',
      '[', ']', '..',
      '++', '--', '**', '->', '<->', '<=>', '@', '&&', '||',
      '\\', '::', '?', '{', '}'
      ]

    ntb = NumberTokenBuilder()
    itb = IdentifierTokenBuilder()
    stb = StringTokenBuilder()
    ptb = PictureTokenBuilder()

    known_operators = [
      'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
      '=', '<>', '>', '>=', '<', '<=',
      'AND', 'OR', 'NOT',
      '(', ')', ',', ':', '.'
      ]
    
    kotb = ListTokenBuilder(known_operators, 'operator')

    unknown_operators = set(all_operators) - set(known_operators)
    uotb = ListTokenBuilder(unknown_operators, 'invalid operators')

    nltb = NewlineTokenBuilder()

    keywords = [
      'ACCEPT',
      'ASSIGN TO',
      'AT END',
      'CLOSE',
      'DATA DIVISION.',
      'DATA RECORDED IS',
      'DISK',
      'DISPLAY',
      'ELSE',
      'END-IF',
      'END-PERFORM',
      'END-READ',
      'ENVIRONMENT DIVISION.',
      'FD',
      'FILE-CONTROL.',
      'FILE SECTION.',
      'FILLER',
      'GREATER THAN',
      'ID DIVISION.',
      'IF',
      'INPUT-OUTPUT SECTION.',
      'IS',
      'LESS THAN',
      'LINE SEQUENTIAL',
      'MOVE',
      'NOT GREATER THAN',
      'NOT LESS THAN',
      'ON ASCENDING KEY',
      'ON DESCENDING KEY',
      'OPEN INPUT',
      'OPEN OUTPUT',
      'ORGANIZATION IS',
      'PERFORM',
      'PROCEDURE DIVISION.',
      'PROGRAM-ID.',
      'PIC',
      'PICTURE',
      'PRINTER',
      'READ',
      'RECORD',
      'RETURN',
      'SELECT',
      'SORT',
      'SPACES.',
      'STOP RUN.',
      'TAPE',
      'TO',
      'UNTIL',
      'VALUE SPACES.',
      'VALUE',
      'VALUES',
      'WORKING-STORAGE SECTION.',
      'WRITE'
      ]

    ktb = ListTokenBuilder(keywords, 'keyword')
    
    tokenbuilders = [wtb, ntb, itb, stb, ptb, kotb, uotb, nltb, ktb]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    found_keywords = set()
    
    self.tokens = []
    for line in lines:
      line = line.strip()
      
      if len(line) > 7 and line[7] != '*':
        tokens = tokenizer.tokenize(line)
        
        for token in tokens:
          self.tokens.append(token.to_debug())

          num_tokens += 1
          if not token.group.startswith('invalid'):
            num_known_tokens += 1

          # count operators
          if token.group == 'operator' or token.group == 'invalid operator':
            num_operators += 1
          if token.group == 'operator':
            num_known_operators += 1

          # collect keywords for counting
          if token.group == 'keyword':
            found_keywords.add(str(token))

    # count unique keywords and compare to number of tokens
    confidence_1 = 0
    num_keywords = len(found_keywords)
    if num_keywords > 10:
      confidence_1 = 1

    #  unknown tokens reduce confidence
    confidence_2 = 1
    if num_tokens > 0:
      confidence_2 = num_known_tokens / num_tokens

    #  unknown operators reduce confidence
    confidence_3 = 1
    if num_operators > 0:
      confidence_3 = num_known_operators / num_operators

    # compute confidence
    self.confidence = confidence_1 * confidence_2
