import string
from Examiner import Examiner
from TokenBuilders import *
from CobolTokenBuilders import *
from Tokenizer import Tokenizer

class CobolExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    lines = code.split('\n')

    num_tokens = 0
    num_known_tokens = 0
    num_operators = 0
    num_known_operators = 0

    wtb = WhitespaceTokenBuilder()

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

    unknown_operators = set(self.common_operators()) - set(known_operators)
    uotb = ListTokenBuilder(unknown_operators, 'invalid operator')

    nltb = NewlineTokenBuilder()

    keywords = [
      'ACCEPT',
      'ADVANCING',
      'AFTER',
      'ARE',
      'ASCENDING',
      'ASSIGN',
      'AT',
      'AUTHOR',
      'CHARACTERS',
      'CLOSE',
      'CONTAINS',
      'DATA',
      'DATE',
      'DESCENDING',
      'DISK',
      'DISPLAY',
      'DIVISION',
      'ELSE',
      'END',
      'END-IF',
      'END-PERFORM',
      'END-READ',
      'ENVIRONMENT',
      'FD',
      'FILE-CONTROL',
      'FILE',
      'FILLER',
      'GREATER',
      'ID',
      'IDENTIFICATION',
      'IF',
      'INPUT',
      'INPUT-OUTPUT',
      'INSTALLATION',
      'IS',
      'KEY',
      'LESS',
      'LINE',
      'LINES',
      'MOVE',
      'NOT',
      'OMITTED',
      'ON',
      'OPEN',
      'ORGANIZATION',
      'OUTPUT',
      'PERFORM',
      'PROCEDURE',
      'PROGRAM-ID',
      'PIC',
      'PICTURE',
      'PRINTER',
      'READ',
      'RECORD',
      'RETURN',
      'RUN',
      'SECTION',
      'SECURITY',
      'SELECT',
      'SEQUENTIAL',
      'SORT',
      'SPACES',
      'STOP',
      'TAPE',
      'THAN',
      'TO',
      'UNTIL',
      'VALUE',
      'VALUE',
      'VALUES',
      'WRITTEN',
      'WORKING-STORAGE',
      'WRITE'
      ]

    ktb = ListTokenBuilder(keywords, 'keyword')
    
    tokenbuilders = [wtb, nltb, ntb, ktb, itb, stb, ptb, kotb, uotb]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    found_keywords = set()
    
    self.tokens = []
    for line in lines:
      line = line.rstrip()
      
      if not line.startswith('      *'):
        tokens = tokenizer.tokenize(line)
        tokens.append(Token('\n', 'newline'))
        self.tokens += tokens
        
        for token in tokens:
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
    keyword_confidence = 0
    num_keywords = len(found_keywords)
    if num_keywords > 10:
      keyword_confidence = 1

    #  unknown tokens reduce confidence
    token_confidence = 1
    if num_tokens > 0:
      token_confidence = num_known_tokens / num_tokens

    #  unknown operators reduce confidence
    operator_confidence = 1
    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    # compute confidence
    self.confidence = keyword_confidence * token_confidence * operator_confidence
    self.confidences = {
      'keyword': keyword_confidence,
      'token': token_confidence,
      'operator': operator_confidence
    }
