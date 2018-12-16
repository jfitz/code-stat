import string
import math
from Examiner import Examiner
from TokenBuilders import *
from PythonTokenBuilders import *
from PythonTokenBuilders import *
from Tokenizer import Tokenizer

class PythonExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    wtb = WhitespaceTokenBuilder()
    nltb = NewlineTokenBuilder()

    ntb = NumberTokenBuilder()
    itb = IdentifierTokenBuilder()
    stb = StringTokenBuilder(['"', "'"])

    hctb = HashCommentTokenBuilder()
    ctb = CommentTokenBuilder()

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '&', '|', '~', '<<', '>>',
      '**',
      '(', ')', ',', '.', ':', ';',
      '[', ']',
      '++', '--', 'and', 'or', 'in', 'is'
      ]

    kotb = ListTokenBuilder(known_operators, 'operator', True)

    unknown_operators = set(self.common_operators()) - set(known_operators)
    uotb = ListTokenBuilder(unknown_operators, 'invalid operator', True)

    continuation_chars = ['\\']
    lctb = ListTokenBuilder(continuation_chars, 'line continuation', False)
    nltb = NewlineTokenBuilder()

    keywords = [
      'import', 'from', 'as', 'def', 'class',
      'None', 'False', 'True',
      'if', 'else',
      'for', 'while'
      ]
    
    ktb = ListTokenBuilder(keywords, 'keyword', True)

    tokenbuilders = [wtb, lctb, nltb, ntb, ktb, itb, stb, kotb, uotb, hctb, ctb]
    
    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = tokenizer.tokenize(code)

    found_keywords = self.find_keywords(self.tokens)

    num_known_tokens = self.count_valid_tokens(self.tokens)
    num_invalid_operators = self.count_invalid_operators(self.tokens)
    num_known_operators = self.count_known_operators(self.tokens)

    # recognized keywords improve confidence
    keyword_confidence = len(found_keywords) / len(keywords)

    # unknown tokens reduce confidence
    token_confidence = 1
    if len(self.tokens) > 0:
      token_confidence = num_known_tokens / len(self.tokens)

    # unknown operators reduce confidence
    operator_confidence = 1
    num_operators = num_known_operators + num_invalid_operators
    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    # line format
    tokens = self.drop_whitespace(self.tokens)
    tokens = self.drop_comments(tokens)
    lines = self.split_tokens(tokens)
    num_lines = 0
    num_lines_correct = 0
    python_keywords = ['class', 'def', 'for', 'while', 'else']

    for line in lines:
      if len(line) > 1:
        first_token = line[0]
        last_token = line[-1]

        if first_token.group == 'keyword' and first_token.text in (keyword.lower() for keyword in python_keywords):
          num_lines += 1

          if last_token.group == 'operator' and last_token.text == ':':
            num_lines_correct += 1

    line_format_confidence = 1

    if num_lines > 0:
      line_format_confidence = num_lines_correct / num_lines

    # compute confidence
    self.confidence = line_format_confidence * token_confidence * operator_confidence
    self.confidences = {
      'line_format': line_format_confidence,
      'keyword': keyword_confidence,
      'token': token_confidence,
      'operator': operator_confidence
      }
