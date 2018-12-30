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

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(True)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False)
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"])
    raw_string_tb = PrefixedStringTokenBuilder('r', ['"', "'"])

    hash_comment_tb = HashCommentTokenBuilder()
    triple_quote_comment_tb = TripleQuoteCommentTokenBuilder()
    raw_triple_quote_comment_tb = RawTripleQuoteCommentTokenBuilder()

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

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    continuation_chars = ['\\']
    line_continuation_tb = ListTokenBuilder(continuation_chars, 'line continuation', False)

    keywords = [
      'import', 'from', 'as', 'def', 'class',
      'None', 'False', 'True',
      'if', 'else',
      'for', 'while'
      ]
    
    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    tokenbuilders = [
      whitespace_tb,
      line_continuation_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      newline_tb,
      keyword_tb,
      identifier_tb,
      string_tb,
      raw_string_tb,
      known_operator_tb,
      self.unknown_operator_tb,
      hash_comment_tb,
      triple_quote_comment_tb,
      raw_triple_quote_comment_tb
    ]
    
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

    # line format 1 - lines without indent start with keyword
    lines = self.split_tokens(self.tokens)
    num_lines = 0
    num_lines_correct = 0
    for line in lines:
      if len(line) > 0:
        num_lines += 1

        if line[0].group in ['keyword', 'whitespace', 'comment', 'string']:
          num_lines_correct += 1
    
    line_format_1_confidence = 0

    if num_lines > 0:
      line_format_1_confidence = num_lines_correct / num_lines

    # line format 2 - some keyword lines end in colon
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

    line_format_2_confidence = 1

    if num_lines > 0:
      line_format_2_confidence = num_lines_correct / num_lines

    # compute confidence
    self.confidence = line_format_1_confidence * line_format_2_confidence * token_confidence * operator_confidence
    self.confidences = {
      'line_format_1': line_format_1_confidence,
      'line_format_2': line_format_2_confidence,
      'keyword': keyword_confidence,
      'token': token_confidence,
      'operator': operator_confidence
      }
