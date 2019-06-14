import string
import math
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  PrefixedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadCommentTokenBuilder,
  TripleQuoteStringTokenBuilder
)
from PythonTokenBuilders import (
  RawTripleQuoteCommentTokenBuilder
)
from Tokenizer import Tokenizer

class PythonExaminer(Examiner):
  def __init__(self, code):
    super().__init__()
    self.newlines_important = 'always'

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator')

    integer_tb = IntegerTokenBuilder(True)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False, False)
    raw_string_tb = PrefixedStringTokenBuilder('r', True, ['"', "'"])
    byte_string_tb = PrefixedStringTokenBuilder('b', True, ['"', "'"])
    unicode_string_tb = PrefixedStringTokenBuilder('u', True, ['"', "'"])
    fast_string_tb = PrefixedStringTokenBuilder('f', True, ['"', "'"])

    hash_comment_tb = LeadCommentTokenBuilder('#')
    triple_quote_comment_tb = TripleQuoteStringTokenBuilder()
    raw_triple_quote_comment_tb = RawTripleQuoteCommentTokenBuilder()

    known_operators = [
      '+', '-', '*', '/', '%', '@',
      '=', ':=',
      '==', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '&', '|', '~', '<<', '>>',
      '**',
      '.', ':',
      '++', '--', 'and', 'or', 'in', 'is', 'not'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-',
      'not', '~',
      '++', '--'
    ]

    self.postfix_operators = [
      '++', '--', ':'
    ]

    self.adjective_operators = ['not']

    self.keyword_postfix = [':']

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    continuation_chars = ['\\']
    line_continuation_tb = ListTokenBuilder(continuation_chars, 'line continuation', False)

    keywords = [
      'import', 'from', 'as', 'def', 'class',
      'if', 'elif', 'else', 'pass', 'while',
      'for', 'while', 'return', 'assert', 'raise',
      'break', 'continue', 'del',
      'try', 'except', 'finally',
      'global', 'lambda', 'nonlocal', 'with', 'yield',
      'print'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    values = [
      'False', 'None', 'True'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      values_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      string_tb,
      raw_string_tb,
      byte_string_tb,
      unicode_string_tb,
      fast_string_tb,
      hash_comment_tb,
      triple_quote_comment_tb,
      raw_triple_quote_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_line_format_confidence()
    # self.calc_keyword_indent_confidence()


  def calc_keyword_indent_confidence(self):
    # lines without indent start with keyword
    lines = self.split_tokens(self.tokens)
    num_lines = 0
    num_lines_correct = 0
    for line in lines:
      if len(line) > 0:
        num_lines += 1

        if line[0].group in ['keyword', 'whitespace', 'comment', 'string']:
          num_lines_correct += 1
        else:
          self.errors.append({
            'TYPE': 'MISSING INDENT',
            'KEYWORD': line[0].text
          })
    
    keyword_indent_confidence = 0.0

    if num_lines > 0:
      keyword_indent_confidence = num_lines_correct / num_lines

    self.confidences['keyword indent'] = keyword_indent_confidence


  def calc_line_format_confidence(self):
    # certain keyword lines end in colon

    tokens = self.unwrap_lines(self.tokens)

    # drop tokens not used by interpreter
    drop_types = ['whitespace', 'comment']

    tokens = self.drop_tokens(tokens, drop_types)

    # split into lines
    lines = self.split_tokens(tokens)

    # check certain lines end in colon
    num_lines = 0
    num_lines_correct = 0
    colon_keywords = [
      'class', 'def', 'for', 'while', 'if', 'else', 'elif'
    ]

    for line in lines:
      if len(line) > 1:
        first_token = line[0]
        last_token = line[-1]

        if first_token.group == 'keyword' and first_token.text in colon_keywords:
          num_lines += 1

          if last_token.group == 'operator' and last_token.text == ':':
            num_lines_correct += 1
          else:
            self.errors.append({
              'TYPE': 'LINE FORMAT',
              'FIRST': first_token.text,
              'SECOND': "END '" + last_token.text + "' NOT ':'"
            })

    line_format_2_confidence = 1.0

    if num_lines > 0:
      line_format_2_confidence = num_lines_correct / num_lines

    self.confidences['line format'] = line_format_2_confidence
    self.calc_statistics()
