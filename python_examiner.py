import string
import math

from codestat_tokenizer import Tokenizer
from codestat_token import Token
from token_builders import (
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
  PrefixedIdentifierTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadToEndOfLineTokenBuilder,
  TripleQuoteStringTokenBuilder
)
from python_token_builders import (
  RawTripleQuoteCommentTokenBuilder
)
from examiner import Examiner

class PythonExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StringTokenBuilder.__escape_z__()
    PrefixedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    PrefixedIdentifierTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    TripleQuoteStringTokenBuilder.__escape_z__()
    RawTripleQuoteCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()
    self.newlines_important = 'always'

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator')

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    real_tb = RealTokenBuilder(False, False, '_')
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', '_')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)

    decorator_tb = PrefixedIdentifierTokenBuilder('@', 'decorator')
    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, False)
    raw_string_tb = PrefixedStringTokenBuilder('r', True, quotes)
    byte_string_tb = PrefixedStringTokenBuilder('b', True, quotes)
    unicode_string_tb = PrefixedStringTokenBuilder('u', True, quotes)
    fast_string_tb = PrefixedStringTokenBuilder('f', True, quotes)
    triple_quote_comment_tb = TripleQuoteStringTokenBuilder(quotes)
    raw_triple_quote_comment_tb = RawTripleQuoteCommentTokenBuilder()
    hash_comment_tb = LeadToEndOfLineTokenBuilder('#', True, 'comment')

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
      '++', '--',
      '.'
    ]

    self.postfix_operators = [
      '++', '--', ':'
    ]

    self.adjective_operators = ['not']

    self.keyword_postfix = [':']

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_starts = ['(', '[', ',', '{']
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
      decorator_tb,
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
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    tokens = self.source_tokens()
    tokens = Examiner.join_parens_continued_lines(tokens)
    tokens = Examiner.join_operator_continued_lines(tokens, self.postfix_operators)

    operand_types = [
      'number',
      'string',
      'variable',
      'identifier',
      'function',
      'symbol',
      'regex',
      'type',
      'value',
      'picture'
    ]

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence(tokens)
    self.calc_operator_3_confidence(tokens, group_ends, operand_types)
    self.calc_operator_4_confidence(tokens, group_starts, operand_types)
    operand_types = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    self.calc_line_format_confidence()
    # self.calc_keyword_indent_confidence()


  # binary operators that precede non-operands reduce confidence
  def calc_operator_4_confidence(self, tokens, group_starts, operand_types):
    num_invalid_operators = Examiner.count_tokens(tokens, ['invalid operator'])
    num_known_operators = Examiner.count_tokens(tokens, ['operator'])
    num_operators = num_known_operators + num_invalid_operators

    operator_confidence_4 = 1.0

    if num_operators > 0:
      errors = 0
      prev_token = Token('\n', 'newline')

      lower_unary_operators = []
      for op in self.unary_operators:
        lower_unary_operators.append(op.lower())

      for token in tokens:
        prev_token_postfix_operator = prev_token.text.lower() in (op.lower() for op in self.postfix_operators)
  
        if prev_token.group == 'operator' and \
          not prev_token_postfix_operator and \
          token.group not in operand_types and \
          token.text.lower() not in lower_unary_operators and \
          token.text not in group_starts and \
          token.text != 'in' and prev_token.text != 'not':
          errors += 1
          self.errors.append({
            'TYPE': 'OPERATOR4',
            'FIRST': prev_token.text,
            'SECOND': token.text
            })

        prev_token = token

      operator_confidence_4 = 1.0 - errors / num_operators

    self.confidences['operator_4'] = operator_confidence_4


  def calc_keyword_indent_confidence(self):
    # lines without indent start with keyword
    lines = PythonExaminer.split_tokens_to_lines(self.tokens)
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


  @staticmethod
  def split_tokens_to_lines(tokens):
    lines = []

    line_tokens = []

    group_level = 0

    for token in tokens:
      if token.group == 'newline':
        if group_level == 0:
          if len(line_tokens) > 0:
            lines.append(line_tokens)
            line_tokens = []
      else:
        if token.group == 'group':
          if token.text in ['(', '[']:
            group_level += 1
          if token.text in [')', ']'] and group_level > 0:
            group_level -= 1

        line_tokens.append(token)
    
    if len(line_tokens) > 0:
      lines.append(line_tokens)

    return lines


  def calc_line_format_confidence(self):
    # certain keyword lines end in colon

    tokens = self.unwrap_code_lines(self.tokens)

    # drop tokens not used by interpreter
    drop_types = ['whitespace', 'comment']

    tokens = Examiner.drop_tokens(tokens, drop_types)

    # split into lines
    lines = self.split_tokens_to_lines(tokens)

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
