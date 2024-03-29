import string
import math

from codestat_tokenizer import Tokenizer
from codestat_token import Token
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  EscapedStringTokenBuilder,
  PrefixedStringTokenBuilder,
  PrefixedRawStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  PrefixedIdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
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
    EscapedStringTokenBuilder.__escape_z__()
    PrefixedStringTokenBuilder.__escape_z__()
    PrefixedRawStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    PrefixedIdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    TripleQuoteStringTokenBuilder.__escape_z__()
    RawTripleQuoteCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()
    self.newlines_important = 'always'

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator', False)

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    real_tb = RealTokenBuilder(False, False, '_')
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', '_')
    operand_types.append('number')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    decorator_tb = PrefixedIdentifierTokenBuilder('@', 'decorator', False)

    quotes = ['"', "'"]
    string_tb = EscapedStringTokenBuilder(quotes, 0)
    raw_string_tb = PrefixedRawStringTokenBuilder('r', True, quotes)
    byte_string_tb = PrefixedStringTokenBuilder('b', True, quotes)
    unicode_string_tb = PrefixedStringTokenBuilder('u', True, quotes)
    fast_string_tb = PrefixedStringTokenBuilder('f', True, quotes)
    operand_types.append('string')

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

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-',
      'not', '~',
      '++', '--',
      '.', '*', '**'
    ]

    self.postfix_operators = [
      '++', '--', ':'
    ]

    self.adjective_operators = ['not']

    self.keyword_postfix = [':']

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_starts = ['(', '[', ',', '{']
    group_mids = [',']
    group_ends = [')', ']', '}']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    continuation_chars = ['\\']
    line_continuation_tb = CaseInsensitiveListTokenBuilder(continuation_chars, 'line continuation', False)

    keywords = [
      'as', 'assert',
      'break',
      'case', 'class', 'continue',
      'def', 'del',
      'elif', 'else', 'except',
      'finally', 'for', 'from',
      'global',
      'if', 'import',
      'lambda',
      'match',
      'nonlocal',
      'pass', 'print',
      'raise', 'return',
      'try',
      'while', 'while', 'with',
      'yield'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    functions = [
      'abs', 'aiter', 'all', 'any', 'anext', 'ascii',
      'bin', 'bool', 'breakpoint', 'bytearray', 'bytes',
      'callable', 'chr', 'classmethod', 'compile', 'complex',
      'delattr', 'dict', 'dir', 'divmod',
      'enumerate', 'eval', 'exec',
      'filter', 'float', 'format', 'frozenset',
      'getattr', 'globals',
      'hasattr', 'hash', 'help', 'hex',
      'id', 'input', 'int', 'isinstance', 'issubclass', 'iter',
      'len', 'list', 'locals',
      'map', 'max', 'memoryview', 'min',
      'next',
      'object', 'oct', 'open', 'ord',
      'pow', 'print', 'property',
      'range', 'repr', 'reversed', 'round',
      'set', 'setattr', 'slice', 'sorted', 'statismethod', 'str', 'sum', 'super'
      'tuple', 'type',
      'vars',
      'zip'
    ]

    values = [
      'False', 'None', 'True'
    ]

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

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

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    self.tokens = tokens
    self.convert_identifiers_to_functions()
    self.convert_functions_to_common_functions(functions)

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_parens_continued_lines(tokens)
    tokens = Examiner.join_operator_continued_lines(tokens, self.postfix_operators)

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_line_continuation_confidence(tokens)

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = [
        ['not', 'in']
      ]
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    self.calc_line_format_confidence()
    self.calc_line_length_confidence(code, self.max_expected_line)

    # self.calc_keyword_indent_confidence()


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
