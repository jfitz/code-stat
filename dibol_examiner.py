import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  EscapedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  PrefixedIntegerTokenBuilder,
  SuffixedIntegerTokenBuilder,
  SuffixedRealTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from examiner import Examiner

class DibolExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    EscapedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    SuffixedIntegerTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    SuffixedRealTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()
    self.newlines_important = 'always'

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("")
    operand_types.append('number')

    leads = '_$'
    extras = '_$'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    quotes = ['"', "'"]
    string_tb = EscapedStringTokenBuilder(quotes, 0)
    operand_types.append('string')

    comment_tb = LeadToEndOfLineTokenBuilder(';', False, 'comment')

    directives = [
      '.PROC', '.LIST', '.NOLIST', '.PAGE', '.INCLUDE',
      '.IFDEF', '.ENDC', '.IFNDEF', '.END'
    ]

    directive_tb = CaseInsensitiveListTokenBuilder(directives, 'directive', False)

    line_continuation_tb = SingleCharacterTokenBuilder('&', 'line continuation', False)
    title_tb = LeadToEndOfLineTokenBuilder('.TITLE', True, 'directive')

    known_operators = [
      '=', '+', '-', '*', '/',
      '.EQ.', '.NE.', '.LT.', '.LE.', '.GE.', '.GT.',
      '.NOT.', '.AND.', '.OR.', '.XOR.'
    ]

    self.unary_operators = [
      '.NOT.', '+', '-'
    ]

    self.postfix_operators = []

    groupers = ['(', ')', ',', '[', ']', ':']
    group_starts = ['(', '[', ',']
    group_ends = [')', ']']
    group_mids = [',', ':']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'ACCEPT',
      'BEGIN', 'BY',
      'CALL', 'CLEAR', 'CLOSE', 'COMMON',
      'DELETE', 'DISPLAY', 'DO',
      'ELSE', 'END', 'ENDUSING',
      'FOR',
      'GOTO',
      'IF', 'INCR',
      'OPEN',
      'PROC',
      'READ', 'READS', 'RECORD', 'RETURN',
      'SELECT', 'STORE', 'SUBROUTINE',
      'THEN', 'THRU',
      'UNTIL', 'USING',
      'WHILE', 'WRITE', 'WRITES',
      'XCALL'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      integer_tb,
      keyword_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      comment_tb,
      string_tb,
      directive_tb,
      title_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['statement terminator', 'newline'], ['{'], ['whitespace', 'comment'])

    self.tokens = tokens
    self.convert_identifiers_after_goto_to_labels()
    # self.convert_identifiers_to_functions()
    # self.convert_functions_to_common_functions(functions)

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence([])
    self.calc_line_continuation_confidence(tokens)

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.no_keyword_confidence = 0.2
    self.calc_keyword_confidence()

    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_length_confidence(code, self.max_expected_line)


  # convert identifiers after 'goto' to labels
  def convert_identifiers_after_goto_to_labels(self):
    prev_2_token = Token('\n', 'newline', False)
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'group' and token.text == ',' and \
        prev_token.group == 'identifier' and \
        prev_2_token.group == 'newline':
        prev_token.group = 'label'
        prev_token.is_operand = False

      if token.group not in ['whitespace']:
        prev_2_token = prev_token
        prev_token = token
