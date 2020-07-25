import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  PrefixedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  PrefixedIntegerTokenBuilder,
  SuffixedIntegerTokenBuilder,
  RealTokenBuilder,
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from assembly_token_builders import (
  AssemblyCommentTokenBuilder
)
from examiner import Examiner

class AssemblyIBMExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StringTokenBuilder.__escape_z__()
    PrefixedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    SuffixedIntegerTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    AssemblyCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, tab_size):
    super().__init__()

    self.newlines_important = 'always'

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    real_tb = RealTokenBuilder(True, True, None)
    hex_integer_1_tb = PrefixedIntegerTokenBuilder('$', False, '0123456789abcdefABCDEF')
    hex_integer_2_tb = PrefixedIntegerTokenBuilder('#$', False, '0123456789abcdefABCDEF')
    hex_integer_3_tb = PrefixedIntegerTokenBuilder('&', False, '0123456789abcdefABCDEF')
    hex_integer_h_tb = SuffixedIntegerTokenBuilder(['h'], False, 'abcdefABCDEF')
    binary_integer_tb = PrefixedIntegerTokenBuilder('0b', False, '01')
    suffixed_integer_tb = SuffixedIntegerTokenBuilder(['Q', 'A', 'O', 'D', 'B'], False, None)
    operand_types.append('number')

    leads = '_$#.@'
    extras = '_$#.@'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, 0)
    hex_string_tb = PrefixedStringTokenBuilder('X', False, quotes)
    char_string_tb = PrefixedStringTokenBuilder('C', False, quotes)
    operand_types.append('string')

    known_operators = [
      '+', '-', '*', '/', '=', '&', '#', '?', "'"
    ]

    self.unary_operators = [
      '+', '-', '=', '&', '#', '?', "'"
    ]

    self.postfix_operators = []

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':', '<', '>']
    group_starts = ['(', '[', ',', '{', '<']
    group_ends = [')', ']', '}', '>']
    # group_mids = [',']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'A', 'AD', 'ADR', 'AE', 'AER', 'AH', 'AL', 'ALR', 'AP', 'AR',
      'AU', 'AUR', 'AW', 'AWR', 'AXR',
      'B', 'BAL', 'BALR', 'BAS', 'BASR', 'BC', 'BCR', 'BCT', 'BCTR',
      'BE', 'BH', 'BL', 'BM', 'BNE', 'BNH', 'BNL', 'BNM', 'BNP', 'BNO', 'BNZ',
      'BO', 'BP', 'BR', 'BXH', 'BXLE', 'BZ',
      'C', 'CD', 'CDR', 'CE', 'CER', 'CH', 'CL', 'CLC', 'CLI', 'CLR', 'CP',
      'CR', 'CVB', 'CVD',
      'D', 'DD', 'DDR', 'DE', 'DER', 'DIAGNOSE', 'DP', 'DR',
      'ED', 'EDMK', 'EX',
      'HDR', 'HER', 'HIO',
      'IC', 'ISK',
      'L',
      'LA', 'LCR', 'LCDR', 'LCER', 'LD', 'LDR',
      'LE', 'LER', 'LH', 'LM',
      'LNDR', 'LNER', 'LNR',
      'LPDR', 'LPER', 'LPR', 'LPSW', 'LR', 'LRDR', 'LRER',
      'LTDR', 'LTER', 'LTR',
      'M', 'MD', 'MDR', 'ME', 'MER', 'MH', 'MP', 'MR', 'MVC', 'MVI',
      'MVN', 'MVO', 'MVZ', 'MXD', 'MXDR', 'MXR',
      'N', 'NC', 'NI', 'NOP', 'NOPR', 'NR',
      'O', 'OC', 'OI', 'OR',
      'PACK',
      'RDD',
      'S', 'SD', 'SDR', 'SE', 'SER', 'SH', 'SIO',
      'SL', 'SLA', 'SLDA', 'SLDL', 'SLL', 'SLR',
      'SP', 'SPM',
      'SR', 'SRA', 'SRDL', 'SRP',
      'SSK', 'SSM', 'SRDA', 'SRL',
      'STC', 'STD', 'STE', 'STH', 'STM', 'SU', 'SUR', 'SVC', 'SW', 'SWR',
      'SXR',
      'TCH', 'TIO', 'TM', 'TR', 'TRT', 'TS',
      'UNPK', 'UNPKU',
      'WRD',
      'X', 'XC', 'XI', 'XR',
      'ZAP'
    ]

    # keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    # types = []

    # types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)

    values = ['*']

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    comment_tb = AssemblyCommentTokenBuilder(';*')

    title_directive_tb = LeadToEndOfLineTokenBuilder('TITLE', False, 'directive')
    subtitle_directive_tb = LeadToEndOfLineTokenBuilder('SUBTTL', False, 'directive')
    include_directive_tb = LeadToEndOfLineTokenBuilder('INCLUDE', False, 'directive')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      integer_tb,
      integer_exponent_tb,
      hex_integer_1_tb,
      hex_integer_2_tb,
      hex_integer_3_tb,
      hex_integer_h_tb,
      binary_integer_tb,
      suffixed_integer_tb,
      real_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      string_tb,
      hex_string_tb,
      char_string_tb,
      title_directive_tb,
      subtitle_directive_tb,
      include_directive_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    # get tokens and indents
    opcode_extras = '.&=,()+-*/'
    label_leads = '.&$@'
    label_mids = '.&$#@'
    label_ends = ':,'
    comment_leads = '*;!'
    tokens, indents = Tokenizer.tokenize_asm_code(code, tab_size, tokenizer, opcode_extras, label_leads, label_mids, label_ends, comment_leads)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['newline'], [], [])
    tokens = Tokenizer.combine_number_and_adjacent_identifier(tokens)
    tokens = AssemblyIBMExaminer.convert_opcodes_to_keywords(tokens, keywords)
    tokens = AssemblyIBMExaminer.convert_asterisks_to_operators(tokens)
    self.tokens = tokens
    self.convert_identifiers_to_labels()

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence([','])

    self.calc_operator_confidence()
    allow_pairs = []
    self.calc_operator_2_confidence(tokens, allow_pairs)
    self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
    self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)

    # self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    # self.calc_keyword_confidence()

    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_indent_confidence(indents)


  # convert leading identifiers to labels
  @staticmethod
  def convert_opcodes_to_keywords(tokens, keywords):
    for token in tokens:
      if token.group == 'opcode' and token.text.upper() in keywords:
        token.group = 'keyword'

    return tokens


  # convert leading identifiers to labels
  @staticmethod
  def convert_asterisks_to_operators(tokens):
    prev_token = Token('\n', 'newline', False)

    for token in tokens:
      if token.group == 'value' and prev_token.group in ['number', 'identifier', 'value']:
        token.group = 'operator'

      if token.group not in ['whitespace']:
        prev_token = token

    return tokens


  def calc_indent_confidence(self, indents):
    opcode_indents = {}
    for indent_dict in indents:
      opcode = indent_dict['opcode']
      if opcode is not None:
        if opcode in opcode_indents:
          opcode_indents[opcode] += 1
        else:
          opcode_indents[opcode] = 1

    total = 0
    highest = 0

    for key in opcode_indents:
      indent = opcode_indents[key]
      total += indent
      if indent > highest:
        highest = indent

    if total > 0:
      confidence = highest / total
    else:
      confidence = 0.0
    
    self.confidences['opcode_indent'] = confidence
