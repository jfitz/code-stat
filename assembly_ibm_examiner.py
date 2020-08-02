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
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from assembly_token_builders import (
  IbmAsmIdentifierTokenBuilder,
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

    leads = '$#.@&'
    extras = '$#.@&'
    identifier_tb = IbmAsmIdentifierTokenBuilder(leads, extras)
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
    group_mids = [',']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    directives = [
      'ABEND',
      'CSECT',
      'DC', 'DROP', 'DS', 
      'EJECT', 'END', 'ENTRY', 'EQU', 'EXTRN',
      'FREEMAIN',
      'GETMAIN', 'GLOBAL',
      'MACRO', 'MEND',
      'NAM', 'NAME',
      'ORG',
      'PAGE', 'PARAM', 'PROC', 'PUBLIC',
      'RETURN',
      'STIMER',
      'TITLE', 'SUBTTL',
      'USING'
    ]

    directive_tb = CaseInsensitiveListTokenBuilder(directives, 'directive', False)

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
      'ST', 'STC', 'STD', 'STE', 'STH', 'STM', 'SU', 'SUR', 'SVC',
      'SW', 'SWR', 'SXR',
      'TCH', 'TIO', 'TM', 'TR', 'TRT', 'TS',
      'UNPK', 'UNPKU',
      'WRD',
      'X', 'XC', 'XI', 'XR',
      'ZAP'
    ]

    opcode_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    registers = [
      'R0', 'R1', 'R2', 'R3', 'R4', 'R5', 'R6', 'R7', 'R8', 'R9', 'R10',
      'R11', 'R12', 'R13', 'R14', 'R15',
      'FP0', 'FP2', 'FP4', 'FP6'
    ]

    register_tb = CaseInsensitiveListTokenBuilder(registers, 'register', True)

    values = ['*']

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    comment_tb = LeadToEndOfLineTokenBuilder('!', False, 'comment')
    line_comment_tb = AssemblyCommentTokenBuilder('*')

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
      register_tb,
      opcode_tb,
      directive_tb,
      include_directive_tb,
      identifier_tb,
      string_tb,
      hex_string_tb,
      char_string_tb,
      comment_tb,
      line_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    opcode_tokenbuilders = [
      whitespace_tb,
      opcode_tb,
      directive_tb,
      include_directive_tb,
      identifier_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    args_tokenbuilders = [
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
      register_tb,
      identifier_tb,
      string_tb,
      hex_string_tb,
      char_string_tb,
      comment_tb,
      line_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    opcode_tokenizer = Tokenizer(opcode_tokenbuilders)
    args_tokenizer = Tokenizer(args_tokenbuilders)

    # tokenize as free-format
    tokens1 = tokenizer.tokenize(code)
    tokens1 = Examiner.combine_adjacent_identical_tokens(tokens1, 'invalid operator')
    tokens1 = Examiner.combine_adjacent_identical_tokens(tokens1, 'invalid')
    tokens1 = AssemblyIBMExaminer.convert_keywords_to_identifiers(tokens1)
    self.tokens = tokens1
    self.convert_asm_identifiers_to_labels()

    self.calc_statistics()
    statistics1 = self.statistics
    self.statistics = {}

    self.calc_confidences(operand_types, group_starts, group_mids, group_ends, None)
    confidences1 = self.confidences
    self.confidences = {}
    errors1 = self.errors
    self.errors = []

    # tokenize as space-format
    opcode_extras = '.&=,()+-*/'
    label_leads = '.&$@'
    label_mids = '.&$#@'
    label_ends = ':,'
    comment_leads = '!'
    line_comment_leads = '*'
    use_line_id = True
    tokens2, indents = Tokenizer.tokenize_asm_code(code, tab_size, opcode_tokenizer, opcode_extras, args_tokenizer, label_leads, label_mids, label_ends, comment_leads, line_comment_leads, use_line_id)
    tokens2 = Examiner.combine_adjacent_identical_tokens(tokens2, 'invalid operator')
    tokens2 = Examiner.combine_adjacent_identical_tokens(tokens2, 'invalid')
    tokens2 = Examiner.combine_identifier_colon(tokens2, ['newline'], [], [])
    tokens2 = Tokenizer.combine_number_and_adjacent_identifier(tokens2)
    tokens2 = AssemblyIBMExaminer.convert_opcodes_to_keywords(tokens2, keywords)
    tokens2 = AssemblyIBMExaminer.convert_keywords_to_identifiers(tokens2)
    tokens2 = Examiner.convert_asterisks_to_operators(tokens2)
    self.tokens = tokens2
    self.convert_asm_identifiers_to_labels()

    self.calc_statistics()
    statistics2 = self.statistics
    self.statistics = {}

    self.calc_confidences(operand_types, group_starts, group_mids, group_ends, indents)
    confidences2 = self.confidences
    self.confidences = {}
    errors2 = self.errors
    self.errors = []

    # select the better of free-format and spaced-format

    confidence1 = 1.0
    for key in confidences1:
      factor = confidences1[key]
      confidence1 *= factor

    confidence2 = 1.0
    for key in confidences2:
      factor = confidences2[key]
      confidence2 *= factor

    if confidence2 > confidence1:
      self.tokens = tokens2
      self.statistics = statistics2
      self.confidences = confidences2
      self.errors = errors2
    else:
      self.tokens = tokens1
      self.statistics = statistics1
      self.confidences = confidences1
      self.errors = errors1


  # convert keywords in expressions to identifiers
  @staticmethod
  def convert_keywords_to_identifiers(tokens):
    prev_token = Token('\n', 'newline', False)

    groups = ['whitespace', 'newline']

    for token in tokens:
      if token.group == 'keyword' and prev_token.group not in groups:
        token.group = 'identifier'
        token.is_operand = True

      prev_token = token

    return tokens


  # convert opcodes to keywords
  @staticmethod
  def convert_opcodes_to_keywords(tokens, keywords):
    for token in tokens:
      if token.group == 'opcode' and token.text.upper() in keywords:
        token.group = 'keyword'

    return tokens


  def calc_confidences(self, operand_types, group_starts, group_mids, group_ends, indents):
    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence([','])

    num_operators = self.count_my_tokens(['operator'])
    if num_operators > 0:
      self.calc_operator_confidence()
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, allow_pairs)
      self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)

    # self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    # self.calc_paired_blockers_confidence(['{'], ['}'])

    if indents is not None:
      self.calc_indent_confidence(indents)

    self.calc_line_ident_confidence()
