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
  SuffixedIdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from examiner import Examiner

class Assembly8080Examiner(Examiner):
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
    IdentifierTokenBuilder.__escape_z__()
    SuffixedIdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, tab_size):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    hex_integer_tb = SuffixedIntegerTokenBuilder('h', False, '0123456789abcdefABCDEF')
    operand_types.append('number')

    leads = '_.'
    extras = '_.'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    label_tb = SuffixedIdentifierTokenBuilder(leads, extras, ':')

    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, 0)
    operand_types.append('string')

    known_operators = [
      '+', '-', '*', '/', '&', '|', '='
    ]

    self.unary_operators = [
      '+', '-'
    ]

    self.postfix_operators = []

    groupers = ['(', ')', ',', '[', ']', '<', '>']
    group_starts = ['(', '[', ',', '<']
    group_ends = [')', ']', '>']
    group_mids = [',']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    preprocessors = [
      'if', 'else', 'endif', 'macro', 'endm', 'error'
    ]

    preprocessor_tb = CaseInsensitiveListTokenBuilder(preprocessors, 'preprocessor', False)

    directives = [
      '.8080', 'ASEG', 'DB', 'DW', 'DS', 'END', 'EQU', 'ORG', 'PAGE'
    ]

    title_directive_tb = LeadToEndOfLineTokenBuilder('TITLE', False, 'directive')
    subtitle_directive_tb = LeadToEndOfLineTokenBuilder('SUBTTL', False, 'directive')
    include_directive_tb = LeadToEndOfLineTokenBuilder('INCLUDE', False, 'directive')

    directive_tb = CaseInsensitiveListTokenBuilder(directives, 'directive', False)

    title_directive_tb = LeadToEndOfLineTokenBuilder('TITLE', False, 'directive')
    subtitle_directive_tb = LeadToEndOfLineTokenBuilder('SUBTTL', False, 'directive')
    include_directive_tb = LeadToEndOfLineTokenBuilder('INCLUDE', False, 'directive')

    opcodes = [
      'ACI', 'ADC', 'ADD', 'ADI', 'ANA', 'ANI',
      'CALL', 'CC', 'CM', 'CMA', 'CMC', 'CMP', 'CNC', 'CNZ', 'CP', 'CPE', 'CPI',
      'CPO', 'CZ',
      'DAA', 'DAD', 'DCR', 'DCX', 'DI',
      'EI',
      'HLT',
      'IN', 'INR', 'INX',
      'JC', 'JM', 'JMP', 'JNC', 'JNZ', 'JP', 'JPE', 'JPO', 'JZ',
      'LDAX', 'LHLD', 'LXI',
      'MOV', 'MVI',
      'NOP',
      'ORA', 'ORI', 'OUT',
      'PCHL', 'POP', 'PUSH',
      'RAL', 'RAR', 'RC', 'RIM', 'RLC', 'RET', 'RM', 'RNC', 'RNZ', 'RP', 'RPE',
      'RPO', 'RRC', 'RST', 'RZ	',
      'SBB', 'SBI', 'SHLD', 'SIM', 'SPHL', 'STA', 'STC', 'STAX', 'SUB', 'SUI',
      'XCHG', 'XRA', 'XRI', 'XTHL',
    ]

    opcode_tb = CaseInsensitiveListTokenBuilder(opcodes, 'keyword', False)

    registers = ['A', 'B', 'C', 'D', 'E', 'H', 'L', 'M', 'PSW', 'F']

    register_tb = CaseSensitiveListTokenBuilder(registers, 'register', True)

    values = ['*', '$']

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    comment_tb = LeadToEndOfLineTokenBuilder(';', False, 'comment')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      integer_tb,
      integer_exponent_tb,
      hex_integer_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      register_tb,
      opcode_tb,
      directive_tb,
      title_directive_tb,
      subtitle_directive_tb,
      include_directive_tb,
      preprocessor_tb,
      identifier_tb,
      label_tb,
      string_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    # tokenize as free-format
    tokenizer = Tokenizer(tokenbuilders)

    tokens1 = tokenizer.tokenize(code)
    tokens1 = Examiner.combine_adjacent_identical_tokens(tokens1, 'invalid operator')
    tokens1 = Examiner.combine_adjacent_identical_tokens(tokens1, 'invalid')
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
    comment_leads = '*;!'
    tokens2, indents = Tokenizer.tokenize_asm_code(code, tab_size, tokenizer, opcode_extras, label_leads, label_mids, label_ends, comment_leads)
    tokens2 = Examiner.combine_adjacent_identical_tokens(tokens2, 'invalid operator')
    tokens2 = Examiner.combine_adjacent_identical_tokens(tokens2, 'invalid')
    tokens2 = Examiner.combine_identifier_colon(tokens2, ['newline'], [], [])
    tokens2 = Tokenizer.combine_number_and_adjacent_identifier(tokens2)
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


  # combine numbers followed by identfiers to identifiers
  @staticmethod
  def combine_number_and_adjacent_identifier(tokens):
    new_list = []

    new_token = None
  
    for token in tokens:
      if token.group == 'identifier' and \
        new_token is not None and new_token.group == 'number':
        new_token = Token(new_token.text + token.text, 'identifier', True)
      else:
        if new_token is not None:
          new_list.append(new_token)
        new_token = token

    if new_token is not None:
      new_list.append(new_token)

    return new_list
