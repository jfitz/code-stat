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

class AssemblyZ80Examiner(Examiner):
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
    hex_integer_1_tb = PrefixedIntegerTokenBuilder('&', False, '0123456789abcdefABCDEF')
    hex_integer_2_tb = SuffixedIntegerTokenBuilder('h', False, '0123456789abcdefABCDEF')
    hex_integer_3_tb = PrefixedIntegerTokenBuilder('$', False, '0123456789abcdefABCDEF')
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
      'ASEG', 'DB', 'DEFB', 'DEFS', 'DEFW', 'DW', 'DS',
      'END', 'EQU', 'ORG', 'PAGE'
    ]

    directive_tb = CaseInsensitiveListTokenBuilder(directives, 'directive', False)

    title_directive_tb = LeadToEndOfLineTokenBuilder('TITLE', False, 'directive')
    subtitle_directive_tb = LeadToEndOfLineTokenBuilder('SUBTTL', False, 'directive')
    include_directive_tb = LeadToEndOfLineTokenBuilder('INCLUDE', False, 'directive')

    opcodes = [
      'ADC', 'ADD', 'AND',
      'BIT',
      'CALL', 'CCF', 'CP', 'CPD', 'CPDR', 'CPI', 'CPIR', 'CPL',
      'DAA', 'DEC', 'DI', 'DJNZ',
      'EI', 'EX', 'EXX',
      'HALT',
      'IM', 'IN', 'INC', 'IND', 'INDR', 'INI', 'INIR',
      'JP', 'JR',
      'LD', 'LDD', 'LDDR', 'LDI', 'LDIR',
      'NEG', 'NOP',
      'OR', 'OTDR', 'OTIR', 'OUT', 'OUTD', 'OUTI',
      'POP', 'PUSH',
      'RES', 'RET', 'RETI', 'RETN', 'RL', 'RLA', 'RLC', 'RLCA', 'RLD',
      'RR', 'RRA', 'RRC', 'RRCA', 'RRD', 'RST',
      'SBC', 'SCF', 'SET', 'SLA', 'SRA', 'SRL', 'SUB',
      'XOR'
    ]

    opcode_tb = CaseInsensitiveListTokenBuilder(opcodes, 'keyword', False)

    registers = [
      'A', 'B', 'C', 'D', 'E', 'H', 'L', 'M', 'PSW', 'F',
      'BC', 'DE', 'HL', 'IX', 'IY'
      ]

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
      hex_integer_1_tb,
      hex_integer_2_tb,
      hex_integer_3_tb,
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

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = AssemblyZ80Examiner.convert_identifiers_to_labels(tokens)
    self.tokens = tokens

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence([])

    num_operators = self.count_my_tokens(['operator'])
    if num_operators > 0:
      self.calc_operator_confidence()
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, allow_pairs)
      self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    # self.calc_paired_blockers_confidence(['{'], ['}'])


  # convert leading identifiers to labels
  @staticmethod
  def convert_identifiers_to_labels(tokens):
    prev_token = Token('\n', 'newline', False)

    for token in tokens:
      if token.group == 'identifier' and prev_token.group == 'newline':
        token.group = 'label'

      prev_token = token
    
    return tokens


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
