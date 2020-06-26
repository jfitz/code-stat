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

class Assembly6502Examiner(Examiner):
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

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    hex_integer_1_tb = PrefixedIntegerTokenBuilder('$', False, '0123456789abcdefABCDEF')
    hex_integer_2_tb = PrefixedIntegerTokenBuilder('#$', False, '0123456789abcdefABCDEF')
    operand_types.append('number')

    leads = '_$#.'
    extras = '_$#.'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, 0)
    operand_types.append('string')

    known_operators = [
      '+', '-', '*', '>'
    ]

    self.unary_operators = [
      '+', '-', '>'
    ]

    self.postfix_operators = []

    groupers = ['(', ')', ',', '[', ']', ':']
    group_starts = ['(', '[', ',']
    group_ends = [')', ']']
    group_mids = [',']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    opcodes = [
      'DFB', 'DFW', 'EQU', 'ORG',
      'ADC', 'AND', 'ASL', 'AST',
      'BCC', 'BCS', 'BEQ', 'BIT', 'BMI', 'BNE',	'BPL', 'BRK', 'BVC', 'BVS',
      'CLC', 'CLD', 'CLI', 'CLV', 'CMP', 'CPR', 'CPX', 'CPY',
      'DEC', 'DEX', 'DEY',
      'EOR',
      'INC', 'INX', 'INY',
      'JMP', 'JSR',
      'LDA', 'LDX', 'LDY', 'LSR',
      'NOP',
      'ORA',
      'PHA', 'PHP', 'PLA', 'PLP',
      'ROL', 'ROR', 'RTI', 'RTS',
      'SBC', 'SEC', 'SED', 'SEI', 'STA', 'STX', 'STY',
      'TAX', 'TAY', 'TSX', 'TXA', 'TXS', 'TYA'
    ]

    # keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    # types = []

    # types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)

    values = ['*']

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    comment_tb = AssemblyCommentTokenBuilder(';*')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      integer_tb,
      integer_exponent_tb,
      hex_integer_1_tb,
      hex_integer_2_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      string_tb,
      comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    # get tokens and indents
    opcode_extras = '.'
    label_leads = '.&'
    label_mids = '.&'
    label_ends = ':,'
    comment_leads = '*;!'
    tokens, indents = self.tokenize_asm_code(code, tab_size, tokenizer, opcode_extras, label_leads, label_mids, label_ends, comment_leads)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    self.tokens = tokens
    self.convert_identifiers_to_labels()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence([])
    self.calc_operator_confidence()
    allow_pairs = []
    self.calc_operator_2_confidence(tokens, allow_pairs)
    self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
    self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)
    self.calc_group_confidence(tokens, group_mids)
    operand_types_2 = ['number']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)
    self.calc_opcode_confidence(opcodes)
    # self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
    self.calc_indent_confidence(indents)
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

    confidence = highest / total
    
    self.confidences['opcode_indent'] = confidence


  def calc_opcode_confidence(self, opcodes):
    confidence = 0

    groups = ['opcode']
    num_opcodes = self.count_my_tokens(groups)

    if num_opcodes > 0:
      count = 0
      for token in self.tokens:
        if token.group == 'opcode':
          if token.text.upper() in opcodes:
            count += 1
          else:
            self.errors.append({
              'TYPE': 'OPCODE',
              'INVALID': token.text,
              'FIRST': '',
              'SECOND': ''
            })

      confidence = count / num_opcodes

    self.confidences['opcode'] = confidence
