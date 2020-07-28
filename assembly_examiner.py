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
  LabelTokenBuilder,
  AssemblyCommentTokenBuilder,
  MultilineCommentTokenBuilder
)
from examiner import Examiner

class AssemblyExaminer(Examiner):
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
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    LabelTokenBuilder.__escape_z__()
    AssemblyCommentTokenBuilder.__escape_z__()
    MultilineCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, tab_size, processor):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    hex_integer_1_tb = PrefixedIntegerTokenBuilder('&', False, '0123456789abcdefABCDEF')
    hex_integer_2_tb = SuffixedIntegerTokenBuilder('h', False, '0123456789abcdefABCDEF')
    hex_integer_3_tb = PrefixedIntegerTokenBuilder('$', False, '0123456789abcdefABCDEF')
    hex_integer_4_tb = PrefixedIntegerTokenBuilder('#$', False, '0123456789abcdefABCDEF')
    operand_types.append('number')

    leads = '_.$@#'
    extras = '_.$@#'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    label_tb = LabelTokenBuilder(leads, extras, ':')

    quotes = ['"', "'", "’"]
    string_tb = StringTokenBuilder(quotes, 0)
    operand_types.append('string')

    known_operators = [
      '+', '-', '*', '/', '&', '|', '=', '??'
    ]

    self.unary_operators = [
      '+', '-', '??'
    ]

    self.postfix_operators = []

    groupers = ['(', ')', ',', '[', ']', '<', '>', ':']
    group_starts = ['(', '[', ',', '<']
    group_ends = [')', ']', '>']
    group_mids = [',', ':']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    preprocessors = [
      'if', 'else', 'endif', 'endc', 'macro', 'endm', 'error'
    ]

    preprocessor_tb = CaseInsensitiveListTokenBuilder(preprocessors, 'preprocessor', False)

    directives = [
      '.8080', '.8086', '.6800', '.6502',
      'ALIGN', 'ASEG', 'ASSUME',
      'BITS',
      '.CODE', 'CPU', 'CSEG',
      'DB', 'DEFB', 'DEFS', 'DEFW', 'DFB', 'DFW', 'DW', 'DS', 'DSEG',
      'EJECT', 'END', 'ENDS', 'EQU', 'EXTRN',
      'GLOBAL',
      'INS86', '%INCLUDE',
      '.MODEL',
      'NAM', 'NAME',
      'ORG',
      'PAGE', 'PROC', 'PUBLIC',
      '.RADIX', 'RESB', 'RESD',
      '.SALL', 'SECTION', 'SEGMENT', 'SSEG', 'START',
      '.XLIST'
    ]

    directive_tb = CaseInsensitiveListTokenBuilder(directives, 'directive', False)

    title_directive_tb = LeadToEndOfLineTokenBuilder('TITLE', False, 'directive')
    subtitle_directive_tb = LeadToEndOfLineTokenBuilder('SUBTTL', False, 'directive')
    include_directive_tb = LeadToEndOfLineTokenBuilder('INCLUDE', False, 'directive')

    comment_directive_tb = MultilineCommentTokenBuilder()

    opcodes = []

    opcodes_6502 = [
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

    if processor in ['6502']:
      opcodes += opcodes_6502

    opcodes_6800 = [
      'ABA', 'ADC', 'ADCA', 'ADCB', 'ADD', 'AND', 'ASL', 'ASR',
      'BCC', 'BCS', 'BEQ', 'BGE', 'BGT', 'BHI', 'BIT', 'BLE', 'BLS', 'BLT', 'BMI', 'BNE', 'BPL', 'BRA', 'BSR', 'BVC', 'BVS',
      'CBA', 'CLC', 'CLI', 'CLR', 'CLRA', 'CLRB', 'CLV', 'CMP', 'COM', 'CPX',
      'DAA', 'DEC', 'DES', 'DEX',
      'EOR', 'EORA', 'EROB',
      'INC', 'INS', 'INX',
      'JMP', 'JSR',
      'LDA', 'LDAA', 'LDAB', 'LDS', 'LDX', 'LSR',
      'NEG', 'NOP',
      'ORA',
      'PSH', 'PUL',
      'ROL', 'ROR', 'RTI', 'RTS',
      'SBA', 'SBC', 'SEC', 'SEI', 'SEV', 'STA', 'STAA', 'STAB', 'STS', 'STX', 'SUB', 'SWI',
      'TAB', 'TAP', 'TBA', 'TPA', 'TST', 'TSX', 'TXS',
      'WAI'
    ]

    if processor in ['6800']:
      opcodes += opcodes_6800

    opcodes_8080 = [
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

    if processor in ['8080']:
      opcodes += opcodes_8080

    opcodes_z80 = [
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

    if processor in ['z80']:
      opcodes += opcodes_z80

    opcodes_8086 = [
      'AAA', 'AAD', 'AAM', 'AAS', 'ADC', 'ADD', 'AND',
      'CALL', 'CBW', 'CLC', 'CLD', 'CLI', 'CMC', 'CMP', 'CMPS', 'CMPSB', 'CMPW', 'CMPXCHG', 'CWD',
      'DAA', 'DAS', 'DEC', 'DIV',
      'ESC',
      'FWAIT',
      'F2XM1', 'FABS', 'FADD', 'FADDP', 'FBLD', 'FBSTP', 'FCHS', 'FCLEX', 'FCOM', 'FCOMP',
      'FCOMPP', 'FCOS', 'FDECSTP', 'FDISI', 'FDIV', 'FDIVP', 'FDIVR', 'FDIVRP',
      'FENI', 'FFREE', 'FIADD', 'FICOM', 'FICOMP', 'FIDIV', 'FIDIVR', 'FILD',
      'FIMUL', 'FINCSTP', 'FINIT', 'FIST', 'FISTP', 'FISUB', 'FISUBR', 'FLD', 'FLD1',
      'FLDCW', 'FLDENV', 'FLDL2E', 'FLDL2T', 'FLDLG2', 'FLDLN2', 'FLDPI',
      'FLDZ', 'FMUL', 'FMULP', 'FNCLEX', 'FNDISI', 'FNENI', 'FNINIT', 'FNOP', 'FNSAVE',
      'FNSTCW', 'FNSTENV', 'FNSTSW', 'FPATAN', 'FPREM', 'FPREM1', 'FPTAN', 'FRNDINT',
      'FRSTOR', 'FSAVE', 'FSCALE', 'FSETPM', 'FSIN', 'FSINCOS', 'FSQRT', 'FST', 'FSTCW',
      'FSTENV', 'FSTP', 'FSTSW', 'FSUB', 'FSUBP', 'FSUBRP', 'FTST', 'FUCOM', 'FUCOMP',
      'FUCOMPP', 'FXAM', 'FXCH', 'FXTRACT', 'FYL2X', 'FYL2XP1',
      'HLT',
      'IDIV', 'IMUL', 'IN', 'INC', 'INT', 'INTO', 'INVD', 'IRET', 'IRETD',
      'JA', 'JAE', 'JB', 'JBE', 'JC', 'JCXZ', 'JE', 'JECXZ', 'JG', 'JGE', 'JL', 'JLE', 'JMP', 'JNA', 'JNAE', 'JNB', 'JNBE', 'JNC', 'JNE', 'JNG', 'JNGE', 'JNL', 'JNLE', 'JNO', 'JNP', 'JNS', 'JO', 'JP', 'JPE', 'JPO', 'JNZ', 'JS', 'JZ',
      'LAHF', 'LAR', 'LDS', 'LEA', 'LES', 'LOCK', 'LODS', 'LODSB', 'LODSW', 'LOOP', 'LOOPE', 'LOOPNE', 'LOOPNZ', 'LOOPZ',
      'MOV', 'MOVS', 'MOVSB', 'MOVSW', 'MUL',
      'NEG', 'NOP', 'NOT',
      'OR', 'OUT',
      'POP', 'POPF', 'POPFD', 'PUSH', 'PUSHF', 'PUSHFD',
      'RCL', 'RCR', 'REP', 'REPE', 'REPNE', 'REPNZ', 'REPZ', 'RET', 'RETF', 'ROL', 'ROR',
      'SAHF', 'SAL', 'SAR', 'SBB', 'SCAS', 'SCASB', 'SCASW', 'SHL', 'SHR', 'STC', 'STD', 'STI', 'STOS', 'STOSB', 'STOSW', 'SUB',
      'TEST',
      'WAIT', 'WBINVD',
      'XCHG', 'XLAT', 'XLATB', 'XOR',
    ]

    if processor in ['8086', '80186', '80286', '80386', '80486']:
      opcodes += opcodes_8086

    opcodes_80186 = [
      'BOUND',
      'ENTER',
      'INS',
      'LEAVE',
      'OUTS',
      'POPA', 'POPAD', 'PUSHA', 'PUSHAD'
    ]

    if processor in ['80186', '80286', '80386', '80486']:
      opcodes += opcodes_80186

    opcodes_80286 = [
      'ARPL',
      'CLTS',
      'LGDT', 'LIDT', 'LLDT', 'LMSW', 'LSL', 'LSS',
      'SGDT', 'SIDT', 'SLDT', 'SMSW', 'STR',
      'VERR', 'VERW'
    ]

    if processor in ['80286', '80386', '80486']:
      opcodes += opcodes_80286

    opcodes_80386 = [
      'BSF', 'BSR', 'BT', 'BTC', 'BTR', 'BTS',
      'CDQ', 'CWDE',
      'LFS', 'LGS', 'LSS',
      'MOVSX', 'MOVZX',
      'SETAE', 'SETB', 'SETC', 'SETNAE', 'SETNB', 'SETNE', 'SETNZ', 'SETG', 'SETGE', 'SETL', 'SETLE', 'SETNC', 'SETNG', 'SETNGE', 'SETNL', 'SETNLE', 'SETNO', 'SETNP', 'SETNS', 'SETE', 'SETO', 'SETP', 'SETPE', 'SETPO', 'SETS', 'SETZ',
      'SHLD', 'SHRD'
    ]

    if processor in ['80386', '80486']:
      opcodes += opcodes_80386

    opcodes_80486 = [
      'BSWAP',
      'INVPLG'
    ]

    if processor in ['80486']:
      opcodes += opcodes_80486

    opcode_tb = CaseInsensitiveListTokenBuilder(opcodes, 'keyword', False)

    registers = []

    registers_6502 = ['A', 'X', 'Y', 'P', 'S']

    if processor in ['6502']:
      registers += registers_6502

    registers_6800 = ['A', 'B', 'IX', 'PC', 'SP']

    if processor in ['6800']:
      registers += registers_6800

    registers_8080 = [
      'A', 'B', 'C', 'D', 'E', 'H', 'L', 'M', 'PSW', 'F'
      ]

    if processor in ['8080']:
      registers += registers_8080

    registers_z80 = [
      'A', 'B', 'C', 'D', 'E', 'H', 'L', 'M', 'PSW', 'F',
      'BC', 'DE', 'HL', 'IX', 'IY'
      ]

    if processor in ['z80']:
      registers += registers_z80

    registers_8086 = [
      'AL', 'AH', 'BL', 'BH', 'CL', 'CH', 'DL', 'DH',
      'AX', 'BX', 'CX', 'DX', 'CS', 'DS', 'SS', 'ES',
      'IP', 'SI', 'DI', 'BP', 'SP', 'FLAGS'
      ]

    if processor in ['8086', '80186', '80286', '80386', '80486']:
      registers += registers_8086

    registers_80286 = [
      'TR'
    ]

    if processor in ['80286', '80386', '80486']:
      registers += registers_80286

    registers_80386 = [
      'EAX', 'EBX', 'ECX', 'EDX', 'ESI', 'EDI', 'EBP', 'ESP',
      'FS', 'GS', 'EFLAGS'
    ]

    if processor in ['80386', '80486']:
      registers += registers_80386

    register_tb = CaseInsensitiveListTokenBuilder(registers, 'register', True)

    values = ['*', '$']

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    comment_tb = LeadToEndOfLineTokenBuilder(';', False, 'comment')
    comment_2_tb = AssemblyCommentTokenBuilder('*')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      integer_tb,
      integer_exponent_tb,
      hex_integer_1_tb,
      hex_integer_2_tb,
      hex_integer_3_tb,
      hex_integer_4_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      register_tb,
      opcode_tb,
      directive_tb,
      title_directive_tb,
      subtitle_directive_tb,
      include_directive_tb,
      comment_directive_tb,
      preprocessor_tb,
      identifier_tb,
      label_tb,
      string_tb,
      comment_tb,
      comment_2_tb,
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
    label_leads = '.&$@#'
    label_mids = '.&$#@'
    label_ends = ':,'
    comment_leads = '*;'
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
