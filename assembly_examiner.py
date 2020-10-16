import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  NullTokenBuilder,
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
  LeadToEndOfLineTokenBuilder,
  SingleCharacterTokenBuilder
)
from assembly_token_builders import (
  LabelTokenBuilder,
  AssemblyCommentTokenBuilder,
  MultilineCommentTokenBuilder,
  HashQuoteCharTokenBuilder
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
    SingleCharacterTokenBuilder.__escape_z__()
    LabelTokenBuilder.__escape_z__()
    AssemblyCommentTokenBuilder.__escape_z__()
    MultilineCommentTokenBuilder.__escape_z__()
    HashQuoteCharTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, tab_size, processor):
    super().__init__()

    self.newlines_important = 'always'

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    comment_tb = LeadToEndOfLineTokenBuilder(';', True, 'comment')

    if processor in ['pdp-8']:
      comment_tb = LeadToEndOfLineTokenBuilder('/', True, 'comment')

    comment_2_tb = NullTokenBuilder()

    if processor in ['1802']:
      comment_2_tb = LeadToEndOfLineTokenBuilder('..', True, 'comment')

    line_comment_tb = AssemblyCommentTokenBuilder('*')

    stmt_separator_tb = NullTokenBuilder()

    if processor in ['pdp-8']:
      stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator', False)

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    integer_1_tb = NullTokenBuilder()
    integer_2_tb = NullTokenBuilder()
    prefixed_integer_tb = PrefixedIntegerTokenBuilder('#', True, '0123456789')
    if processor in ['pdp-11']:
      integer_1_tb = SuffixedIntegerTokenBuilder('$', True, '0123456789')
    if processor in ['z80']:
      integer_1_tb = SuffixedIntegerTokenBuilder('O', True, '0123456789')
      integer_2_tb = SuffixedIntegerTokenBuilder('D', True, '0123456789')

    hex_integer_1_tb = PrefixedIntegerTokenBuilder('&', True, '0123456789abcdefABCDEF')
    hex_integer_2_tb = SuffixedIntegerTokenBuilder('h', False, '0123456789abcdefABCDEF')
    hex_integer_3_tb = PrefixedIntegerTokenBuilder('$', True, '0123456789abcdefABCDEF')
    hex_integer_4_tb = PrefixedIntegerTokenBuilder('#$', True, '0123456789abcdefABCDEF')

    hash_quote_value_tb = NullTokenBuilder()

    if processor in ['pdp-11']:
      hash_quote_value_tb = HashQuoteCharTokenBuilder()

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
      '+', '-', '*', '/', '&', '|', '=', '??', '#', '@', "'", '!'
    ]

    self.unary_operators = [
      '+', '-', '??', '#', '@', "'"
    ]

    self.postfix_operators = ['+']

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
      '.8080', '.8086', '.6800', '.6502', ".386",
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
      'RESB', 'RESD',
      'SECTION', 'SEGMENT', 'SSEG', 'START',
      '.ASCII', '.ASCIZ', '.ASECT',
      '.BLKB;', '.BLKW', '.BYTE',
      '.CSECT',
      '.ENABLE', 'DSABLE', '.EVEN', '.ODD', '.END', '.EOT', '.ERROR',
      '.FLT2', '.FLT4',
      '.GLOBL',
      '.IDENT', '.IF', '.ENDC', '.IFF', '.IFT', '.IFTF', '.IRP', '.IRPC',
      '.LIMIT', '.LIST', '.NLST', '.NLIST',
      '.MACRO', '.ENDM', '.MEXIT', '.MCALL',
      '.NARG', '.NCHR', '.NTYPE', 
      '.PAGE', '.PRINT', '.PSECT',
      '.RAD50', '.RADIX', '.REPT', '.ENDR', '.RESTORE',
      '.SALL',
      '.WORD',
      '.XLIST'
    ]

    directive_tb = CaseInsensitiveListTokenBuilder(directives, 'directive', False)

    title_directive_tb = LeadToEndOfLineTokenBuilder('TITLE', False, 'directive')
    title_directive_2_tb = LeadToEndOfLineTokenBuilder('.TITLE', False, 'directive')
    subtitle_directive_tb = LeadToEndOfLineTokenBuilder('SUBTTL', False, 'directive')
    subtitle_directive_2_tb = LeadToEndOfLineTokenBuilder('.SUBTTL', False, 'directive')
    subtitle_directive_3_tb = LeadToEndOfLineTokenBuilder('.SBTTL', False, 'directive')
    include_directive_tb = LeadToEndOfLineTokenBuilder('INCLUDE', False, 'directive')
    include_directive_2_tb = LeadToEndOfLineTokenBuilder('.INCLUDE', False, 'directive')

    comment_directive_tb = MultilineCommentTokenBuilder()

    opcodes_1802 = [
      'IDL', 'LDN', 'INC', 'DEC', 'BR', 'BO', 'BZ', 'BDF', 'BPZ', 'BGE',
      'B1', 'B2', 'B3', 'B4', 'SKP', 'NBR', 'BNO', 'BNZ', 'BNF', 'BM', 'BL',
      'BN1', 'BN2', 'BN3', 'BN4', 'LDA', 'STR', 'IRX', 'OUT', 'INP',
      'RET', 'DIS', 'LDXA', 'STXD', 'ADC', 'SDB', 'SHRC', 'RSHR', 'SMB',
      'SAV', 'MARK', 'REQ', 'SEQ', 'ADCI', 'SDBI', 'SHLC', 'RSHL', 'SMBI',
      'GLO', 'GHI', 'PLO', 'PHI', 'LBO', 'LBZ', 'LBDF', 'NOP', 'LSNO',
      'LSNZ', 'LSNF', 'LSKP', 'NLBR', 'LBNQ', 'LBNZ', 'LBNF', 'LSIE', 'LSQ',
      'LSZ', 'LSDF', 'SEP', 'SEX', 'LDX', 'OR', 'AND', 'XOR', 'ADD', 'SD',
      'SHR', 'SM', 'LDI', 'ORI', 'ANI', 'XRI', 'ADI', 'SDI', 'SHL', 'SMI'
    ]

    registers_1802 = []

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

    registers_6502 = ['A', 'X', 'Y', 'P', 'S']

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

    registers_6800 = ['A', 'B', 'IX', 'PC', 'SP']

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

    registers_8080 = [
      'A', 'B', 'C', 'D', 'E', 'H', 'L', 'M', 'PSW', 'F'
      ]

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

    registers_z80 = [
      'A', 'B', 'C', 'D', 'E', 'H', 'L', 'F', 'AF', 'BC', 'DE', 'HL',
      "A'", "B'", "C'", "D'", "E'", "H'", "L'", "AF'", "F'", "BC'", "DE'", "HL'",
      'IX', 'IY', 'PSW', 'M'
      ]

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

    registers_8086 = [
      'AL', 'AH', 'BL', 'BH', 'CL', 'CH', 'DL', 'DH',
      'AX', 'BX', 'CX', 'DX', 'CS', 'DS', 'SS', 'ES',
      'IP', 'SI', 'DI', 'BP', 'SP', 'FLAGS'
      ]

    opcodes_80186 = [
      'BOUND',
      'ENTER',
      'INS',
      'LEAVE',
      'OUTS',
      'POPA', 'POPAD', 'PUSHA', 'PUSHAD'
    ]

    opcodes_80286 = [
      'ARPL',
      'CLTS',
      'LGDT', 'LIDT', 'LLDT', 'LMSW', 'LSL', 'LSS',
      'SGDT', 'SIDT', 'SLDT', 'SMSW', 'STR',
      'VERR', 'VERW'
    ]

    registers_80286 = [
      'TR'
    ]

    opcodes_80386 = [
      'BSF', 'BSR', 'BT', 'BTC', 'BTR', 'BTS',
      'CDQ', 'CWDE',
      'LFS', 'LGS', 'LSS',
      'MOVSX', 'MOVZX',
      'SETAE', 'SETB', 'SETC', 'SETNAE', 'SETNB', 'SETNE', 'SETNZ', 'SETG', 'SETGE', 'SETL', 'SETLE', 'SETNC', 'SETNG', 'SETNGE', 'SETNL', 'SETNLE', 'SETNO', 'SETNP', 'SETNS', 'SETE', 'SETO', 'SETP', 'SETPE', 'SETPO', 'SETS', 'SETZ',
      'SHLD', 'SHRD'
    ]

    registers_80386 = [
      'EAX', 'EBX', 'ECX', 'EDX', 'ESI', 'EDI', 'EBP', 'ESP',
      'FS', 'GS', 'EFLAGS'
    ]

    opcodes_80486 = [
      'BSWAP',
      'INVPLG'
    ]

    opcodes_pdp8 = [
      'AND', 'TAD', 'ISZ', 'DCA', 'JMS', 'JMP',
      'CDF', 'CIF', 'RDF', 'RIF', 'RIB', 'RMF',
      'CLA', 'CLL', 'CMA', 'CML', 'IAC', 'RAR', 'RAL', 'RTR', 'RTL', 'BSW',
      'SMA', 'SZA', 'SNL', 'SPA', 'SNA', 'SZL', 'OSR', 'HLT', 'MQA', 'MQL',
      'SEL', 'LCD', 'XDR', 'STR', 'SER', 'SDN', 'INTR', 'INIT',
      'DILC', 'DICD', 'DISD', 'DILX', 'DILY', 'DIXY', 'DILE', 'DIRE',
      'RCSF', 'RCRA', 'RCRB', 'RCNO', 'RCRC', 'RCNI', 'RCSD', 'RCSE',
      'RCRD', 'RCSI', 'RCTF',
      'RPE', 'RSF', 'RRB', 'RFC', 'PCE', 'PSF', 'PCF', 'PPC', 'PLS',
      'KCF', 'KSF', 'KCC', 'KRS', 'KIE', 'KRB', 'TFL', 'TSF', 'TCF',
      'TPC', 'TSK', 'TLS'
    ]

    opcodes_pdp11 = [
      'CLR', 'CLRB', 'COM', 'COMB', 'INC', 'INCB', 'DEC', 'DECB', 'NEG', 'NEGB',
      'NOP', 'TST', 'TSTB', 'TSTSET', 'WRTLCK', 'ASR', 'ASRB', 'ASL', 'ASLB',
      'ROR', 'RORB', 'ROL', 'ROLB', 'SWAB', 'ADC', 'ADCB', 'SBC', 'SBCB', 'SXT',
      'MOV', 'MOVB', 'ADD', 'SUB', 'CMP', 'CMPB', 'ASH', 'ASHC',
      'MUL', 'DIV', 'BIT', 'BITB', 'BIC', 'BICB', 'BIS', 'BISB',
      'XOR', 'CLR', 'CLRB', 'BR', 'BNE', 'BPL', 'BEQ', 'BMI', 'BVC',
      'BVS', 'BCC', 'BCS', 'BGE', 'BLT', 'BGT', 'BLE', 'SOB', 'BHI',
      'BLOS', 'BHIS', 'BLO',
      'JMP', 'JSR', 'RTS', 'MARK', 'EMT', 'TRAP', 'BPT', 'IOT', 'CSM',
      'RTI', 'RTT', 'HALT', 'WAIT', 'RESET',
      'MTPD', 'MTPI', 'MFPD', 'MTPS', 'MFPS', 'MFPT',
      'CLC', 'CLV', 'CLZ', 'CLN', 'CCC', 'SEC', 'SEV', 'SEZ', 'SEN', 'SCC',
      'FADD', 'FSUB', 'FMUL', 'FDIV',
      'DIV', 'MUL'
    ]

    registers_pdp11 = [
      'r0', 'r1', 'r2', 'r3', 'r4', 'r5', 'r6', 'r7'
    ]

    opcodes = []
    registers = []

    if processor in ['1802']:
      opcodes += opcodes_1802
      registers += registers_1802

    if processor in ['6502']:
      opcodes += opcodes_6502
      registers += registers_6502

    if processor in ['6800']:
      opcodes += opcodes_6800
      registers += registers_6800

    if processor in ['8080']:
      opcodes += opcodes_8080
      registers += registers_8080

    if processor in ['z80']:
      opcodes += opcodes_z80
      registers += registers_z80

    if processor in ['8086', '80186', '80286', '80386', '80486']:
      opcodes += opcodes_8086
      registers += registers_8086

    if processor in ['80286', '80386', '80486']:
      opcodes += opcodes_80186
      opcodes += opcodes_80286
      registers += registers_80286

    if processor in ['80386', '80486']:
      opcodes += opcodes_80386
      registers += registers_80386

    if processor in ['80486']:
      opcodes += opcodes_80486

    if processor in ['pdp-8']:
      opcodes += opcodes_pdp8
      # registers += registers_pdp8

    if processor in ['pdp-11']:
      opcodes += opcodes_pdp11
      registers += registers_pdp11

    opcode_tb = CaseInsensitiveListTokenBuilder(opcodes, 'keyword', False)
    register_tb = CaseInsensitiveListTokenBuilder(registers, 'register', True)

    values = ['*', '$', '.']

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      integer_1_tb,
      integer_2_tb,
      prefixed_integer_tb,
      hex_integer_1_tb,
      hex_integer_2_tb,
      hex_integer_3_tb,
      hex_integer_4_tb,
      hash_quote_value_tb,
      values_tb,
      groupers_tb,
      register_tb,
      opcode_tb,
      directive_tb,
      title_directive_tb,
      title_directive_2_tb,
      subtitle_directive_tb,
      subtitle_directive_2_tb,
      subtitle_directive_3_tb,
      include_directive_tb,
      include_directive_2_tb,
      comment_directive_tb,
      preprocessor_tb,
      identifier_tb,
      label_tb,
      string_tb,
      comment_tb,
      comment_2_tb,
      line_comment_tb,
      known_operator_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    opcode_tokenbuilders = [
      opcode_tb,
      directive_tb,
      title_directive_tb,
      subtitle_directive_tb,
      include_directive_tb,
      comment_directive_tb,
      preprocessor_tb,
      invalid_token_builder
    ]

    args_tokenbuilders = [
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
      identifier_tb,
      label_tb,
      string_tb,
      comment_tb,
      line_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    opcode_tokenizer = Tokenizer(opcode_tokenbuilders)
    args_tokenizer = Tokenizer(args_tokenbuilders)

    # tokenize as free-format
    tokens_free = tokenizer.tokenize(code)
    tokens_free = Examiner.combine_adjacent_identical_tokens(tokens_free, 'invalid operator')
    tokens_free = Examiner.combine_adjacent_identical_tokens(tokens_free, 'invalid')
    tokens_free = Examiner.combine_identifier_colon(tokens_free, ['newline'], [], [])
    tokens_free = Tokenizer.combine_number_and_adjacent_identifier(tokens_free)
    tokens_free = Examiner.convert_values_to_operators(tokens_free, known_operators)
    self.tokens = tokens_free
    self.convert_asm_identifiers_to_labels()

    self.calc_statistics()
    statistics_free = self.statistics
    self.statistics = {}

    self.calc_confidences(operand_types, group_starts, group_mids, group_ends, None)
    confidences_free = self.confidences
    self.confidences = {}
    errors_free = self.errors
    self.errors = []

    if processor in ['pdp-8', 'pdpd-11']:
      # do not try space-format, it never exists for these processors
      tokens_space = []
      statistics_space = {}
      confidences_space = {}
      errors_space = []
    else:
      # tokenize as space-format
      opcode_extras = '.&=,()+-*/'
      label_leads = '.&$@#'
      label_mids = '.&$#@'
      label_ends = ':'
      comment_leads = '*;'
      line_comment_leads = ''
      use_line_id = False
      tokens_space, indents = Tokenizer.tokenize_asm_code(code, tab_size, opcode_tokenizer, opcode_extras, args_tokenizer, label_leads, label_mids, label_ends, comment_leads, line_comment_leads, use_line_id)
      tokens_space = Examiner.combine_adjacent_identical_tokens(tokens_space, 'invalid operator')
      tokens_space = Examiner.combine_adjacent_identical_tokens(tokens_space, 'invalid')
      tokens_space = Examiner.combine_identifier_colon(tokens_space, ['newline'], [], [])
      tokens_space = Tokenizer.combine_number_and_adjacent_identifier(tokens_space)
      tokens_space = Examiner.convert_values_to_operators(tokens_space, known_operators)
      self.tokens = tokens_space
      self.convert_asm_identifiers_to_labels()

      self.calc_statistics()
      statistics_space = self.statistics
      self.statistics = {}

      self.calc_confidences(operand_types, group_starts, group_mids, group_ends, indents)
      confidences_space = self.confidences
      self.confidences = {}
      errors_space = self.errors
      self.errors = []

    # compute confidence for free-format and spaced-format
    confidence_free = 1.0
    if len(confidences_free) == 0:
      confidence_free = 0.0
    else:
      for key in confidences_free:
        factor = confidences_free[key]
        confidence_free *= factor

    confidence_space = 1.0
    if len(confidences_space) == 0:
      confidence_space = 0.0
    else:
      for key in confidences_space:
        factor = confidences_space[key]
        confidence_space *= factor

    # select the better of free-format and spaced-format
    if confidence_space > confidence_free:
      self.tokens = tokens_space
      self.statistics = statistics_space
      self.confidences = confidences_space
      self.errors = errors_space
    else:
      self.tokens = tokens_free
      self.statistics = statistics_free
      self.confidences = confidences_free
      self.errors = errors_free


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
