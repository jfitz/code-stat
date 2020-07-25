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
  MultilineCommentTokenBuilder
)
from examiner import Examiner

class AssemblyX86Examiner(Examiner):
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
    operand_types.append('number')

    leads = '_.$@'
    extras = '_.$@'
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
      'if', 'else', 'endif', 'macro', 'endm', 'error'
    ]

    preprocessor_tb = CaseInsensitiveListTokenBuilder(preprocessors, 'preprocessor', False)

    directives = [
      'ASEG', 'CSEG', 'DB', 'DW', 'DS', 'DSEG',
      'END', 'ENDS', 'EQU', 'ORG', 'PAGE', 'SSEG',
      '.RADIX', '.SALL', '.XLIST', '.MODEL', '.code', 'PROC',
      'EXTRN', 'SEGMENT', 'PUBLIC', 'ASSUME', 'INS86',
      'RESB', 'RESD', '%INCLUDE', 'BITS', 'GLOBAL', 'SECTION', 'ALIGN'
    ]

    directive_tb = CaseInsensitiveListTokenBuilder(directives, 'directive', False)

    title_directive_tb = LeadToEndOfLineTokenBuilder('TITLE', False, 'directive')
    subtitle_directive_tb = LeadToEndOfLineTokenBuilder('SUBTTL', False, 'directive')
    include_directive_tb = LeadToEndOfLineTokenBuilder('INCLUDE', False, 'directive')

    comment_directive_tb = MultilineCommentTokenBuilder()

    opcodes = [
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

    registers = [
      'AL', 'AH', 'BL', 'BH', 'CL', 'CH', 'DL', 'DH',
      'AX', 'BX', 'CX', 'DX', 'CS', 'DS', 'SS', 'ES',
      'IP', 'SI', 'DI', 'BP', 'SP', 'FLAGS'
      ]

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
      comment_directive_tb,
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
    self.tokens = tokens
    self.convert_asm_identifiers_to_labels()

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
