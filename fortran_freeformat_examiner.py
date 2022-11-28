import string

from codestat_exception import CodeStatException
from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  IdentifierTokenBuilder,
  StuffedQuoteStringTokenBuilder,
  PrefixedStringTokenBuilder,
  SingleCharacterTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from fortran_token_builders import (
  UserDefinedOperatorTokenBuilder,
  KindIntegerTokenBuilder,
  KindRealTokenBuilder
)
from fortran_examiner import FortranExaminer
from examiner import Examiner

class FortranFreeFormatExaminer(FortranExaminer):
  @staticmethod
  def __escape_z__():
    FortranExaminer.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    UserDefinedOperatorTokenBuilder.__escape_z__()
    KindIntegerTokenBuilder.__escape_z__()
    KindRealTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, year):
    super().__init__()

    if year is not None and year not in ['90', '1990', '95', '1995', '2003', '2008']:
      raise CodeStatException('Unknown year for language')

    operand_types = []

    kind_integer_tb = KindIntegerTokenBuilder()
    kind_real_tb = KindRealTokenBuilder()
    operand_types.append('number')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    bang_comment_tb = LeadToEndOfLineTokenBuilder('!', False, 'comment')

    quotes = ["'", '"', "’"]
    string_tb = StuffedQuoteStringTokenBuilder(quotes, False)
    binary_string_tb = PrefixedStringTokenBuilder('B', False, quotes)
    octal_string_tb = PrefixedStringTokenBuilder('O', False, quotes)
    hex_string_tb = PrefixedStringTokenBuilder('Z', False, quotes)
    operand_types.append('string')

    known_operators = [
      '=', '+', '-', '*', '/', '**',
      '==', '>', '>=', '<', '<=', '/=',
      '.EQ.', '.NE.', '.LT.', '.LE.', '.GT.', '.GE.',
      '.AND.', '.OR.', '.NOT.', '.EQV.', '.NEQV.',
      ':', '::', '=>', '%'
    ]

    known_operator_tb = CaseInsensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-'
    ]

    user_operator_tb = UserDefinedOperatorTokenBuilder()
    continuation_tb = SingleCharacterTokenBuilder('&', 'line continuation', False)
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator', False)

    groupers = ['(', ')', ',', '[', ']']
    # group_starts = ['(', '[', ',', '{']
    group_mids = [',']
    # group_ends = [')', ']', '}']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'IF', 'THEN', 'ELSE', 'ENDIF', 'END IF', 'GO', 'TO', 'GOTO', 'GO TO',
      'READ', 'WRITE', 'BACKSPACE', 'REWIND', 'ENDFILE', 'FORMAT', 'PRINT',
      'DO', 'CONTINUE',
      'PROGRAM', 'SUBROUTINE', 'FUNCTION', 'BLOCK DATA',
      'IMPLICIT', 'SAVE',
      'COMMON', 'DIMENSION', 'EQUIVALENCE', 'DATA', 'EXTERNAL',
      'CALL', 'RETURN', 'STOP', 'END',
      'INQUIRE', 'INTRINSIC', 'PARAMETER',
      'allocate', 'case', 'contains', 'cycle', 'deallocate',
      'elsewhere', 'exit', 'include', 'interface', 'intent', 'kind', 'module',
      'namelist', 'nullify', 'only', 'operator', 'optional', 'pointer',
      'private', 'procedure', 'public', 'recursive', 'result', 'select',
      'sequence', 'target', 'type', 'use', 'while', 'where',
      'enddo', 'end do', 'none'
    ]

    keywords_95 = [
      'FORALL', 'PURE', 'ELEMENTAL'
    ]

    keywords_2003 = [
      'abstract', 'allocatable', 'associate', 'bind', 'class', 'enum', 'end enum',
      'import', 'protected', 'select type', 'type guard', 'value', 'wait'
    ]

    keywords_2008 = [
      'block', 'contiguous'
    ]

    if year in ['95', '2003', '2008']:
      keywords += keywords_95

    if year in ['2003', '2008']:
      keywords += keywords_2003

    if year in ['2008']:
      keywords += keywords_2008

    keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    types = [
      'INTEGER', 'REAL', 'COMPLEX', 'DOUBLE PRECISION', 'DOUBLEPRECISION',
      'DOUBLE', 'PRECISION', 'LOGICAL', 'CHARACTER'
    ]

    types_tb = CaseInsensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    tokenbuilders = [
      self.newline_tb,
      self.whitespace_tb,
      continuation_tb,
      stmt_separator_tb,
      self.integer_tb,
      self.integer_exponent_tb,
      kind_integer_tb,
      self.real_tb,
      self.real_exponent_tb,
      self.double_exponent_tb,
      kind_real_tb,
      keyword_tb,
      types_tb,
      known_operator_tb,
      user_operator_tb,
      groupers_tb,
      identifier_tb,
      string_tb,
      binary_string_tb,
      octal_string_tb,
      hex_string_tb,
      bang_comment_tb,
      self.jcl_tb,
      self.unknown_operator_tb,
      self.invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['newline'], [], ['whitespace', 'comment', 'line description'])
    self.tokens = tokens
    self.convert_identifiers_to_labels()

    self.convert_numbers_to_lineNumbers()
    self.convert_stars_to_io_channels()

    self.calc_statistics()

    tokens = self.source_tokens()

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      # self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      # self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()
    self.calc_line_length_confidence(code, self.max_expected_line)
