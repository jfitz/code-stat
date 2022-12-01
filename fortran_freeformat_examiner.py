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
  LeadToEndOfLineTokenBuilder,
  NullTokenBuilder
)
from fortran_token_builders import (
  FortranIdentifierTokenBuilder,
  FormatSpecifierTokenBuilder,
  HollerithStringTokenBuilder,
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
    FormatSpecifierTokenBuilder.__escape_z__()
    HollerithStringTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    UserDefinedOperatorTokenBuilder.__escape_z__()
    KindIntegerTokenBuilder.__escape_z__()
    KindRealTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, year):
    super().__init__()

    # FORTRAN-66 should be upper case only
    # FORTRAN-77 may be upper or lower case
    case_significant = year in ['66', '1966']

    operand_types = []

    if year in ['66', '1966', '77', '1977']:
      kind_integer_tb = NullTokenBuilder()
      kind_real_tb = NullTokenBuilder()
    else:
      kind_integer_tb = KindIntegerTokenBuilder()
      kind_real_tb = KindRealTokenBuilder()

    operand_types.append('number')

    if year in ['66', '1966']:
      identifier_tb = FortranIdentifierTokenBuilder()
    elif year in ['77', '1977']:
      leads = ''
      extras = ''
      identifier_tb = IdentifierTokenBuilder(leads, extras)
    else:
      leads = '_'
      extras = '_'
      identifier_tb = IdentifierTokenBuilder(leads, extras)

    operand_types.append('identifier')

    if year in ['66', '1966', '77', '1977']:
      bang_comment_tb = NullTokenBuilder()
    else:
      bang_comment_tb = LeadToEndOfLineTokenBuilder('!', False, 'comment')

    quotes = ['"', "'"]

    if year in ['66', '1966', '77', '1977']:
      if year in ['66', '1966']:
        hollerith_tb = HollerithStringTokenBuilder()
        string_tb = NullTokenBuilder()
      else:
        hollerith_tb = NullTokenBuilder()
        string_tb = StuffedQuoteStringTokenBuilder(quotes, False)

      binary_string_tb = NullTokenBuilder()
      octal_string_tb = NullTokenBuilder()
      hex_string_tb = NullTokenBuilder()
    else:
      string_tb = StuffedQuoteStringTokenBuilder(quotes, False)
      binary_string_tb = PrefixedStringTokenBuilder('B', False, quotes)
      octal_string_tb = PrefixedStringTokenBuilder('O', False, quotes)
      hex_string_tb = PrefixedStringTokenBuilder('Z', False, quotes)

    operand_types.append('string')

    if year in ['66', '1966', '77', '1977']:
      format_tb = FormatSpecifierTokenBuilder()
    else:
      format_tb = NullTokenBuilder()

    if year in ['66', '1966', '77', '1977']:
      known_operators = [
        '=', '+', '-', '*', '/', '**',
        '.EQ.', '.GT.', '.GE.', '.LT.', '.LE.', '.NE.',
        '.AND.', '.OR.', '.NOT.'
      ]
    else:
      known_operators = [
        '=', '+', '-', '*', '/', '**',
        '==', '>', '>=', '<', '<=', '/=',
        '.EQ.', '.NE.', '.LT.', '.LE.', '.GT.', '.GE.',
        '.AND.', '.OR.', '.NOT.', '.EQV.', '.NEQV.',
        ':', '::', '=>', '%'
      ]

    if case_significant:
      known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)
    else:
      known_operator_tb = CaseInsensitiveListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-', '.NOT.'
    ]

    if year in ['66', '1966', '77', '1977']:
      user_operator_tb = NullTokenBuilder()
      continuation_tb = NullTokenBuilder()
      stmt_separator_tb = NullTokenBuilder()
    else:
      user_operator_tb = UserDefinedOperatorTokenBuilder()
      continuation_tb = SingleCharacterTokenBuilder('&', 'line continuation', False)
      stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator', False)

    if year in ['66', '1966', '77', '1977']:
      groupers = ['(', ')', ',']
      # group_starts = ['(', ',']
      group_mids = [',']
      # group_ends = [')']
    else:
      groupers = ['(', ')', ',', '[', ']']
      # group_starts = ['(', '[', ',', '{']
      group_mids = [',']
      # group_ends = [')', ']', '}']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'IF', 'GO', 'TO', 'GOTO', 'GO TO', 'ASSIGN',
      'READ', 'WRITE', 'BACKSPACE', 'REWIND', 'ENDFILE', 'FORMAT',
      'DO', 'CONTINUE',
      'SUBROUTINE', 'FUNCTION', 'BLOCK DATA',
      'COMMON', 'DIMENSION', 'EQUIVALENCE',
      'DATA', 'EXTERNAL',
      'CALL', 'RETURN', 'PAUSE', 'STOP', 'END'
    ]

    keywords_77 = [
      'THEN', 'ELSE', 'ENDIF', 'END IF',
      'PRINT', 'PROGRAM',
      'IMPLICIT', 'SAVE',
      'INQUIRE', 'INTRINSIC', 'PARAMETER'
    ]

    keywords_90 = [
      'DATA', 'EXTERNAL',
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

    if year in ['77', '1977', '90', '1990', '95', '2003', '2008']:
      keywords += keywords_77

    if year in ['90', '1990', '95', '2003', '2008']:
      keywords += keywords_90

    if year in ['95', '2003', '2008']:
      keywords += keywords_95

    if year in ['2003', '2008']:
      keywords += keywords_2003

    if year in ['2008']:
      keywords += keywords_2008

    if case_significant:
      keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)
    else:
      keyword_tb = CaseInsensitiveListTokenBuilder(keywords, 'keyword', False)

    if year in ['66', '1966']:
      types = [
        'INTEGER', 'REAL', 'COMPLEX', 'DOUBLE PRECISION', 'DOUBLEPRECISION',
        'LOGICAL'
      ]
    else:
      types = [
        'INTEGER', 'REAL', 'COMPLEX', 'DOUBLE PRECISION', 'DOUBLEPRECISION',
        'DOUBLE', 'LOGICAL', 'CHARACTER'
      ]

    if case_significant:
      types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    else:
      types_tb = CaseInsensitiveListTokenBuilder(types, 'type', True)

    operand_types.append('type')

    tokenbuilders_free = [
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

    tokenizer_free = Tokenizer(tokenbuilders_free)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens_free = tokenizer_free.tokenize(ascii_code)

    tokens_free = Examiner.combine_adjacent_identical_tokens(tokens_free, 'invalid operator')
    tokens_free = Examiner.combine_adjacent_identical_tokens(tokens_free, 'invalid')
    tokens_free = Examiner.combine_identifier_colon(tokens_free, ['newline'], [], ['whitespace', 'comment', 'line description'])
    self.tokens = tokens_free

    self.convert_identifiers_to_labels()
    self.convert_numbers_to_lineNumbers()
    self.convert_stars_to_io_channels()

    self.calc_statistics()

    tokens_free = self.source_tokens()

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens_free, num_operators, allow_pairs)
      # self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      # self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens_free, group_mids)

    operand_types_2 = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_n_confidence(tokens_free, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens_free, operand_types, 4)

    self.calc_keyword_confidence()
    self.calc_line_length_confidence(code, self.max_expected_line)
