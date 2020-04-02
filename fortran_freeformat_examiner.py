import string

from codestat_exception import CodeStatException
from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  IdentifierTokenBuilder,
  StuffedQuoteStringTokenBuilder,
  PrefixedStringTokenBuilder,
  SingleCharacterTokenBuilder,
  ListTokenBuilder,
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
    LeadToEndOfLineTokenBuilder.__escape_z__()
    UserDefinedOperatorTokenBuilder.__escape_z__()
    KindIntegerTokenBuilder.__escape_z__()
    KindRealTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, year):
    super().__init__()

    if year is not None and year not in ['90', '1990', '95', '1995', '2003', '2008']:
      raise CodeStatException('Unknown year for language')

    kind_integer_tb = KindIntegerTokenBuilder()
    kind_real_tb = KindRealTokenBuilder()

    leads = '_'
    extras = '_'
    suffixes = ''
    identifier_tb = IdentifierTokenBuilder(leads, extras, suffixes)

    bang_comment_tb = LeadToEndOfLineTokenBuilder('!', False, 'comment')
    quotes = ["'", '"', "’"]
    string_tb = StuffedQuoteStringTokenBuilder(quotes, False)
    binary_string_tb = PrefixedStringTokenBuilder('B', False, quotes)
    octal_string_tb = PrefixedStringTokenBuilder('O', False, quotes)
    hex_string_tb = PrefixedStringTokenBuilder('Z', False, quotes)

    known_operators = [
      '=', '+', '-', '*', '/', '**',
      '==', '>', '>=', '<', '<=', '/=',
      '.EQ.', '.NE.', '.LT.', '.LE.', '.GT.', '.GE.',
      '.AND.', '.OR.', '.NOT.', '.EQV.', '.NEQV.',
      ':', '::', '=>', '%'
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', False)

    self.unary_operators = [
      '+', '-'
    ]

    user_operator_tb = UserDefinedOperatorTokenBuilder()
    continuation_tb = SingleCharacterTokenBuilder('&', 'line continuation')
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator')

    groupers = ['(', ')', ',', '[', ']']
    # group_starts = ['(', '[', ',', '{']
    # group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

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

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False)

    types = [
      'INTEGER', 'REAL', 'COMPLEX', 'DOUBLE PRECISION', 'DOUBLEPRECISION',
      'DOUBLE', 'PRECISION', 'LOGICAL', 'CHARACTER'
    ]

    types_tb = ListTokenBuilder(types, 'type', False)

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
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['newline'], [], ['whitespace', 'comment'])
    self.tokens = tokens
    self.convert_identifiers_to_labels()

    self.convert_numbers_to_lineNumbers()
    self.convert_stars_to_io_channels()

    tokens = self.source_tokens()

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence(tokens)
    # self.calc_operator_3_confidence(tokens, group_ends)
    # self.calc_operator_4_confidence(tokens, group_starts)
    operand_types = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    self.calc_statistics()
