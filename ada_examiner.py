import string
import math
from codestat_exception import CodeStatException
from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StuffedQuoteStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder
)
from ada_token_builders import (
  AdaCharTokenBuilder,
  DashDashCommentTokenBuilder
)
from examiner import Examiner

class AdaExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StuffedQuoteStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    AdaCharTokenBuilder.__escape_z__()
    DashDashCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, year):
    super().__init__()

    if year is not None and year not in ['83', '1983', '95', '1995', '2005', '2012']:
      raise CodeStatException('Unknown year for language')

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(False)
    real_tb = RealTokenBuilder(True, True, False)
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E', None)

    leads = '_'
    extras = '_'
    suffixes = ''
    identifier_tb = IdentifierTokenBuilder(leads, extras, suffixes)

    quotes = ['"']
    string_tb = StuffedQuoteStringTokenBuilder(quotes, False)
    char_tb = AdaCharTokenBuilder(["'", "’"])

    dash_dash_comment_tb = DashDashCommentTokenBuilder()

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

    known_operators = [
      'and', 'or', 'xor',
      '/=', '=', '<', '<=', '>', '>=',
      '+', '-', '&', '*', '/', 'mod', 'rem',
      '**', 'not', 'abs',
      '..',
      ':=', '.', "'",
      'new'
    ]

    self.unary_operators = [
      '+', '-', 'not', 'abs',
      'new'
    ]

    groupers = ['(', ')', ',', '=>', ':']
    # group_starts = ['(', ',']
    group_ends = [')']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'abort', 'accept', 'access', 'array', 'at',
      'begin', 'body',
      'case', 'constant',
      'declare', 'delay', 'delta', 'digits', 'do',
      'else', 'elsif', 'end', 'entry', 'exception', 'exit',
      'for', 'function',
      'generic', 'goto',
      'if', 'in', 'is',
      'limited', 'loop',
      'of', 'out',
      'package', 'pragma', 'private', 'procedure',
      'raise', 'range', 'record', 'renames', 'return', 'reverse',
      'select', 'separate', 'subtype',
      'task', 'terminate', 'then', 'type',
      'use',
      'when', 'while', 'with'
    ]

    keywords_95 =[
      'abstract', 'aliased',
      'protected',
      'requeue',
      'tagged',
      'until'
    ]

    keywords_2005 = [
      'interface',
      'overriding',
      'synchronized'
    ]

    keywords_2012 = [
      'some'
    ]

    if year in ['95', '1995', '2005', '2012']:
      keywords += keywords_95

    if year in ['2005', '2012']:
      keywords += keywords_2005

    if year in ['2012']:
      keywords += keywords_2012

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'character', 'duration', 'float', 'integer',
      'string', 'boolean', 'wide_character', 'wide_wide_character',
      'array', 'range'
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'all', 'null', 'others', '<>'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      types_tb,
      values_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      string_tb,
      char_tb,
      dash_dash_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence(tokens)
    self.calc_operator_3_confidence(tokens, group_ends)
    # self.calc_operator_4_confidence(tokens, group_starts)
    operand_types = ['number', 'string', 'symbol']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    # self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
