import string
import math
from Token import Token
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  ListTokenBuilder
)
from AdaTokenBuilders import (
  AdaCharTokenBuilder,
  DashDashCommentTokenBuilder
)
from Tokenizer import Tokenizer

class AdaExaminer(Examiner):
  def __init__(self, code, year):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(True, True)
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E')
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"'], True, False)
    char_tb = AdaCharTokenBuilder(["'"])

    dash_dash_comment_tb = DashDashCommentTokenBuilder()

    terminators_tb = ListTokenBuilder([';'], 'statement terminator', False)

    known_operators = [
      'and', 'or', 'xor',
      '/=', '=', '<', '<=', '>', '>=',
      '+', '-', '&', '*', '/', 'mod', 'rem',
      '**', 'not', 'abs',
      'in', '..',
      ':=', '.', ':', "'"
    ]

    self.unary_operators = [
      '+', '-', 'not', 'abs'
    ]

    groupers = ['(', ')', ',', '=>']
    group_ends = [')']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'abort',
      'accept',
      'access',
      'array',
      'at',
      'begin',
      'body',
      'case',
      'constant',
      'declare',
      'delay',
      'delta',
      'digits',
      'do',
      'else',
      'elsif',
      'end',
      'entry',
      'exception',
      'exit',
      'for',
      'function',
      'generic',
      'goto',
      'if',
      'is',
      'limited',
      'loop',
      'new',
      'of',
      'out',
      'package',
      'pragma',
      'private',
      'procedure',
      'raise',
      'range',
      'record',
      'renames',
      'return',
      'reverse',
      'select',
      'separate',
      'subtype',
      'task',
      'terminate',
      'then',
      'type',
      'use',
      'when',
      'while',
      'with'
    ]

    keywords_95 =[
      'abstract',
      'aliased',
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

    if year in ['95', '2005', '2012']:
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
      whitespace_tb,
      newline_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      types_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      string_tb,
      char_tb,
      dash_dash_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    # self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
