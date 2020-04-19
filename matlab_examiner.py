import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  PrefixedIntegerTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  PrefixedIdentifierTokenBuilder,
  ListTokenBuilder,
  LeadToEndOfLineTokenBuilder,
  BlockTokenBuilder,
  KeywordTokenBuilder
)
from matlab_token_builders import MatlabStringTokenBuilder
from examiner import Examiner

class MatlabExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    PrefixedIdentifierTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    BlockTokenBuilder.__escape_z__()
    KeywordTokenBuilder.__escape_z__()
    MatlabStringTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code, version):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder("'")
    integer_exponent_tb = IntegerExponentTokenBuilder("'")
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', False, '0123456789abcdefABCDEF')
    binary_integer_tb = PrefixedIntegerTokenBuilder('0b', False, '01')
    real_tb = RealTokenBuilder(False, False, "'")
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', "'")

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)

    command_tb = PrefixedIdentifierTokenBuilder('!', 'command', False)

    metaclass_tb = PrefixedIdentifierTokenBuilder('?', 'metaclass', False)

    quotes = ['"', "'", "’"]
    string_tb = MatlabStringTokenBuilder(quotes, False)

    line_comment_m_tb = LeadToEndOfLineTokenBuilder('%', False, 'comment')
    line_comment_o_tb = LeadToEndOfLineTokenBuilder('#', False, 'comment')
    block_comment_m_tb = BlockTokenBuilder('%{', '%}', 'comment')
    block_comment_o_tb = BlockTokenBuilder('#{', '#}', 'comment')

    line_continuation_tb = KeywordTokenBuilder('...', 'line continuation')

    known_operators = [
      '+', '-', '.*', '*', './', '/', '\\', '.^', '^', ".'", "'",
      '=', '==', '~=', '>', '>=', '<', '<=',
      '&', '|', '&&', '||', '~',
      '@', '.', '.?'
    ]

    operators_octave =[
      '++', '--', '+=', '-=', '*=', '/=', '^=', '!', '!=', '**'
    ]

    if version == 'octave':
      known_operators += operators_octave

    self.unary_operators = [
      '+', '-', '~', '@'
    ]

    self.postfix_operators = [
      "'"
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}', ';', ':']
    group_starts = ['(', '[', ',', '{']
    # group_mids = [',', ';', ':']
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False, False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', False, True)

    keywords = [
    'break',
    'case', 'catch', 'classdef', 'continue',
    'else', 'elseif', 'end',
    'for', 'function',
    'global',
    'if',
    'otherwise',
    'parfor', 'persistent',
    'return',
    'spmd', 'switch',
    'try',
    'while'
    ]

    keywords_octave = [
      'endfor', 'endif', 'endwhile'
    ]

    if version == 'octave':
      keywords += keywords_octave

    keyword_tb = ListTokenBuilder(keywords, 'keyword', False, True)

    values = [
      'inf', 'Nan'
    ]

    values_tb = ListTokenBuilder(values, 'value', True, True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      integer_tb,
      integer_exponent_tb,
      hex_integer_tb,
      binary_integer_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      command_tb,
      metaclass_tb,
      string_tb,
      line_comment_m_tb,
      block_comment_m_tb
    ]

    tokenbuilders_2 = [
      line_comment_o_tb,
      block_comment_o_tb
    ]

    if version == 'octave':
      tokenbuilders += tokenbuilders_2

    tokenbuilders_9 = [
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenbuilders += tokenbuilders_9

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_operator_confidence()

    allow_pairs = []

    self.calc_operator_2_confidence(tokens, allow_pairs)
    self.calc_operator_3_confidence(tokens, group_ends, allow_pairs)
    self.calc_operator_4_confidence(tokens, group_starts, allow_pairs)
    # self.calc_group_confidence(tokens, group_mids)
    # operand_types = ['number']
    # self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
