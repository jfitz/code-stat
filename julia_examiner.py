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
  TripleQuoteStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  PrefixedIntegerTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  SuffixedRealTokenBuilder,
  IdentifierTokenBuilder,
  PrefixedIdentifierTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadCommentTokenBuilder,
  NestedCommentTokenBuilder
)
from examiner import Examiner

class JuliaExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StringTokenBuilder.__escape_z__()
    PrefixedStringTokenBuilder.__escape_z__()
    TripleQuoteStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    SuffixedRealTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    PrefixedIdentifierTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadCommentTokenBuilder.__escape_z__(),
    NestedCommentTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    hex_integer_tb = PrefixedIntegerTokenBuilder('0x', False, '0123456789abcdefABCDEF')
    real_tb = RealTokenBuilder(False, False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    imaginary_tb = SuffixedRealTokenBuilder(False, False, ['im', 'cx'], None)
    identifier_tb = IdentifierTokenBuilder()
    symbol_tb = PrefixedIdentifierTokenBuilder(':', 'symbol')
    attribute_tb = PrefixedIdentifierTokenBuilder('@', 'attribute')
    dollar_sign_tb = SingleCharacterTokenBuilder('$', 'identifier')
    quotes = ['"', "'"]
    string_tb = StringTokenBuilder(quotes, False, False, False)
    raw_string_tb = PrefixedStringTokenBuilder('raw', True, quotes)
    b_string_tb = PrefixedStringTokenBuilder('b', True, quotes)
    triple_quote_string_tb = TripleQuoteStringTokenBuilder(quotes)

    comment_tb = LeadCommentTokenBuilder('#')
    nested_comment_tb = NestedCommentTokenBuilder('#=', '=#')

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation')
    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

    known_operators = [
      'where', 'in', 'isa', '′', "'",
      '+', '-', '*', '/', '\\', '^', '%', '//',
      '<<', '>>', '<<<', '>>>',
      ':', '=', '==', '!=', '===', '!==',
      '+=', '-=', '*=', '/=', '^=', '%=',
      '<', '>', '<=', '>=',
      '~', '&', '|', '!', '&&', '||', '?', '.',
      '<:', '>:',
      '::', '->',
      '...',
      '∀', '≤', '≥', '⊻', '⊽', '⊼'
    ]

    # 0x391 through 0x3a9 (capital)
    # 0x3b1 through 0x3c9 (small)
    greek_letters = [
      'α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ', 'ι', 'κ', 'λ', 'μ',
      'ν', 'ξ', 'ο', 'π', 'ρ', 'ς', 'σ', 'τ', 'υ', 'φ', 'χ', 'ψ', 'ω'
    ]

    greek_letter_tb = ListTokenBuilder(greek_letters, 'identifier', True)

    self.unary_operators = [
      'isa', '+', '-', '~', '!', '.', ':', '::', "'",
      '<:', '>:',
    ]

    self.postfix_operators = [
      '...', '′'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'baremodule', 'begin', 'break',
      'catch', 'const', 'continue',
      'do',
      'else', 'elseif', 'end', 'export',
      'finally', 'for', 'function',
      'global',
      'if',
      'import',
      'let', 'local',
      'macro', 'module',
      'quote',
      'return',
      'struct', 'try',
      'using',
      'while',
      'abstract', 'mutable', 'primitive', 'type'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
       'Int8', 'UInt8', 'Int16', 'UInt16', 'Int32', 'UInt32', 'Int64', 'UInt64',
       'Int128', 'UInt128', 'Float16', 'Float32', 'Float64',
       'Bool', 'Char'
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'false', 'true'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      hex_integer_tb,
      real_tb,
      real_exponent_tb,
      imaginary_tb,
      keyword_tb,
      types_tb,
      values_tb,
      groupers_tb,
      known_operator_tb,
      identifier_tb,
      symbol_tb,
      attribute_tb,
      dollar_sign_tb,
      greek_letter_tb,
      string_tb,
      raw_string_tb,
      b_string_tb,
      triple_quote_string_tb,
      comment_tb,
      nested_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()
