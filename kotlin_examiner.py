import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  PrefixedIntegerTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  PrefixedIdentifierTokenBuilder,
  ListTokenBuilder,
  SingleCharacterTokenBuilder,
  TripleQuoteStringTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder,
  ClassTypeTokenBuilder
)
from examiner import Examiner

class KotlinExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    PrefixedIntegerTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    PrefixedIdentifierTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    SlashStarCommentTokenBuilder.__escape_z__()
    TripleQuoteStringTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    prefixed_integer_tb = PrefixedIntegerTokenBuilder('0x', False, '0123456789ABCDEFabcdef_')
    real_tb = RealTokenBuilder(True, True, '_')
    real_exponent_tb = RealExponentTokenBuilder(True, True, 'E', '_')
    identifier_tb = IdentifierTokenBuilder()
    decorator_tb = PrefixedIdentifierTokenBuilder('@', 'decorator')
    quotes = ['"', "'"]
    string_tb = StringTokenBuilder(quotes, False, False)
    triple_quote_string_tb = TripleQuoteStringTokenBuilder(quotes)

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()
    class_tb = ClassTypeTokenBuilder()

    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator')

    known_operators = [
      '+', '-', '*', '/', '%',
      '=',
      '+=', '-=', '*=', '/=', '%=',
      '++', '--',
      '&&', '||', '!',
      '==', '!=',
      '===', '!==',
      '<', '>', '<=', '>=',
      '!!',
      '?.',
      '?:',
      '::',
      '..',
      ':',
      '?',
      '.'

    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    self.unary_operators = [
      '+', '-',
      '!', '*',
      '++', '--'
    ]

    self.postfix_operators = [
      '++', '--', ':'
    ]

    groupers = ['->', '(', ')', ',', '[', ']', '{', '}']
    group_ends = [')', ']', '}']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'as', 'as?',
      'break',
      'class', 'continue',
      'do',
      'else',
      'for', 'fun',
      'if', 'in', '!in', 'is', '!is',
      'object',
      'package',
      'return',
      'super',
      'throw', 'try', 'typealias', 'typeof',
      'val', 'var', 'when', 'while',
      'by',
      'catch', 'constructor',
      'delegate', 'dynamic',
      'field', 'file', 'finally',
      'get',
      'import', 'init',
      'param', 'property',
      'receiver',
      'set', 'setparam',
      'where',
      'actual', 'abstract', 'annotation',
      'companion', 'const', 'crossinline',
      'data',
      'enum', 'expect', 'external',
      'final',
      'infix', 'inline', 'inner', 'internal',
      'lateinit',
      'noinline',
      'open', 'operator', 'out', 'override',
      'private', 'protected', 'public',
      'reified',
      'sealed', 'suspend',
      'tailrec',
      'vararg'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    types = [
      'Byte', 'Short', 'Int', 'Long', 'Float', 'Double', 'Char',
      'u', 'f', 'ul'
    ]

    type_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'false', 'null', 'this', 'true'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      terminators_tb,
      integer_tb,
      integer_exponent_tb,
      prefixed_integer_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      type_tb,
      values_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      class_tb,
      decorator_tb,
      string_tb,
      triple_quote_string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    self.tokens = self.combine_numbers_and_adjacent_types(tokens)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    operand_types = ['number', 'string', 'symbol']
    self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_statistics()


  ## TODO: move to Examiner (along with copy in RustExaminer)
  def combine_numbers_and_adjacent_types(self, tokens):
    new_list = []

    new_token = None
    for token in tokens:
      if token.group == 'type' and new_token is not None and new_token.group == 'number':
        new_token = Token(new_token.text + token.text, 'number')
      else:
        if new_token is not None:
            new_list.append(new_token)
        new_token = token
    if new_token is not None:
      new_list.append(new_token)

    return new_list
