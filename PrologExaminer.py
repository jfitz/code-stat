import string
import math
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
  ListTokenBuilder,
  LeadCommentTokenBuilder
)
from PrologTokenBuilders import (
  IdentifierTokenBuilder,
  PrologVariableTokenBuilder
)
from Tokenizer import Tokenizer

class PrologExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    stmt_separator_tb = ListTokenBuilder([';'], 'statement separator', False)

    integer_tb = IntegerTokenBuilder(True)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False)
    variable_tb = PrologVariableTokenBuilder()
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"', "'"], False)

    comment_tb = LeadCommentTokenBuilder('%')

    known_operators = [
        '-->', ':-',
        '?-',
        '|',
        '->', '*->',
        ':=', '\\+',
        '<', '=', '=..', '=@=', '\\=@=', '=:=', '=<', '==', '=\\=',
        '>', '>=', '@<', '@=<', '@>', '@>=', '\\=', '\\==', 'as', 'is', '>:<',
        ':<',
        ':', '+', '-', '/\\', '\\/', 'xor',
        '?', '*', '/', '//', 'div', 'rdiv', '<<', '>>', 'mod', 'rem',
        '**', '^', '+', '-', '\\', '.', '$'
      ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    groupers = ['(', ')', ',', '[', ']']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    keywords = [
      'dynamic', 'discontiguous', 'initialization', 'meta_predicate',
      'module_transparent', 'multifile', 'public', 'thread_local',
      'thread_initialization', 'volatile'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      variable_tb,
      known_operator_tb,
      groupers_tb,
      identifier_tb,
      string_tb,
      comment_tb,
      self.unknown_operator_tb
    ]

    invalid_token_builder = InvalidTokenBuilder()
    tokenizer = Tokenizer(tokenbuilders, invalid_token_builder)

    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
