import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  EscapedStringTokenBuilder,
  PrefixedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from cx_token_builders import (
  SlashSlashCommentTokenBuilder,
  SlashStarCommentTokenBuilder,
  ClassTypeTokenBuilder,
  IncludeNameTokenBuilder
)
from objectivec_token_builders import (
  DirectiveTokenBuilder
)
from examiner import Examiner

class ObjectiveCExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    EscapedStringTokenBuilder.__escape_z__()
    PrefixedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    SlashSlashCommentTokenBuilder.__escape_z__()
    SlashStarCommentTokenBuilder.__escape_z__()
    ClassTypeTokenBuilder.__escape_z__()
    IncludeNameTokenBuilder.__escape_z__()
    DirectiveTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    operand_types.append('number')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    directive_tb = DirectiveTokenBuilder()

    quotes = ['"', "'"]
    string_tb = EscapedStringTokenBuilder(quotes, 10)
    prefixed_string_tb = PrefixedStringTokenBuilder('@', False, quotes)
    operand_types.append('string')

    class_type_tb = ClassTypeTokenBuilder()
    operand_types.append('class')

    slash_slash_comment_tb = SlashSlashCommentTokenBuilder()
    slash_star_comment_tb = SlashStarCommentTokenBuilder()

    directives = [
      '#define', '#undef',
      '#ifdef', '#ifndef', '#if', '#endif', '#else', '#elif',
      '#import', '#line', '#include'
    ]

    line_continuation_tb = SingleCharacterTokenBuilder('\\', 'line continuation', False)
    c_preprocessor_tb = CaseSensitiveListTokenBuilder(directives, 'preprocessor', False)
    c_warning_tb = LeadToEndOfLineTokenBuilder('#warning', True, 'preprocessor')
    c_error_tb = LeadToEndOfLineTokenBuilder('#error', True, 'preprocessor')
    c_pragma_tb = LeadToEndOfLineTokenBuilder('#pragma', True, 'preprocessor')
    include_name_tb = IncludeNameTokenBuilder('#include')
    import_name_tb = IncludeNameTokenBuilder('#import')
    terminators_tb = SingleCharacterTokenBuilder(';', 'statement terminator', False)

    known_operators = [
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '>', '>=', '<', '<=',
      '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
      '!', '&', '|', '<<', '>>', '~',
      '.', '->',
      '++', '--', '&&', '||', '^',
      '?', '##'
    ]

    self.unary_operators = [
      '+', '-', '*',
      '!', '&', '^', '~',
      '++', '--',
      '##'
    ]

    self.postfix_operators = [
      '++', '--', '&', '->', '*', '^'
    ]

    groupers = ['(', ')', ',', '[', ']', '{', '}', ':']
    group_starts = ['(', '[', ',', '{']
    group_ends = [')', ']', '}']
    group_mids = [',', ':']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'atomic',
      'break', 'bycopy', 'byref',
      'case', 'continue',
      'default', 'do',
      'else',
      'for',
      'goto',
      'if', 'IMP', 'in', 'inline', 'inout',
      'nonatomic',
      'oneway', 'out',
      'Protocol',
      'restrict', 'retain', 'return',
      'SEL', 'sizeof', 'switch',
      'typedef',
      'while',
      '@interface', '@end', '@implementation', '@protocol', '@class',
      '@public', '@protected', '@private', '@property', '@try', '@throw',
      '@catch()', '@finally', '@synthesize', '@dynamic', '@selector'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    types = [
      'auto',
      'char', 'const',
      'double',
      'enum', 'extern',
      'float',
      'id', 'int',
      'long',
      'register',
      'short', 'signed', 'static', 'struct',
      'union', 'unsigned',
      'void', 'volatile',
      '_Bool', '_Complex', '_Imaginary',
      'BOOL',
      'Class'
    ]

    types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    values = [
      'self', 'super', 'nil', 'YES', 'NO', 'NULL', '...'
    ]

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
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
      directive_tb,
      identifier_tb,
      string_tb,
      prefixed_string_tb,
      slash_slash_comment_tb,
      slash_star_comment_tb,
      c_preprocessor_tb,
      c_warning_tb,
      c_error_tb,
      c_pragma_tb,
      include_name_tb,
      import_name_tb,
      class_type_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    # tokens = Examiner.combine_identifier_colon(tokens, ['statement terminator', 'newline'], ['{'], ['whitespace', 'comment'])
    self.tokens = tokens
    self.convert_identifiers_before_colon_to_labels('{};')
    self.convert_identifiers_after_goto_to_labels()
    self.convert_values_to_operators()

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence(['*', ';'])

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'string', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence(0.7)

    self.calc_paired_blockers_confidence(['{'], ['}'])
    self.calc_line_length_confidence(code, self.max_expected_line)


  # convert values to operators
  def convert_values_to_operators(self):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'value' and token.text == '...' and \
        prev_token.is_operand:
        token.group = 'operator'
        token.is_operand = False

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token
