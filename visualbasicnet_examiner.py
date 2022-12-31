import string
import math

from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  EscapedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  SuffixedIdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  LeadToEndOfLineTokenBuilder,
  SingleCharacterTokenBuilder
)
from visualbasic_token_builders import (
  VisualBasicVariableTokenBuilder,
  RemarkTokenBuilder
)
from examiner import Examiner

class VisualBasicNETExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    EscapedStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    SuffixedIdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    VisualBasicVariableTokenBuilder.__escape_z__()
    RemarkTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    line_continuation_tb = SingleCharacterTokenBuilder(['_'], 'line continuation', False)

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    operand_types.append('number')

    variable_tb = VisualBasicVariableTokenBuilder('$%#!')
    operand_types.append('variable')

    leads = '_'
    extras = '_'
    suffixes = '$%#!'
    identifier_tb = SuffixedIdentifierTokenBuilder(leads, extras, suffixes)
    operand_types.append('identifier')

    quotes = ['"']
    string_tb = EscapedStringTokenBuilder(quotes, 0)
    operand_types.append('string')

    remark_tb = RemarkTokenBuilder()
    comment_tb = LeadToEndOfLineTokenBuilder("'", True, 'comment')

    directives = [
      '#If', '#Else', '#ElseIf', '#End If',
      '#ExternalSource',
      '#Line', '#Region', '#End Region',
      '#Const'
    ]

    preprocessor_tb = CaseSensitiveListTokenBuilder(directives, 'preprocessor', False)

    known_operators = [
      '&', '&=', '*', '*=', '/', '/=', '\\', '\\=', '^', '^=',
      '+', '+=', '-', '-=', '>>', '>>=', '<<', '<<=',
      '.', '=', '<', '<=', '>', '>=', '<>',
      'AddressOf', 'And', 'AndAlso', 'In', 'Is', 'IsNot', 'Like',
      'Or', 'OrElse', 'Xor'
    ]

    self.unary_operators = [
      '+', '-', 'Not', 'IsNot'
    ]

    groupers = ['(', ')', ',', '[', ']']
    group_starts = ['(', '[', ',']
    group_mids = [',']
    group_ends = [')', ']']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    keywords = [
      'AddHandler', 'Alias', 'As',
      'ByRef', 'ByVal',
      'Call', 'Case', 'Catch', 'Class', 'Const', 'Continue',
      'Declare', 'Default', 'Delegate', 'Dim', 'DirectCast', 'Do'
      'Each', 'Else', 'ElseIf', 'End', 'Enum', 'Erase', 'Error', 'Event',
      'Finally', 'For', 'For Each', 'Friend', 'Function',
      'Get', 'GetType', 'GetXMLNamespace', 'Global', 'GoSub', 'GoTo',
      'Handles',
      'If', 'Implements', 'Imports', 'Inherits', 'Interface',
      'Let', 'Lib', 'Loop',
      'Module', 'MustInherit', 'MustOverride',
      'Namespace', 'Narrowing', 'New Constraint', 'New Operator', 'Next',
      'NotInheritable', 'NotOverridable',
      'Of', 'On', 'Operator', 'Option', 'Optional', 'Out',
      'Overloads', 'Overridable', 'Overrides',
      'ParamArray', 'Partial', 'Private', 'Property', 'Protected', 'Public',
      'RaiseEvent', 'ReadOnly', 'ReDim', 'REM', 'RemoveHandler', 'Resume',
      'Return', 'Select', 'Set', 'Shadows', 'Shared',
      'Static', 'Step', 'Stop', 'Structure', 'Sub',
      'SyncLock', 'Then', 'Throw', 'To', 'Try', 'TryCast',
      'TypeOf', 'Using',
      'Wend', 'When', 'While', 'Widening', 'With', 'WithEvents',
      'WriteOnly'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    functions = [
      'Asc', 'AscW', 'Chr', 'ChrW', 'Filter', 'Format',
      'GetChar', 'InStr', 'InStrRev', 'Join', 'LCase', 'Left',
      'Len', 'LSet', 'LTrim', 'Mid', 'Replace', 'Right', 'RSet',
      'RTrim', 'Space', 'Split', 'StrComp', 'StrConv', 'StrDup',
      'StrReverse', 'Trim', 'UCase'
    ]

    function_tb = CaseSensitiveListTokenBuilder(functions, 'function', True)

    types = [
      'Boolean', 'Byte',
      'CBool', 'CByte', 'CChar', 'CDate', 'CDbl', 'CDec', 'Char',
      'CInt', 'CLng', 'CObj', 'CSByte', 'CShort', 'CSng', 'CStr',
      'CType', 'CUInt', 'CULng', 'CUShort',
      'Date', 'Decimal', 'Double',
      'Integer',
      'Long',
      'Object',
      'SByte', 'Short', 'Single', 'String',
      'UInteger', 'ULong', 'UShort',
    ]

    types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    values = [
      'False', 'True', 'Nothing',
      'MyBase', 'MyClass'
    ]

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      line_continuation_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      groupers_tb,
      known_operator_tb,
      types_tb,
      values_tb,
      function_tb,
      variable_tb,
      identifier_tb,
      string_tb,
      remark_tb,
      comment_tb,
      preprocessor_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['newline'], [], ['whitespace', 'comment'])
    self.tokens = tokens
    self.convert_identifiers_before_colon_to_labels('')
    self.convert_identifiers_after_goto_to_labels()
    self.convert_keywords_to_identifiers(['.'])
    self.convert_functions_to_identifiers()

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()

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

    self.calc_keyword_confidence()
    self.calc_line_length_confidence(code, self.max_expected_line)


  def convert_functions_to_identifiers(self):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'function' and\
        prev_token.group == 'operator' and prev_token.text == '.':
        token.group = 'identifier'

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token
