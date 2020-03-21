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
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  ListTokenBuilder,
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
    StringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    VisualBasicVariableTokenBuilder.__escape_z__()
    RemarkTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()
    line_continuation_tb = SingleCharacterTokenBuilder(['_'], 'line continuation')

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    variable_tb = VisualBasicVariableTokenBuilder(['$', '%', '#', '!'])

    leads = '_'
    extras = '_'
    suffixes = ''
    identifier_tb = IdentifierTokenBuilder(leads, extras, suffixes)

    quotes = ['"']
    string_tb = StringTokenBuilder(quotes, False)

    remark_tb = RemarkTokenBuilder()
    comment_tb = LeadToEndOfLineTokenBuilder("'", True, 'comment')
    comment2_tb = LeadToEndOfLineTokenBuilder("’", True, 'comment')

    directives = [
      '#If', '#Else', '#ElseIf', '#End If',
      '#ExternalSource',
      '#Line', '#Region', '#End Region',
      '#Const'
    ]

    preprocessor_tb = ListTokenBuilder(directives, 'preprocessor', True)

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
    group_ends = [')', ']']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

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

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    functions = [
      'Asc', 'AscW', 'Chr', 'ChrW', 'Filter', 'Format',
      'GetChar', 'InStr', 'InStrRev', 'Join', 'LCase', 'Left',
      'Len', 'LSet', 'LTrim', 'Mid', 'Replace', 'Right', 'RSet',
      'RTrim', 'Space', 'Split', 'StrComp', 'StrConv', 'StrDup',
      'StrReverse', 'Trim', 'UCase'
    ]

    function_tb = ListTokenBuilder(functions, 'function', True)

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

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'False', 'True', 'Nothing',
      'MyBase', 'MyClass'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

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
      comment2_tb,
      preprocessor_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')
    tokens = Examiner.combine_identifier_colon(tokens, ['newline'], [], ['whitespace', 'comment'])
    self.tokens = tokens
    self.convert_identifiers_to_labels()

    self.convert_keywords_to_identifiers(['.'])
    self.convert_functions_to_identifiers()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence(tokens)
    self.calc_operator_3_confidence(tokens, group_ends)
    self.calc_operator_4_confidence(tokens, group_starts)
    operand_types = ['number', 'string', 'symbol']
    self.calc_operand_confidence(tokens, operand_types)
    self.calc_keyword_confidence()
    self.calc_statistics()


  def convert_functions_to_identifiers(self):
    prev_token = Token('\n', 'newline')

    for token in self.tokens:
      if token.group == 'function' and\
        prev_token.group == 'operator' and prev_token.text == '.':
        token.group = 'identifier'

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token
