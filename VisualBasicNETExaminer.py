import string
import math
from Token import Token
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  PrefixedStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  ListTokenBuilder,
  LeadCommentTokenBuilder
)
from BasicTokenBuilders import (
  BasicSuffixedIntegerTokenBuilder,
  BasicSuffixedRealTokenBuilder
)
from VisualBasicTokenBuilders import (
  VisualBasicVariableTokenBuilder,
  RemarkTokenBuilder
)
from CXTokenBuilders import (
  CPreProcessorTokenBuilder
)
from Tokenizer import Tokenizer

class VisualBasicNETExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    variable_tb = VisualBasicVariableTokenBuilder(['$', '%', '#', '!'])
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"'], False, False)

    remark_tb = RemarkTokenBuilder()
    comment_tb = LeadCommentTokenBuilder("'")

    directives = (
      '#If', '#Else', '#ElseIf', '#End If',
      '#ExternalSource',
      '#Line', '#Region', '#End Region',
      '#Const'
    )

    c_preprocessor_tb = CPreProcessorTokenBuilder(directives)

    known_operators = [
      '&', '&=', '*', '*=', '/', '/=', '\\', '\\=', '^', '^=',
      '+', '+=', '-', '-=', '>>', '>>=', '<<', '<<=',
      '.', '=', '<', '<=', '>', '>=', '<>',
      'AddressOf', 'And', 'AndAlso', 'Is', 'IsNot', 'Like',
      'Or', 'OrElse', 'Xor'
    ]

    self.unary_operators = [
      '+', '-', 'Not', 'IsNot'
    ]

    continuation_tb = ListTokenBuilder(['_'], 'line continuation', False)

    groupers = ['(', ')', ',', '[', ']']

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
      'If', 'Implements', 'Imports', 'In', 'Inherits', 'Interface',
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
      'SyncLock', 'Then', 'Throw', 'To', 'True', 'Try', 'TryCast',
      'TypeOf…Is', 'Using',
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
      'MyBase', 'MyClass', 'Object',
      'SByte', 'Short', 'Single', 'String',
      'UInteger', 'ULong', 'UShort',
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'False', 'True', 'Nothing'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      continuation_tb,
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
      c_preprocessor_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.ConvertKeywordsToIdentifiers()
    self.convert_functions_to_identifiers()

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence()
    # self.calc_operand_confidence()
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
