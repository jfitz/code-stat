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
  ListTokenBuilder,
  LeadCommentTokenBuilder
)
from VisualBasicTokenBuilders import (
  VisualBasicVariableTokenBuilder,
  RemarkTokenBuilder
)
from Tokenizer import Tokenizer

class VisualBasic6Examiner(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    variable_tb = VisualBasicVariableTokenBuilder(['$', '%', '#', '!'])
    identifier_tb = IdentifierTokenBuilder()
    string_tb = StringTokenBuilder(['"'], False, False, False)

    remark_tb = RemarkTokenBuilder()
    comment_tb = LeadCommentTokenBuilder("'")

    known_operators = [
      '+', '-', '*', '/', '\\', 'Mod', '^', '&',
      '>', '>=', '<', '<=', '<>', '=',
      'And', 'Or', 'Eqv', 'Is', 'Imp', 'Like', 'Not', 'Xor',
      '.'
    ]

    self.unary_operators = [
      '+', '-', 'Not'
    ]

    groupers = ['(', ')', ',', '[', ']']
    group_ends = [')', ']']

    groupers_tb = ListTokenBuilder(groupers, 'group', False)

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    keywords = [
      'Access', 'Alias', 'Any',
      'AppActivate', 'Append', 'AppendChunk', 'Arrange', 'As',
      'Beep', 'BeginTrans', 'ByVal', 'Call', 'Case', 'Circle', 'Clear',
      'Close', 'Cls', 'CommitTrans',
      'Compare', 'Const', 'Controls', 'CreateDynaset',
      'Data',
      'DateSerial', 'DateValue', 'Declare', 'DefCur', 'DefDbl',
      'DefInt', 'DefLng', 'DefSng', 'DefStr', 'DefVar', 'Delete', 'Dim',
      'Do', 'DoEvents', 'Drag', 'Edit',
      'Else', 'ElseIf', 'End', 'EndDoc', 'EndIf',
      'Erase', 'ExecuteSQL', 'Exit',
      'Explicit', 'FieldSize', 'FileAttr', 'FileCopy', 'FileDateTime',
      'Fix', 'For', 'Form', 'Format', 'Format$', 'Forms',
      'Function', 'Get', 'GetAttr', 'GetChunk', 'GetData',
      'GetFormat', 'GetText', 'Global', 'GoSub', 'GoTo',
      'Hide', 'If', 'Input', 'Input$', 'InputBox', 'InputBox$',
      'Kill', 
      'Let', 'Lib', 'Line', 'LinkExecute', 'LinkPoke',
      'LinkRequest', 'LinkSend', 'Load', 'LoadPicture', 'Loc', 'Local',
      'Lock', 'LOF', 'Loop', 'LSet',
      'MkDir', 'Move',
      'MoveFirst', 'MoveLast', 'MoveNext', 'MovePrevious', 'MoveRelative',
      'MsgBox', 'Name', 'New', 'NewPage', 'Next', 'NextBlock',
      'On', 'Open', 'OpenDataBase', 'Option',
      'Output', 'Point', 'Preserve', 'Print', 'PrintForm',
      'Private', 'PSet', 'Put', 'QBColor', 'Random', 'Randomize', 'Read',
      'ReDim', 'Refresh', 'RegisterDataBase', 'Rem', 'RemoveItem', 'Reset',
      'Restore', 'Resume', 'Return', 'RmDir',
      'Rollback', 'RSet', 'SavePicture', 'Scale',
      'Seek', 'Select', 'SendKeys', 'Set', 'SetAttr', 'SetData', 'SetFocus',
      'SetText', 'Shared', 'Shell', 'Show', 'Static', 'Step', 'Stop',
      'Sub', 'System', 'Text',
      'TextHeight', 'TextWidth', 'Then', 'Timer',
      'TimeSerial', 'TimeValue', 'To', 'Type',
      'TypeOf', 'Unload', 'Unlock', 'Until',
      'Update', 'Using', 'VarType', 'Weekday', 'Wend',
      'While', 'Width', 'Write', 'ZOrder'
    ]

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    functions = [
      'Abs', 'AddItem', 'AddNew', 'Asc', 'Atn',
      'CCur',
      'CDbl', 'ChDir', 'ChDrive', 'Chr', 'Chr$', 'CInt',
      'CLng', 'Command', 'Command$', 'Cos', 'CSng',
      'CStr', 'CurDir$', 'CVar', 'CVDate', 'Date', 'Date$', 'Day',
      'Dir', 'Dir$', 'Environ$', 'EOF', 'Error', 'Error$', 'Exp',
      'FileLen', 'FreeFile',
      'Hex', 'Hex$', 'Hour',
      'InStr', 'Int', 'InStrRev', 'IsDate', 'IsEmpty', 'IsNull',
      'IsNumeric',
      'Join',
      'LBound', 'LCase', 'LCase$', 'Left', 'Left$',
      'Len', 'Log', 'LTrim', 'LTrim$',
      'Mid', 'Mid$', 'Minute', 'Mod', 'Month',
      'Now', 'Oct', 'Oct$', 'RGB', 'Right', 'Right$', 'Rnd',
      'RTrim', 'RTrim$', 'Second', 'Sgn', 'Sin', 'Space',
      'Space$', 'Spc', 'Split', 'Sqr', 'Str', 'Str$', 'StrComp',
      'String$',
      'Tab', 'Tan', 'Time', 'Time$', 'Trim', 'Trim$',
      'UBound', 'UCase', 'UCase$',
      'Val', 'Year'
    ]

    function_tb = ListTokenBuilder(functions, 'function', True)

    types = [
      'Binary', 'Control', 'Currency', 'Double', 'Dynaset', 'Integer',
      'Long', 'Single', 'String', 'Variant'
    ]

    types_tb = ListTokenBuilder(types, 'type', True)

    values = [
      'False', 'True', 'App', 'Base', 'Clipboard', 'Debug', 'Erl', 'Err',
      'Printer', 'Me', 'Nothing', 'Null'
    ]

    values_tb = ListTokenBuilder(values, 'value', True)

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
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
    self.calc_statistics()
