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

class VisualBasic6Examiner(Examiner):
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

    operand_types =[]

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
    group_starts = ['(', '[', ',']
    group_mids = [',']
    group_ends = [')', ']']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

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

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

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

    function_tb = CaseSensitiveListTokenBuilder(functions, 'common function', True)
    operand_types.append('common function')
    operand_types.append('function')

    types = [
      'Binary', 'Control', 'Currency', 'Double', 'Dynaset', 'Integer',
      'Long', 'Single', 'String', 'Variant'
    ]

    types_tb = CaseSensitiveListTokenBuilder(types, 'type', True)
    operand_types.append('type')

    values = [
      'False', 'True', 'App', 'Base', 'Clipboard', 'Debug', 'Erl', 'Err',
      'Printer', 'Me', 'Nothing', 'Null'
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
    # self.convert_identifiers_to_functions()
    # self.convert_functions_to_common_functions(functions)

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()
    self.calc_line_continuation_confidence(tokens)

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

    self.no_keyword_confidence = 0.2
    self.calc_keyword_confidence()

    self.calc_line_format_confidence()
    self.calc_line_length_confidence(code, self.max_expected_line)


  def calc_line_format_confidence(self):
    # remove tokens we don't care about
    drop_types = ['whitespace', 'comment', 'EOF']
    tokens = Examiner.drop_tokens(self.tokens, drop_types)

    # join continued lines
    tokens = self.join_continued_lines(tokens)

    # split tokens by lines
    lines = self.split_tokens_into_lines(tokens)

    # check that line that begin with 'if' or 'elseif' end with 'then'
    num_lines = len(lines)
    num_lines_correct = 0

    for line in lines:
      if len(line) > 0:
        if line[0].text.lower() in ['if', 'endif']:
          if line[-1].text.lower() == 'then':
            num_lines_correct += 1
          else:
            self.errors.append({
              'TYPE': 'LINE FORMAT',
              'FIRST': line[0].text,
              'SECOND': line[-1].text
            })
        else:
          num_lines_correct += 1
      else:
        num_lines_correct += 1

    line_format_confidence = 1.0
    if num_lines > 0:
      line_format_confidence = num_lines_correct / num_lines

    self.confidences['line format'] = line_format_confidence

    return tokens
