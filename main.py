import argparse
import cProfile
import json
import codecs
from flask import Flask, request, render_template
from BasicExaminer import BasicExaminer
from CBasicExaminer import CBasicExaminer
from CExaminer import CExaminer
from CobolFixedFormatExaminer import CobolFixedFormatExaminer
from CobolFreeFormatExaminer import CobolFreeFormatExaminer
from CppExaminer import CppExaminer
from CsharpExaminer import CsharpExaminer
from FortranFixedFormatExaminer import FortranFixedFormatExaminer
from FortranFreeFormatExaminer import FortranFreeFormatExaminer
from FsharpExaminer import FsharpExaminer
from HTMLExaminer import HTMLExaminer
from JavaExaminer import JavaExaminer
from JavaScriptExaminer import JavaScriptExaminer
from ObjectiveCExaminer import ObjectiveCExaminer
from PascalExaminer import PascalExaminer
from PrologExaminer import PrologExaminer
from PythonExaminer import PythonExaminer
from RExaminer import RExaminer
from RubyExaminer import RubyExaminer
from SqlExaminer import SqlExaminer
from SwiftExaminer import SwiftExaminer
from TypeScriptExaminer import TypeScriptExaminer
from VisualBasic6Examiner import VisualBasic6Examiner
from VisualBasicNETExaminer import VisualBasicNETExaminer


def decode_bytes(in_bytes):
  text = ''
  try:
    text = in_bytes.decode('ascii')
  except UnicodeDecodeError:
    try:
      text = in_bytes.decode('utf-8')
    except UnicodeDecodeError:
      try:
        text = in_bytes.decode('utf-16')
      except UnicodeDecodeError:
        pass

  return text


app = Flask(__name__)

codesAndNames = {
  'basic': 'BASIC',
  'c': 'C',
  'cplusplus': 'C++',
  'csharp': 'C#',
  'cbasic': 'CBASIC',
  'cobol68': 'COBOL-68',
  'cobol74': 'COBOL-74',
  'cobol85': 'COBOL-85',
  'cobol2002': 'COBOL-2002',
  'cobol2014': 'COBOL-2014',
  'cobol2014acu': 'COBOL-2014-ACU',
  'cobol2014ibm': 'COBOL-2014-IBM',
  'cobol2014gnu': 'COBOL-2014-GNU',
  'fortran66': 'FORTRAN-66',
  'fortran77': 'FORTRAN-77',
  'fortran90': 'Fortran-90',
  'fortran95': 'Fortran-95',
  'fortran2003': 'Fortran-2003',
  'fortran2008': 'Fortran-2008',
  'fsharp': 'F#',
  'java': 'Java',
  'javascript': 'JavaScript',
  'html': 'HTML',
  'objectivec': 'Objective-C',
  'pascal': 'Pascal',
  'prolog': 'Prolog',
  'python': 'Python',
  'r': 'R',
  'ruby': 'Ruby',
  'sql92': 'SQL-92',
  'sql99': 'SQL-99',
  'sql2003': 'SQL-2003',
  'sql2008': 'SQL-2008',
  'sql2011': 'SQL-2011',
  'sql2016': 'SQL-2016',
  'swift': 'Swift',
  'typescript': 'TypeScript',
  'visualbasic6': 'VisualBasic-6',
  'visualbasicnet': 'VisualBasic-NET'
}

@app.route('/languages', methods=['GET'])
def languages():
  json_text = json.dumps(codesAndNames)
  return json_text


@app.route('/detab', methods=['POST'])
def detab():
  tab_size = 8

  if 'tabsize' in request.args:
    tab_size = request.args['tabsize']
 
    try:
      tab_size = int(tab_size)
    except ValueError:
      tab_size = 8

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)
  detabbed_text = tabs_to_spaces(text, tab_size)

  return detabbed_text


@app.route('/truncate', methods=['POST'])
def truncate():
  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)
  truncated_text = truncate_lines(text, 72)

  return truncated_text


@app.route('/unwrap', methods=['POST'])
def unwrap():
  language = ''
  if 'language' in request.args:
    language = request.args['language']

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)
  unwrapped_text = text
  if language.lower() == 'fortran':
    unwrapped_text = unwrap_fortran_lines(text)
  if language.lower() == 'cobol':
    unwrapped_text = unwrap_cobol_lines(text)

  return unwrapped_text


@app.route('/detect', methods=['POST'])
def detect():
  tabsize = 8
  if 'tabsize' in request.args:
    tabsize = request.args['tabsize']

  languages = []
  if 'languages' in request.args:
    languages = request.args['languages'].split(' ')

  if len(languages) == 0:
    languages = list(codesAndNames.keys())

  wide = 'wide' in request.args

  tiebreak_keywords = False
  if 'tiebreak-keywords' in request.args:
    tiebreak_keywords = True

  tiebreak_tokens = False
  if 'tiebreak-tokens' in request.args:
    tiebreak_tokens = True

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)
  detected_languages = identify_language(text, tabsize, wide, tiebreak_keywords, tiebreak_tokens, languages)

  json_text = json.dumps(detected_languages)

  return json_text


@app.route('/tokens', methods=['POST'])
def tokens():
  language = ''
  if 'language' in request.args:
    language = request.args['language']

  tabsize = 8
  if 'tabsize' in request.args:
    tabsize = request.args['tabsize']

  wide = 'wide' in request.args

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)
  tokens = tokenize(text, language.lower(), tabsize, wide)
  token_list = []

  for token in tokens:
    token_list.append(token.toDict())

  json_text = json.dumps(token_list)

  return json_text


@app.route('/confidence', methods=['POST'])
def confidence():
  language = ''
  if 'language' in request.args:
    language = request.args['language']

  tabsize = 8
  if 'tabsize' in request.args:
    tabsize = request.args['tabsize']

  wide = 'wide' in request.args

  get_errors = 'errors' in request.args

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)

  json_text = tokenize_confidence(text, language.lower(), tabsize, get_errors, wide)

  return json_text


@app.route('/statistics', methods=['POST'])
def statistics():
  language = ''
  if 'language' in request.args:
    language = request.args['language']

  tabsize = 8
  if 'tabsize' in request.args:
    tabsize = request.args['tabsize']

  wide = 'wide' in request.args

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)

  json_text = tokenize_statistics(text, language.lower(), tabsize, wide)

  return json_text


def tabs_to_spaces(text, tab_size):
  column = 0
  detabbed_text = ''

  for c in text:
    if c in ['\n', '\r']:
      detabbed_text += c
      column = 0
    else:
      if c == '\t':
        next_tab_stop = int((column + tab_size) / tab_size) * tab_size
        while column < next_tab_stop:
          detabbed_text += ' '
          column += 1
      else:
        detabbed_text += c
        column += 1
  
  return detabbed_text


def split_lines(text):
  lines = []

  line = ''
  for c in text:
    if c == '\r':
      pass
    else:
      if c == '\n':
        lines.append(line)
        line = ''
      else:
        line += c
  if len(line) > 0:
    lines.append(line)

  return lines


def truncate_lines(text, column):
  truncated_lines = ''

  # break into lines
  lines = split_lines(text)

  for line in lines:
    line = line[:column]
    truncated_lines += line
    truncated_lines += '\n'

  return truncated_lines


def unwrap_fortran_lines(text):
  unwrapped_lines = ''

  # break into lines
  lines = split_lines(text)

  buffer = None
  for line in lines:
    # rtrim
    line = line.rstrip()

    # if continuation (not comment, longer than 6, not space in column 5)
    if len(line) > 5 and line[0] != 'C' and line[5] != ' ':
      # drop leading columns
      line = line[6:]
      # append to buffer
      if buffer is None:
        buffer = line
      else:
        buffer += line
    else:
      if buffer is not None:
        unwrapped_lines += buffer
        unwrapped_lines += '\n'
      buffer = line
  if len(buffer) > 0:
    unwrapped_lines += buffer
    unwrapped_lines += '\n'

  return unwrapped_lines


def unwrap_cobol_lines(text):
  unwrapped_lines = ''

  # break into lines
  lines = split_lines(text)

  buffer = None
  for line in lines:
    # remove line description (if any)
    line = line[:72]

    # force line length to 72 (used later when continuing strings)
    while len(line) < 72:
      line += ' '

    # if continuation (not comment, longer than 6, not space in column)
    if len(line) > 6 and line[6] == '-':
      # drop leading columns
      line = line[7:]

      # append to buffer
      if buffer is None:
        buffer = line
      else:
        # drop leading spaces and the leading quote
        line = line.lstrip()

        buffer2 = buffer.rstrip()
        if len(buffer2) > 0:
          # if the buffer ends with other than quote,
          if buffer2[-1] not in "'\"":
            # combine strings by dropping this line's opening quote
            if len(line) > 0 and line[0] in "'\"":
              line = line[1:]
              buffer += line
          else:
            # previous line ends in quote
            buffer = buffer2 + ' '  # space to separate string tokens
            buffer += line
        else:
          buffer = line
    else:
      if buffer is not None:
        # now drop the extra spaces on the right
        unwrapped_lines += buffer.rstrip()
        unwrapped_lines += '\n'
      buffer = line

  if buffer is not None and len(buffer) > 0:
    unwrapped_lines += buffer
    unwrapped_lines += '\n'

  return unwrapped_lines


def identify_language(code, tabsize, wide, tiebreak_keywords, tiebreak_tokens, languages):
  try:
    tab_size = int(tabsize)
  except ValueError:
    tab_size = 8

  code = code.replace('\r\n','\n')

  examiners = {}

  if 'basic' in languages:
    examiners['BASIC'] = BasicExaminer(code)

  if 'cbasic' in languages:
    examiners['CBASIC'] = CBasicExaminer(code)

  if 'c' in languages:
    examiners['C'] = CExaminer(code)

  if 'cplusplus' in languages:
    examiners['C++'] = CppExaminer(code)

  if 'csharp' in languages:
    examiners['C#'] = CsharpExaminer(code)

  if 'cobol68' in languages:
    examiners['COBOL-68'] = CobolFixedFormatExaminer(code, '68', '', tab_size, wide)

  if 'cobol74' in languages:
    examiners['COBOL-74'] = CobolFixedFormatExaminer(code, '74', '', tab_size, wide)

  if 'cobol85' in languages:
    examiners['COBOL-85'] = CobolFixedFormatExaminer(code, '85', '', tab_size, wide)

  if 'cobol2002' in languages:
    examiners['COBOL-2002'] = CobolFreeFormatExaminer(code, '2002', '')

  if 'cobol2014' in languages:
    examiners['COBOL-2014'] = CobolFreeFormatExaminer(code, '2014', '')

  if 'cobol2014acu' in languages:
    examiners['COBOL-2014-ACU'] = CobolFreeFormatExaminer(code, '2014', 'acu')

  if 'cobol2014ibm' in languages:
    examiners['COBOL-2014-IBM'] = CobolFreeFormatExaminer(code, '2014', 'ibm')

  if 'cobol2014gnu' in languages:
    examiners['COBOL-2014-GNU'] = CobolFreeFormatExaminer(code, '2014', 'gnu')

  if 'fortran66' in languages:
    examiners['Fortran-66'] = FortranFixedFormatExaminer(code, '66', tab_size, wide)

  if 'fortran77' in languages:
    examiners['Fortran-77'] = FortranFixedFormatExaminer(code, '77', tab_size, wide)

  if 'fortran90' in languages:
    examiners['Fortran-90'] = FortranFreeFormatExaminer(code, '90')

  if 'fortran95' in languages:
    examiners['Fortran-95'] = FortranFreeFormatExaminer(code, '95')

  if 'fortran2003' in languages:
    examiners['Fortran-2003'] = FortranFreeFormatExaminer(code, '2003')

  if 'fortran2008' in languages:
    examiners['Fortran-2008'] = FortranFreeFormatExaminer(code, '2008')

  if 'fsharp' in languages:
    examiners['F#'] = FsharpExaminer(code)

  if 'html' in languages:
    examiners['HTML'] = HTMLExaminer(code)

  if 'objectivec' in languages:
    examiners['Objective-C'] = ObjectiveCExaminer(code)

  if 'java' in languages:
    examiners['Java'] = JavaExaminer(code)

  if 'javascript' in languages:
    examiners['JavaScript'] = JavaScriptExaminer(code)

  if 'pascal' in languages:
    examiners['Pascal'] = PascalExaminer(code)

  if 'prolog' in languages:
    examiners['Prolog'] = PrologExaminer(code)

  if 'python' in languages:
    examiners['Python'] = PythonExaminer(code)

  if 'r' in languages:
    examiners['R'] = RExaminer(code)

  if 'ruby' in languages:
    examiners['Ruby'] = RubyExaminer(code)

  if 'sql92' in languages:
    examiners['SQL-92'] = SqlExaminer(code, '92', '')

  if 'sql99' in languages:
    examiners['SQL-99'] = SqlExaminer(code, '99', '')

  if 'sql2003' in languages:
    examiners['SQL-2003'] = SqlExaminer(code, '2003', '')

  if 'sql2008' in languages:
    examiners['SQL-2008'] = SqlExaminer(code, '2008', '')

  if 'sql2011' in languages:
    examiners['SQL-2011'] = SqlExaminer(code, '2011', '')

  if 'sql2016' in languages:
    examiners['SQL-2016'] = SqlExaminer(code, '2016', '')

  if 'swift' in languages:
    examiners['Swift'] = SwiftExaminer(code)

  if 'typescript' in languages:
    examiners['TypeScript'] = TypeScriptExaminer(code)

  if 'visualbasic6' in languages:
    examiners['VisualBasic-6'] = VisualBasic6Examiner(code)

  if 'visualbasicnet' in languages:
    examiners['VisualBasic-NET'] = VisualBasicNETExaminer(code)

  # get confidence values
  retval = {}
  highest_confidence = 0
  for name in examiners:
    confidence = examiners[name].confidence()
    retval[name] = confidence
    # find the highest value
    if confidence > highest_confidence:
      highest_confidence = confidence

  if tiebreak_keywords:
    # count how many have the greatest confidence
    high_names = []
    for name in examiners:
      confidence = examiners[name].confidence()
      if confidence == highest_confidence:
        high_names.append(name)

    # if a tie among multiple examiners
    if len(high_names) > 1:
      highest_keyword_count = 0
      for name in high_names:
        keyword_count = len(examiners[name].find_keywords())
        if keyword_count > highest_keyword_count:
          highest_keyword_count = keyword_count

      if highest_keyword_count > 0:
        # assign confidence to number of keywords (more is better)
        for name in high_names:
          count = len(examiners[name].find_keywords())
          keyword_count_confidence = count / highest_keyword_count
          examiners[name].confidences['keyword_count'] = keyword_count_confidence

        # recalculate confidence with new factor
        for name in high_names:
          confidence = examiners[name].confidence()
          retval[name] = confidence

  if tiebreak_tokens:
    # count how many have the greatest confidence
    high_names = []
    for name in examiners:
      confidence = examiners[name].confidence()
      if confidence == highest_confidence:
        high_names.append(name)

    # if a tie among multiple examiners
    if len(high_names) > 1:
      lowest_token_count = None
      for name in high_names:
        count = len(examiners[name].tokens)
        if lowest_token_count is None or count < lowest_token_count:
          lowest_token_count = count

      # assign confidence to number of tokens (fewer tokens are better)
      for name in high_names:
        count = len(examiners[name].tokens)
        token_count_confidence = lowest_token_count / count
        examiners[name].confidences['token_count'] = token_count_confidence

      # recalculate confidence with new factor
      for name in high_names:
        confidence = examiners[name].confidence()
        retval[name] = confidence

  return retval

def tokenize(code, language, tabsize, wide):
  try:
    tab_size = int(tabsize)
  except ValueError:
    tab_size = 8

  tokens = []

  if language in ['basic', 'bas']:
    examiner = BasicExaminer(code)
    tokens = examiner.tokens

  if language in ['c']:
    examiner = CExaminer(code)
    tokens = examiner.tokens

  if language in ['c++', 'cplusplus']:
    examiner = CppExaminer(code)
    tokens = examiner.tokens

  if language in ['c#', 'csharp']:
    examiner = CsharpExaminer(code)
    tokens = examiner.tokens

  if language in ['cbasic']:
    examiner = CBasicExaminer(code)
    tokens = examiner.tokens

  if language in ['cobol-68']:
    examiner = CobolFixedFormatExaminer(code, '68', '', tab_size, wide)
    tokens = examiner.tokens

  if language in ['cobol-74']:
    examiner = CobolFixedFormatExaminer(code, '74', '', tab_size, wide)
    tokens = examiner.tokens

  if language in ['cobol-85']:
    examiner = CobolFixedFormatExaminer(code, '85', '', tab_size, wide)
    tokens = examiner.tokens

  if language in ['cobol-2002']:
    examiner = CobolFreeFormatExaminer(code, '2002', '')
    tokens = examiner.tokens

  if language in ['cobol-2014']:
    examiner = CobolFreeFormatExaminer(code, '2014', '')
    tokens = examiner.tokens

  if language in ['cobol-2014-acu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'acu')
    tokens = examiner.tokens

  if language in ['cobol-2014-ibm']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'ibm')
    tokens = examiner.tokens

  if language in ['cobol-2014-gnu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'gnu')
    tokens = examiner.tokens

  if language in ['fortran', 'for', 'ftn', 'fortran-66']:
    examiner = FortranFixedFormatExaminer(code, '66', tab_size, wide)
    tokens = examiner.tokens

  if language in ['f77', 'fortran-77']:
    examiner = FortranFixedFormatExaminer(code, '77', tab_size, wide)
    tokens = examiner.tokens

  if language in ['f90', 'fortran-90']:
    examiner = FortranFreeFormatExaminer(code, '90')
    tokens = examiner.tokens

  if language in ['f95', 'fortran-95']:
    examiner = FortranFreeFormatExaminer(code, '95')
    tokens = examiner.tokens

  if language in ['f03', 'fortran-2003']:
    examiner = FortranFreeFormatExaminer(code, '2003')
    tokens = examiner.tokens

  if language in ['f08', 'fortran-2008']:
    examiner = FortranFreeFormatExaminer(code, '2008')
    tokens = examiner.tokens

  if language in ['fs', 'fsharp']:
    examiner = FsharpExaminer(code)
    tokens = examiner.tokens

  if language in ['html', 'php']:
    examiner = HTMLExaminer(code)
    tokens = examiner.tokens

  if language in ['java', 'jav']:
    examiner = JavaExaminer(code)
    tokens = examiner.tokens

  if language in ['javascript', 'js']:
    examiner = JavaScriptExaminer(code)
    tokens = examiner.tokens

  if language in ['objective-c', 'objc']:
    examiner = ObjectiveCExaminer(code)
    tokens = examiner.tokens

  if language in ['pascal', 'pas']:
    examiner = PascalExaminer(code)
    tokens = examiner.tokens

  if language in ['prolog']:
    examiner = PrologExaminer(code)
    tokens = examiner.tokens

  if language in ['python', 'py']:
    examiner = PythonExaminer(code)
    tokens = examiner.tokens

  if language in ['r', 'rmd']:
    examiner = RExaminer(code)
    tokens = examiner.tokens

  if language in ['ruby', 'rb']:
    examiner = RubyExaminer(code)
    tokens = examiner.tokens

  if language in ['sql-92']:
    examiner = SqlExaminer(code, '92', '')
    tokens = examiner.tokens

  if language in ['sql-99']:
    examiner = SqlExaminer(code, '99', '')
    tokens = examiner.tokens

  if language in ['sql-2003']:
    examiner = SqlExaminer(code, '2003', '')
    tokens = examiner.tokens

  if language in ['sql-2008']:
    examiner = SqlExaminer(code, '2008', '')
    tokens = examiner.tokens

  if language in ['sql-2011']:
    examiner = SqlExaminer(code, '2011', '')
    tokens = examiner.tokens

  if language in ['sql-2016']:
    examiner = SqlExaminer(code, '2016', '')
    tokens = examiner.tokens

  if language in ['swift']:
    examiner = SwiftExaminer(code)
    tokens = examiner.tokens

  if language in ['typescript']:
    examiner = TypeScriptExaminer(code)
    tokens = examiner.tokens

  if language in ['visualbasic-6']:
    examiner = VisualBasic6Examiner(code)
    tokens = examiner.tokens

  if language in ['visualbasic-net']:
    examiner = VisualBasicNETExaminer(code)
    tokens = examiner.tokens

  return tokens


def tokenize_confidence(code, language, tabsize, get_errors, wide):
  try:
    tab_size = int(tabsize)
  except ValueError:
    tab_size = 8

  confidences = {}
  errors = []

  if language in ['basic', 'bas']:
    examiner = BasicExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['c']:
    examiner = CExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['c++', 'cplusplus']:
    examiner = CppExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['c#', 'csharp']:
    examiner = CsharpExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cbasic']:
    examiner = CBasicExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-68']:
    examiner = CobolFixedFormatExaminer(code, '68', '', tab_size, wide)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-74']:
    examiner = CobolFixedFormatExaminer(code, '74', '', tab_size, wide)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-85', 'cobol', 'cob', 'cbl']:
    examiner = CobolFixedFormatExaminer(code, '85', '', tab_size, wide)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-2002']:
    examiner = CobolFreeFormatExaminer(code, '2002', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-2014']:
    examiner = CobolFreeFormatExaminer(code, '2014', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-2014-acu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'acu')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-2014-ibm']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'ibm')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-2014-gnu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'gnu')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['fortran', 'for', 'ftn', 'fortran-66']:
    examiner = FortranFixedFormatExaminer(code, '66', tab_size, wide)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['f77', 'fortran-77']:
    examiner = FortranFixedFormatExaminer(code, '77', tab_size, wide)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['f90', 'fortran-90']:
    examiner = FortranFreeFormatExaminer(code, '90')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['f95', 'fortran-95']:
    examiner = FortranFreeFormatExaminer(code, '95')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['f03', 'fortran-2003']:
    examiner = FortranFreeFormatExaminer(code, '2003')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['f08', 'fortran-2008']:
    examiner = FortranFreeFormatExaminer(code, '2008')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['fs', 'fsharp']:
    examiner = FsharpExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['html', 'php']:
    examiner = HTMLExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['java', 'jav']:
    examiner = JavaExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['javascript', 'js']:
    examiner = JavaScriptExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['objective-c', 'objc']:
    examiner = ObjectiveCExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['pascal', 'pas']:
    examiner = PascalExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['prolog']:
    examiner = PrologExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['python', 'py']:
    examiner = PythonExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['r', 'rmd']:
    examiner = RExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['ruby', 'rb']:
    examiner = RubyExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['sql-92']:
    examiner = SqlExaminer(code, '92', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['sql-99']:
    examiner = SqlExaminer(code, '99', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['sql-2003']:
    examiner = SqlExaminer(code, '2003', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['sql-2008']:
    examiner = SqlExaminer(code, '2008', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['sql-2011']:
    examiner = SqlExaminer(code, '2011', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['sql-2016']:
    examiner = SqlExaminer(code, '2016', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['swift']:
    examiner = SwiftExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['typescript']:
    examiner = TypeScriptExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['visualbasic-6']:
    examiner = VisualBasic6Examiner(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['visualbasic-net']:
    examiner = VisualBasicNETExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if get_errors:
    retval = json.dumps(errors)
  else:
    retval = json.dumps(confidences)

  return retval


def tokenize_statistics(code, language, tabsize, wide):
  try:
    tab_size = int(tabsize)
  except ValueError:
    tab_size = 8

  statistics = {}

  if language in ['basic', 'bas']:
    examiner = BasicExaminer(code)
    statistics = examiner.statistics

  if language in ['c']:
    examiner = CExaminer(code)
    statistics = examiner.statistics

  if language in ['c++', 'cplusplus']:
    examiner = CppExaminer(code)
    statistics = examiner.statistics

  if language in ['c#', 'csharp']:
    examiner = CsharpExaminer(code)
    statistics = examiner.statistics

  if language in ['cbasic']:
    examiner = CBasicExaminer(code)
    statistics = examiner.statistics

  if language in ['cobol-68']:
    examiner = CobolFixedFormatExaminer(code, '68', '', tab_size, wide)
    statistics = examiner.statistics

  if language in ['cobol-74']:
    examiner = CobolFixedFormatExaminer(code, '74', '', tab_size, wide)
    statistics = examiner.statistics

  if language in ['cobol-85', 'cobol', 'cob', 'cbl']:
    examiner = CobolFixedFormatExaminer(code, '85', '', tab_size, wide)
    statistics = examiner.statistics

  if language in ['cobol-2002']:
    examiner = CobolFreeFormatExaminer(code, '2002', '')
    statistics = examiner.statistics

  if language in ['cobol-2014']:
    examiner = CobolFreeFormatExaminer(code, '2014', '')
    statistics = examiner.statistics

  if language in ['cobol-2014-acu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'acu')
    statistics = examiner.statistics

  if language in ['cobol-2014-ibm']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'ibm')
    statistics = examiner.statistics

  if language in ['cobol-2014-gnu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'gnu')
    statistics = examiner.statistics

  if language in ['fortran', 'for', 'ftn', 'fortran-66']:
    examiner = FortranFixedFormatExaminer(code, '66', tab_size, wide)
    statistics = examiner.statistics

  if language in ['f77', 'fortran-77']:
    examiner = FortranFixedFormatExaminer(code, '77', tab_size, wide)
    statistics = examiner.statistics

  if language in ['f90', 'fortran-90']:
    examiner = FortranFreeFormatExaminer(code, '90')
    statistics = examiner.statistics

  if language in ['f95', 'fortran-95']:
    examiner = FortranFreeFormatExaminer(code, '95')
    statistics = examiner.statistics

  if language in ['f03', 'fortran-2003']:
    examiner = FortranFreeFormatExaminer(code, '2003')
    statistics = examiner.statistics

  if language in ['f08', 'fortran-2008']:
    examiner = FortranFreeFormatExaminer(code, '2008')
    statistics = examiner.statistics

  if language in ['fs', 'fsharp']:
    examiner = FsharpExaminer(code)
    statistics = examiner.statistics

  if language in ['html', 'php']:
    examiner = HTMLExaminer(code)
    statistics = examiner.statistics

  if language in ['java', 'jav']:
    examiner = JavaExaminer(code)
    statistics = examiner.statistics

  if language in ['javascript', 'js']:
    examiner = JavaScriptExaminer(code)
    statistics = examiner.statistics

  if language in ['objective-c', 'objc']:
    examiner = ObjectiveCExaminer(code)
    statistics = examiner.statistics

  if language in ['pascal', 'pas']:
    examiner = PascalExaminer(code)
    statistics = examiner.statistics

  if language in ['prolog']:
    examiner = PrologExaminer(code)
    statistics = examiner.statistics

  if language in ['python', 'py']:
    examiner = PythonExaminer(code)
    statistics = examiner.statistics

  if language in ['r', 'rmd']:
    examiner = RExaminer(code)
    statistics = examiner.statistics

  if language in ['ruby', 'rb']:
    examiner = RubyExaminer(code)
    statistics = examiner.statistics

  if language in ['sql-92']:
    examiner = SqlExaminer(code, '92', '')
    statistics = examiner.statistics

  if language in ['sql-99']:
    examiner = SqlExaminer(code, '99', '')
    statistics = examiner.statistics

  if language in ['sql-2003']:
    examiner = SqlExaminer(code, '2003', '')
    statistics = examiner.statistics

  if language in ['sql-2008']:
    examiner = SqlExaminer(code, '2008', '')
    statistics = examiner.statistics

  if language in ['sql-2011']:
    examiner = SqlExaminer(code, '2011', '')
    statistics = examiner.statistics

  if language in ['sql-2016']:
    examiner = SqlExaminer(code, '2016', '')
    statistics = examiner.statistics

  if language in ['swift']:
    examiner = SwiftExaminer(code)
    statistics = examiner.statistics

  if language in ['typescript']:
    examiner = TypeScriptExaminer(code)
    statistics = examiner.statistics

  if language in ['visualbasic-6']:
    examiner = VisualBasic6Examiner(code)
    statistics = examiner.statistics

  if language in ['visualbasic-net']:
    examiner = VisualBasicNETExaminer(code)
    statistics = examiner.statistics

  retval = json.dumps(statistics)

  return retval


if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  parser.add_argument('--tokenize', action='store', dest='tokenize')
  parser.add_argument('--confidence', action='store', dest='confidence')
  args = parser.parse_args()

  if args.tokenize is not None:
    print('Tokenizing file ' + args.tokenize)
    in_file = open(args.tokenize, 'r')
    code = in_file.read()
    in_file.close()
    language = 'ruby'
    tabsize = 4
    cProfile.run('tokenize(code, language, tabsize)')
  else:
    if args.confidence is not None:
      print('Confidencing file ' + args.confidence)
      in_file = open(args.confidence, 'r')
      code = in_file.read()
      in_file.close()
      language = 'ruby'
      tabsize = 4
      cProfile.run('tokenize_confidence(code, language, tabsize)')
    else:
      app.run()
