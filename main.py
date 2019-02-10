import argparse
import cProfile
import json
import codecs
from flask import Flask, request, render_template
from BasicExaminer import BasicExaminer
from CExaminer import CExaminer
from Cobol68Examiner import Cobol68Examiner
from Cobol74Examiner import Cobol74Examiner
from Cobol85Examiner import Cobol85Examiner
from Cobol2002Examiner import Cobol2002Examiner
from CppExaminer import CppExaminer
from CsharpExaminer import CsharpExaminer
from Fortran66Examiner import Fortran66Examiner
from Fortran77Examiner import Fortran77Examiner
from Fortran90Examiner import Fortran90Examiner
from Fortran95Examiner import Fortran95Examiner
from JavaExaminer import JavaExaminer
from JavaScriptExaminer import JavaScriptExaminer
from ObjectiveCExaminer import ObjectiveCExaminer
from PascalExaminer import PascalExaminer
from PrologExaminer import PrologExaminer
from PythonExaminer import PythonExaminer
from RExaminer import RExaminer
from RubyExaminer import RubyExaminer
from SwiftExaminer import SwiftExaminer


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

@app.route('/languages', methods=['GET'])
def languages():
  names = [
    'BASIC',
    'C',
    'C++',
    'C#',
    'COBOL-68',
    'COBOL-74',
    'COBOL-85',
    'COBOL-2002',
    'FORTRAN-66',
    'FORTRAN-77',
    'Fortran-90',
    'Fortran-95',
    'Java',
    'JavaScript',
    'Objective-C',
    'Pascal',
    'Prolog',
    'Python',
    'Ruby',
    'Swift'
  ]

  json_text = json.dumps(names)
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

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)
  detected_languages = identify_language(text, tabsize)

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

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)
  tokens = tokenize(text, language.lower(), tabsize)
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

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)

  get_errors = False
  if 'errors' in request.args:
    get_errors = True

  json_text = tokenize_confidence(text, language.lower(), tabsize, get_errors)

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
      line = line[5:]
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
    # rtrim
    line = line.rstrip()

    # if continuation (not comment, longer than 6, not space in column)
    if len(line) > 6 and line[6] == '-':
      # drop leading columns
      line = line[7:]
      # append to buffer
      if buffer is None:
        buffer = line
      else:
        # for COBOL, drop leading spaces and the leading quote
        line = line.lstrip()
        if len(line) > 0 and line[0] in "'\"":
          line = line[1:]
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


def identify_language(code, tabsize):
  try:
    tab_size = int(tabsize)
  except ValueError:
    tab_size = 8

  code = code.replace('\r\n','\n')

  examiners = {}
  examiners['BASIC'] = BasicExaminer(code)
  examiners['C'] = CExaminer(code)
  examiners['C++'] = CppExaminer(code)
  examiners['C#'] = CsharpExaminer(code)
  examiners['COBOL-68'] = Cobol68Examiner(code, tab_size)
  examiners['COBOL-74'] = Cobol74Examiner(code, tab_size)
  examiners['COBOL-85'] = Cobol85Examiner(code, tab_size)
  examiners['COBOL-2002'] = Cobol2002Examiner(code)
  examiners['Fortran-66'] = Fortran66Examiner(code, tab_size)
  examiners['Fortran-77'] = Fortran77Examiner(code, tab_size)
  examiners['Fortran-90'] = Fortran90Examiner(code)
  examiners['Fortran-95'] = Fortran95Examiner(code)
  examiners['Objective-C'] = ObjectiveCExaminer(code)
  examiners['Java'] = JavaExaminer(code)
  examiners['JavaScript'] = JavaScriptExaminer(code)
  examiners['Pascal'] = PascalExaminer(code)
  examiners['Prolog'] = PrologExaminer(code)
  examiners['Python'] = PythonExaminer(code)
  examiners['R'] = RExaminer(code)
  examiners['Ruby'] = RubyExaminer(code)
  examiners['Swift'] = SwiftExaminer(code)

  # get confidence values
  retval = {}
  highest_confidence = 0
  for name in examiners:
    confidence = examiners[name].confidence()
    retval[name] = confidence
    # find the highest value
    if confidence > highest_confidence:
      highest_confidence = confidence

  # count how many have the greatest value
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

  # count how many have the greatest value after token count confidence
  high_names = []
  for name in examiners:
    confidence = examiners[name].confidence()
    if confidence == highest_confidence:
      high_names.append(name)

  # if still a tie among multiple examiners
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

  return retval

def tokenize(code, language, tabsize):
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

  if language in ['c++', 'cpp']:
    examiner = CppExaminer(code)
    tokens = examiner.tokens

  if language in ['c#', 'csharp']:
    examiner = CsharpExaminer(code)
    tokens = examiner.tokens

  if language in ['cobol-68']:
    examiner = Cobol68Examiner(code, tab_size)
    tokens = examiner.tokens

  if language in ['cobol-74']:
    examiner = Cobol74Examiner(code, tab_size)
    tokens = examiner.tokens

  if language in ['cobol-85', 'cobol', 'cob', 'cbl']:
    examiner = Cobol85Examiner(code, tab_size)
    tokens = examiner.tokens

  if language in ['cobol-2002']:
    examiner = Cobol2002Examiner(code)
    tokens = examiner.tokens

  if language in ['fortran', 'for', 'ftn', 'fortran-66']:
    examiner = Fortran66Examiner(code, tab_size)
    tokens = examiner.tokens

  if language in ['f77', 'fortran-77']:
    examiner = Fortran77Examiner(code, tab_size)
    tokens = examiner.tokens

  if language in ['f90', 'fortran-90']:
    examiner = Fortran90Examiner(code)
    tokens = examiner.tokens

  if language in ['f95', 'fortran-95']:
    examiner = Fortran90Examiner(code)
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

  if language in ['swift']:
    examiner = SwiftExaminer(code)
    tokens = examiner.tokens

  return tokens


def tokenize_confidence(code, language, tabsize, get_errors):
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

  if language in ['c++', 'cpp']:
    examiner = CppExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['c#', 'csharp']:
    examiner = CsharpExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-68']:
    examiner = Cobol68Examiner(code, tab_size)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-74']:
    examiner = Cobol74Examiner(code, tab_size)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-85', 'cobol', 'cob', 'cbl']:
    examiner = Cobol85Examiner(code, tab_size)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-2002']:
    examiner = Cobol2002Examiner(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['fortran', 'for', 'ftn', 'fortran-66']:
    examiner = Fortran66Examiner(code, tab_size)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['f77', 'fortran-77']:
    examiner = Fortran77Examiner(code, tab_size)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['f90', 'fortran-90']:
    examiner = Fortran90Examiner(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['f95', 'fortran-95']:
    examiner = Fortran90Examiner(code)
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

  if language in ['swift']:
    examiner = SwiftExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if get_errors:
    retval = json.dumps(errors)
  else:
    retval = json.dumps(confidences)

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
