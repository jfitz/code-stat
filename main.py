import json
import codecs
from flask import Flask, request, render_template
from BasicExaminer import BasicExaminer
from CExaminer import CExaminer
from CobolExaminer import CobolExaminer
from CppExaminer import CppExaminer
from CsharpExaminer import CsharpExaminer
from Fortran66Examiner import Fortran66Examiner
from Fortran77Examiner import Fortran77Examiner
from Fortran90Examiner import Fortran90Examiner
from JavaExaminer import JavaExaminer
from PascalExaminer import PascalExaminer
from PrologExaminer import PrologExaminer
from PythonExaminer import PythonExaminer
from RubyExaminer import RubyExaminer


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
    'Fixed-length COBOL',
    'Free-form COBOL',
    'FORTRAN-66',
    'FORTRAN-77',
    'Fortran-90',
    'Java',
    'Pascal',
    'Prolog',
    'Python',
    'Ruby'
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

  json_text = tokenize_confidence(text, language.lower(), tabsize)

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
  examiners['Fixed-Format-COBOL'] = CobolExaminer(code, True, tab_size)
  examiners['Free-Format-COBOL'] = CobolExaminer(code, False, tab_size)
  examiners['Fortran-66'] = Fortran66Examiner(code,tab_size)
  examiners['Fortran-77'] = Fortran77Examiner(code,tab_size)
  examiners['Fortran-90'] = Fortran90Examiner(code,tab_size)
  examiners['Java'] = JavaExaminer(code)
  examiners['Pascal'] = PascalExaminer(code)
  examiners['Prolog'] = PrologExaminer(code)
  examiners['Python'] = PythonExaminer(code)
  examiners['Ruby'] = RubyExaminer(code)

  # store confidence values
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
    # for each with the high value, get the confidences
    highest_keyword_value = 0
    for name in high_names:
      confidences = examiners[name].confidences()
      keyword_confidence = confidences['keyword']
      if keyword_confidence > highest_keyword_value:
        highest_keyword_value = keyword_confidence

    # increase all values by respective keyword confidence
    # then reduce by the highest keyword confidence
    for name in high_names:
      confidences = examiners[name].confidences()
      keyword_confidence = confidences['keyword']
      retval[name] += keyword_confidence - highest_keyword_value

      # constrain to [0.0, 1.0]
      if retval[name] > 1.0:
        retval[name] = 1.0
      if retval[name] < 0.0:
        retval[name] = 0.0

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

  if language in ['fixed-format-cobol', 'cobol', 'cob', 'cbl']:
    examiner = CobolExaminer(code, True, tab_size)
    tokens = examiner.tokens

  if language in ['free-format-cobol']:
    examiner = CobolExaminer(code, False, tab_size)
    tokens = examiner.tokens

  if language in ['fortran', 'for', 'ftn', 'fortran-66']:
    examiner = Fortran66Examiner(code, tab_size)
    tokens = examiner.tokens

  if language in ['f77', 'fortran-77']:
    examiner = Fortran77Examiner(code, tab_size)
    tokens = examiner.tokens

  if language in ['f90', 'fortran-90']:
    examiner = Fortran90Examiner(code, tab_size)
    tokens = examiner.tokens

  if language in ['java', 'jav']:
    examiner = JavaExaminer(code)
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

  if language in ['ruby', 'rb']:
    examiner = RubyExaminer(code)
    tokens = examiner.tokens

  return tokens


def tokenize_confidence(code, language, tabsize):
  try:
    tab_size = int(tabsize)
  except ValueError:
    tab_size = 8

  confidences = {}

  if language in ['basic', 'bas']:
    examiner = BasicExaminer(code)
    confidences = examiner.confidences()

  if language in ['c']:
    examiner = CExaminer(code)
    confidences = examiner.confidences()

  if language in ['c++', 'cpp']:
    examiner = CppExaminer(code)
    confidences = examiner.confidences()

  if language in ['c#', 'csharp']:
    examiner = CsharpExaminer(code)
    confidences = examiner.confidences()

  if language in ['fixed-format-cobol', 'cobol', 'cob', 'cbl']:
    examiner = CobolExaminer(code, True, tab_size)
    confidences = examiner.confidences()

  if language in ['free-format-cobol']:
    examiner = CobolExaminer(code, False, tab_size)
    confidences = examiner.confidences()

  if language in ['fortran', 'for', 'ftn', 'fortran-66']:
    examiner = Fortran66Examiner(code, tab_size)
    confidences = examiner.confidences()

  if language in ['f77', 'fortran-77']:
    examiner = Fortran77Examiner(code, tab_size)
    confidences = examiner.confidences()

  if language in ['f90', 'fortran-90']:
    examiner = Fortran90Examiner(code, tab_size)
    confidences = examiner.confidences()

  if language in ['java', 'jav']:
    examiner = JavaExaminer(code)
    confidences = examiner.confidences()

  if language in ['pascal', 'pas']:
    examiner = PascalExaminer(code)
    confidences = examiner.confidences()

  if language in ['prolog']:
    examiner = PrologExaminer(code)
    confidences = examiner.confidences()

  if language in ['python', 'py']:
    examiner = PythonExaminer(code)
    confidences = examiner.confidences()

  if language in ['ruby', 'rb']:
    examiner = RubyExaminer(code)
    confidences = examiner.confidences()

  retval = json.dumps(confidences)

  return retval

if __name__ == '__main__':
  app.run()
