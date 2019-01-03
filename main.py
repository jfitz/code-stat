import json
import codecs
from flask import Flask, request, render_template
from BasicExaminer import BasicExaminer
from PascalExaminer import PascalExaminer
from CobolExaminer import CobolExaminer
from CExaminer import CExaminer
from CppExaminer import CppExaminer
from PythonExaminer import PythonExaminer
from Fortran66Examiner import Fortran66Examiner
from Fortran77Examiner import Fortran77Examiner

app = Flask(__name__)

@app.route('/languages', methods=['GET'])
def languages():
  names = [
    'BASIC',
    'C',
    'C++',
    'Fixed-length COBOL',
    'Free-form COBOL',
    'FORTRAN-66',
    'FORTRAN-77',
    'Pascal',
    'Python'
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
  text = request_bytes.decode('ascii')
  detabbed_text = tabs_to_spaces(text, tab_size)

  return detabbed_text

@app.route('/detect', methods=['POST'])
def detect():
  tabsize = 8
  if 'tabsize' in request.args:
    tabsize = request.args['tabsize']

  request_bytes = request.get_data()
  text = request_bytes.decode('ascii')
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
  text = request_bytes.decode('ascii')
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
  text = request_bytes.decode('ascii')

  json_text = tokenize_confidence(text, language.lower(), tabsize)

  return json_text


def tabs_to_spaces(text, tab_size):
  column = 0
  detabbed_text = ''

  for c in text:
    if c == '\n':
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

  retval = {}

  code = code.replace('\r\n','\n')

  basic_examiner = BasicExaminer(code)
  retval['BASIC'] = basic_examiner.confidence

  c_examiner = CExaminer(code)
  retval['C'] = c_examiner.confidence

  cpp_examiner = CppExaminer(code)
  retval['C++'] = cpp_examiner.confidence

  fixed_cobol_examiner = CobolExaminer(code, True, tab_size)
  retval['Fixed-Format-COBOL'] = fixed_cobol_examiner.confidence

  free_cobol_examiner = CobolExaminer(code, False, tab_size)
  retval['Free-Format-COBOL'] = free_cobol_examiner.confidence

  fortran66_examiner = Fortran66Examiner(code,tab_size)
  retval['Fortran-66'] = fortran66_examiner.confidence

  fortran77_examiner = Fortran77Examiner(code,tab_size)
  retval['Fortran-77'] = fortran77_examiner.confidence

  pascal_examiner = PascalExaminer(code)
  retval['Pascal'] = pascal_examiner.confidence

  py_examiner = PythonExaminer(code)
  retval['Python'] = py_examiner.confidence

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

  if language in ['pascal', 'pas']:
    examiner = PascalExaminer(code)
    tokens = examiner.tokens

  if language in ['python', 'py']:
    examiner = PythonExaminer(code)
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
    confidences = examiner.confidences

  if language in ['c']:
    examiner = CExaminer(code)
    confidences = examiner.confidences

  if language in ['c++', 'cpp']:
    examiner = CppExaminer(code)
    confidences = examiner.confidences

  if language in ['fixed-format-cobol', 'cobol', 'cob', 'cbl']:
    examiner = CobolExaminer(code, True, tab_size)
    confidences = examiner.confidences

  if language in ['free-format-cobol']:
    examiner = CobolExaminer(code, False, tab_size)
    confidences = examiner.confidences

  if language in ['fortran', 'for', 'ftn', 'fortran-66']:
    examiner = Fortran66Examiner(code, tab_size)
    confidences = examiner.confidences

  if language in ['f77', 'fortran-77']:
    examiner = Fortran77Examiner(code, tab_size)
    confidences = examiner.confidences

  if language in ['pascal', 'pas']:
    examiner = PascalExaminer(code)
    confidences = examiner.confidences

  if language in ['python', 'py']:
    examiner = PythonExaminer(code)
    confidences = examiner.confidences

  retval = json.dumps(confidences)

  return retval

if __name__ == '__main__':
  app.run()
