import json
import codecs
from flask import Flask, request, render_template
from BasicExaminer import BasicExaminer
from PascalExaminer import PascalExaminer
from CobolExaminer import CobolExaminer
from CExaminer import CExaminer
from CppExaminer import CppExaminer

app = Flask(__name__)

@app.route('/')
def code_stat():
  html = render_template('index.jinja.txt', title='CodeStat')
  return html

@app.route('/detect', methods=['POST'])
def detect():
  bytes = request.get_data()
  text = bytes.decode('ascii')
  detected_languages = identify_language(text)
  json_text = json.dumps(detected_languages)
  return json_text

@app.route('/tokens', methods=['POST'])
def tokens():
  language = request.args["language"]
  bytes = request.get_data()
  text = bytes.decode('ascii')
  json_text = tokenize(text, language.lower())
  return json_text

def identify_language(code):
  retval = {}

  code = code.replace('\r\n','\n')

  basic_examiner = BasicExaminer(code)
  retval['BASIC'] = basic_examiner.confidence

  pascal_examiner = PascalExaminer(code)
  retval['Pascal'] = pascal_examiner.confidence

  cobol_examiner = CobolExaminer(code)
  retval['COBOL'] = cobol_examiner.confidence

  c_examiner = CExaminer(code)
  retval['C'] = c_examiner.confidence

  cpp_examiner = CppExaminer(code)
  retval['C++'] = cpp_examiner.confidence

  return retval

def tokenize(code, language):
  tokens = []
  if language in ['c++', 'cpp']:
    examiner = CppExaminer(code)
    tokens = examiner.tokens
  if language in ['c']:
    examiner = CExaminer(code)
    tokens = examiner.tokens
  if language in ['pascal', 'pas']:
    examiner = PascalExaminer(code)
    tokens = examiner.tokens
  if language in ['basic', 'bas']:
    examiner = BasicExaminer(code)
    tokens = examiner.tokens
  if language in ['cobol', 'cob', 'cbl']:
    examiner = CobolExaminer(code)
    tokens = examiner.tokens

  retval = ''
  for token in tokens:
    if token.__class__.__name__ == 'str':
      retval += token + '\n'
    else:
      retval += token.toJSON() + '\n'

  return retval

if __name__ == '__main__':
  app.run()
