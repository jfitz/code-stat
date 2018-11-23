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
  html = render_template('index.jinja.txt', title='Code-Stat')
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
  tokens = tokenize(text, language.lower())
  token_list = []

  for token in tokens:
    token_list.append(token.toDict())

  json_text = json.dumps(token_list)

  return json_text

@app.route('/confidence', methods=['POST'])
def confidence():
  language = request.args["language"]
  bytes = request.get_data()
  text = bytes.decode('ascii')

  json_text = tokenize_confidence(text, language.lower())

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

  return tokens

def tokenize_confidence(code, language):
  confidences = {}
  if language in ['c++', 'cpp']:
    examiner = CppExaminer(code)
    confidences = examiner.confidences
  if language in ['c']:
    examiner = CExaminer(code)
    confidences = examiner.confidences
  if language in ['pascal', 'pas']:
    examiner = PascalExaminer(code)
    confidences = examiner.confidences
  if language in ['basic', 'bas']:
    examiner = BasicExaminer(code)
    confidences = examiner.confidences
  if language in ['cobol', 'cob', 'cbl']:
    examiner = CobolExaminer(code)
    confidences = examiner.confidences

  retval = json.dumps(confidences)

  return retval

if __name__ == '__main__':
  app.run()
