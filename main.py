import json
import codecs
from flask import Flask, request, render_template
from BasicExaminer import BasicExaminer
from PascalExaminer import PascalExaminer
from CobolExaminer import CobolExaminer

app = Flask(__name__)

@app.route('/')
def hello_world():
  html = render_template('index.jinja.txt', title='CodeStat')
  return html

@app.route('/detect', methods=['POST'])
def detect():
  bytes = request.get_data()
  text = bytes.decode('ascii')
  detected_languages = identify_language(text)
  json_text = json.dumps(detected_languages)
  return json_text

@app.route('/basic', methods=['POST'])
def basic():
  bytes = request.get_data()
  text = bytes.decode('ascii')
  basic_examiner = BasicExaminer(text)
  retval = basic_examiner.tokens
  return str(retval)

@app.route('/pascal', methods=['POST'])
def pascal():
  bytes = request.get_data()
  text = bytes.decode('ascii')
  pascal_examiner = PascalExaminer(text)
  retval = pascal_examiner.tokens
  return str(retval)

def identify_language(code):
  retval = {}

  code = code.replace('\r\n','\n')

  basic_examiner = BasicExaminer(code)
  retval['BASIC'] = basic_examiner.confidence

  pascal_examiner = PascalExaminer(code)
  retval['Pascal'] = pascal_examiner.confidence

  cobol_examiner = CobolExaminer(code)
  retval['COBOL'] = cobol_examiner.confidence

  return retval

def find_tokens(code):

  code = code.replace('\r\n','\n')

  cobol_examiner = CobolExaminer(code)
  return cobol_examiner.tokens

if __name__ == '__main__':
  app.run()
