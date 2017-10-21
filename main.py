import json
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
  text = request.form['code']
  action = request.form['action']
  if action == 'code':
    detected_languages = identify_language(text)
    lines = text.split('\r\n')
    html = render_template('detect_language.jinja.txt', languages=detected_languages, code=lines)
  if action == 'tokens':
    found_tokens = find_tokens(text)
    html = render_template('display_tokens.jinja.txt', tokens = found_tokens)
  if action == 'json':
    detected_languages = identify_language(text)
    html = json.dumps(detected_languages)
  return html

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
