from flask import Flask, request, render_template
from Examiner import Examiner
from BasicExaminer import BasicExaminer
from PascalExaminer import PascalExaminer

app = Flask(__name__)

@app.route('/')
def hello_world():
  html = render_template('index.jinja.txt', title='CodeStat')
  return html

@app.route('/detect', methods=['POST'])
def detect_language():
  return 'Page text'
  #text = request.form['code']
  #lines = text.split('\r\n')
  #detected_languages = identify_language(lines)
  #html = render_template('detect_language.jinja.txt', languages=detected_languages, code=lines)
  #return html

def identify_language(code):
  retval = {}

  basic_examiner = BasicExaminer(code)
  retval['BASIC'] = basic_examiner.confidence

  pascal_examiner = PascalExaminer(code)
  retval['Pascal'] = pascal_examiner.confidence

  return retval

if __name__ == '__main__':
  app.run()
