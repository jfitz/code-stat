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
  detected_languages = identify_language(text)
  lines = text.split('\r\n')
  html = render_template('detect_language.jinja.txt', languages=detected_languages, code=lines)
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

if __name__ == '__main__':
  app.run()
