from flask import Flask, request, render_template

app = Flask(__name__)

@app.route('/')
def hello_world():
  html = render_template('index.jinja.txt', title='CodeStat')
  return html

@app.route('/detect-language', methods=['POST'])
def detect_language():
  text = request.form['code']
  lines = text.split('\r\n')
  tokens = simple_lexer(lines)
  detected_language = identify_language(lines)
  html = render_template('detect_language.jinja.txt', language=detected_language, code=lines)
  return html

def simple_lexer(text):
  tokens = []
  for line in text:
    line_tokens = split_to_tokens(line)
    tokens = tokens + line_tokens
  return tokens

def split_to_tokens(line):
  tokens = []
  st = 0
  token = ''
  for c in line:
    t = chartype(c)
    if st == 0 or t == st:
      token = token + c
      st = t
    else:
      if len(token) > 0 and st == 1:
        tokens.append(token)
      token = c
      st = t
  if len(token) > 0 and st == 1:
    tokens.append(token)
  return tokens

def chartype(c):
  retval = 4
  if str.isalpha(c):
    retval = 1
  if str.isdigit(c):
    retval = 2
  if str.isspace(c):
    retval = 3
  return retval

def identify_language(code):
  retval = 'Unknown'
  if is_basic_source(code):
    retval = 'BASIC'
  return retval

def is_basic_source(code):
  retval = True
  for line in code:
    if len(line) > 0 and not line[0].isdigit():
      retval = False
  return retval

if __name__ == '__main__':
  app.run()
