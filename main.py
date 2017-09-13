from flask import Flask, request, render_template
import string

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
  detected_language, unknown_tokens = identify_language(lines)
  html = render_template('detect_language.jinja.txt', language=detected_language, code=lines, unknowns=unknown_tokens)
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
      if len(token) > 0 and (st == 1 or st == 4):
        tokens.append(token)
      token = c
      st = t
  if len(token) > 0 and (st == 1 or st == 4):
    tokens.append(token)
  return tokens

def chartype(c):
  retval = 4
  if c.isalpha():
    retval = 1
  if c.isdigit():
    retval = 2
  if c.isspace():
    retval = 3
  if c in string.punctuation:
    retval = 4
  return retval

def drop_numbers(tokens):
  results = []
  for token in tokens:
    if not token[0].isdigit():
      results.append(token)
  return results

def is_variable(token):
  if len(token) == 1 and token[0].isalpha():
    return True
  if len(token) == 2 and token[0].isalpha() and token[1].isdigit():
    return True
  return False

def remove_string_literals(line):
  result = ''
  in_string = False
  for c in line:
    if c == '"' and not in_string:
      in_string = True
      c = ''

    if not in_string:
      result += c

    if c == '"' and in_string:
      in_string = False
      
  return result

def identify_language(code):
  retval = 'Unknown'
  basic_confidence, unknowns = is_basic_source(code)
  pascal_confidence, unknowns = is_pascal_source(code)
  retval = 'Pascal with confidence ' + '{0:.3f}'.format(pascal_confidence)
  return retval, unknowns

def is_basic_source(lines):
  # Pass 1 - all lines begin with numbers
  num_lines_start_num = 0
  for line in lines:
    if len(line) > 0 and line[0].isdigit():
      num_lines_start_num += 1
  confidence_1 = num_lines_start_num / len(lines)

  # Pass 2 - reasonable tokens
  num_tokens = 0
  num_known_tokens = 0
  operators = [
    '(', ')', '=', '+', '-', '*', '/', '^', '<=', '<', '>', '>=',
    ',', ';', ':', '#', '%', '$', '.'
    ]
  functions = [ 'INT', 'TAB', 'EXP', 'SIN', 'COS', 'SQR' ]
  user_functions = [
    'FNA', 'FNB', 'FNC', 'FND', 'FNE', 'FNF', 'FNG', 'FNH', 'FNI', 'FNJ',
    'FNK', 'FNL', 'FNM', 'FNN', 'FNO', 'FNP', 'FNQ', 'FNR', 'FNS', 'FNT',
    'FNU', 'FNV', 'FNW', 'FNX', 'FNY', 'FNZ'
    ]
  keywords = [
    'READ', 'DATA', 'DEF', 'DIM', 'FOR', 'TO', 'STEP', 'NEXT',
    'IF', 'THEN', 'ELSE',
    'CHANGE', 'LET',
    'PRINT', 'USING', 'INPUT', 'LINE', 'LINPUT',
    'GOTO', 'GOSUB', 'ON', 'RETURN', 'END', 'STOP',
    'OPEN', 'CLOSE'
    ]
  defined_tokens = keywords + functions + user_functions + operators
  unknown_tokens = []
  
  for line in lines:
    #  if line begins with number, remove number
    line = line.lstrip(string.digits)
    # remove leading and trailing blanks
    line = line.strip()
    # remove comments (REM and apostrophe)
    if line.startswith('REM'):
      line = ''
    #  consider only lines with text
    if len(line) > 0:
      line = remove_string_literals(line)
      #  simple lexer
      tokens = split_to_tokens(line)
      #  merge adjacent tokens when compatible
      #  drop all numbers
      tokens = drop_numbers(tokens)
      #  unknown operators reduce confidence
      #  unknown identifiers (text of two or more, not FNx) reduce confidence
      for token in tokens:
        num_tokens += 1
        if token in defined_tokens:
          num_known_tokens += 1
        elif is_variable(token):
          num_known_tokens += 1
        else:
          unknown_tokens.append(token)

  confidence_2 = 0
  if num_tokens > 0:
    confidence_2 = num_known_tokens / num_tokens

  # compute confidence
  confidence = confidence_1 * confidence_2
  
  return confidence, unknown_tokens

def remove_pascal_comments(line):
  result = ''
  in_brace_comment = False
  for c in line:
    if c == '{' and not in_brace_comment:
      in_brace_comment = True
      c = ''

    if not in_brace_comment:
      result += c

    if c == '}':
      in_brace_comment = False
      
  return result

def is_pascal_source(lines):
  confidence_1 = 0
  first_token = ''
  last_token = ''
  num_begin = 0
  num_end = 0
  for line in lines:
    line = remove_pascal_comments(line)
    # remove leading and trailing blanks
    line = line.strip()
    #  consider only lines with text
    if len(line) > 0:
      line = remove_string_literals(line)
      #  simple lexer
      tokens = split_to_tokens(line)
      #  merge adjacent tokens when compatible
      #  drop all numbers
      tokens = drop_numbers(tokens)
      #  count 'begin' and 'end' tokens
      for token in tokens:
        if first_token == '':
          first_token = token
        last_token = token
        if token == 'begin':
          num_begin += 1
        if token == 'end':
          num_end += 1

  unknown_tokens = []

  if first_token == 'program':
    unknown_tokens.append('first token is program')
    confidence_1 += 0.5
  else:
    unknown_tokens.append('first token is "' + first_token + '"')
    
  if last_token == '.':
    unknown_tokens.append('last token is .')
    confidence_1 += 0.5
  else:
    unknown_tokens.append('last token is "' + last_token + '"')

  if num_begin == num_end and num_begin > 0:
    unknown_tokens.append('begin and end counts match')
    confidence_2 = 1.0
  else:
    unknown_tokens.append('begin and end counts do not match')
    
  # compute confidence
  confidence = confidence_1 * confidence_2
  
  return confidence, unknown_tokens

if __name__ == '__main__':
  app.run()
