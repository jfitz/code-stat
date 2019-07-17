import argparse
import cProfile
import json
import codecs
from flask import Flask, request, render_template, Response
from CodeStatException import CodeStatException
from GenericCodeExaminer import GenericCodeExaminer
from AdaExaminer import AdaExaminer
from BasicExaminer import BasicExaminer
from CBasicExaminer import CBasicExaminer
from CExaminer import CExaminer
from CobolFixedFormatExaminer import CobolFixedFormatExaminer
from CobolFreeFormatExaminer import CobolFreeFormatExaminer
from CppExaminer import CppExaminer
from CsharpExaminer import CsharpExaminer
from FortranFixedFormatExaminer import FortranFixedFormatExaminer
from FortranFreeFormatExaminer import FortranFreeFormatExaminer
from FsharpExaminer import FsharpExaminer
from GoExaminer import GoExaminer
from HTMLExaminer import HTMLExaminer
from JavaExaminer import JavaExaminer
from JavaScriptExaminer import JavaScriptExaminer
from ObjectiveCExaminer import ObjectiveCExaminer
from PascalExaminer import PascalExaminer
from PL1FixedFormatExaminer import PL1FixedFormatExaminer
from PL1FreeFormatExaminer import PL1FreeFormatExaminer
from PrologExaminer import PrologExaminer
from PythonExaminer import PythonExaminer
from RExaminer import RExaminer
from RubyExaminer import RubyExaminer
from RustExaminer import RustExaminer
from SqlExaminer import SqlExaminer
from SwiftExaminer import SwiftExaminer
from TypeScriptExaminer import TypeScriptExaminer
from VisualBasic6Examiner import VisualBasic6Examiner
from VisualBasicNETExaminer import VisualBasicNETExaminer


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


def tokens_to_dict(tokens):
  token_list = []
  for token in tokens:
    token_list.append(token.toDict())
  return token_list


def find_winners(text, tabsize, wide, languages):
  tiebreak_keywords = False
  tiebreak_tokens = False
  detected_languages = identify_language(text, tabsize, wide, tiebreak_keywords, tiebreak_tokens, languages)

  # find the highest value
  high_value = 0

  for key in detected_languages:
    if detected_languages[key] > high_value:
      high_value = detected_languages[key]

  # select all with the highest value
  winning_languages = []
  for key in detected_languages:
    if detected_languages[key] == high_value:
      winning_languages.append(key)

  return winning_languages


app = Flask(__name__)

codesAndNames = {
  'ada-83': 'Ada-83',
  'ada-95': 'Ada-95',
  'ada-2005': 'Ada-2005',
  'ada-2012': 'Ada-2012',
  'basic': 'BASIC',
  'c': 'C',
  'cplusplus': 'C++',
  'csharp': 'C#',
  'cbasic': 'CBASIC',
  'cobol-68': 'COBOL-68',
  'cobol-74': 'COBOL-74',
  'cobol-85': 'COBOL-85',
  'cobol-2002': 'COBOL-2002',
  'cobol-2014': 'COBOL-2014',
  'cobol-2014-acu': 'COBOL-2014-ACU',
  'cobol-2014-ibm': 'COBOL-2014-IBM',
  'cobol-2014-gnu': 'COBOL-2014-GNU',
  'fortran-66': 'FORTRAN-66',
  'fortran-77': 'FORTRAN-77',
  'fortran-90': 'Fortran-90',
  'fortran-95': 'Fortran-95',
  'fortran-2003': 'Fortran-2003',
  'fortran-2008': 'Fortran-2008',
  'fsharp': 'F#',
  'go': 'Go',
  'java': 'Java',
  'javascript': 'JavaScript',
  'html': 'HTML',
  'objective-c': 'Objective-C',
  'pascal': 'Pascal',
  'pl1-fixed': 'PL/1-Fixed',
  'pl1-free': 'PL/1-Free',
  'prolog': 'Prolog',
  'python': 'Python',
  'r': 'R',
  'ruby': 'Ruby',
  'rust': 'Rust',
  'sql-92': 'SQL-92',
  'sql-99': 'SQL-99',
  'sql-2003': 'SQL-2003',
  'sql-2008': 'SQL-2008',
  'sql-2011': 'SQL-2011',
  'sql-2016': 'SQL-2016',
  'swift': 'Swift',
  'typescript': 'TypeScript',
  'visualbasic-6': 'VisualBasic-6',
  'visualbasic-net': 'VisualBasic-NET'
}


@app.route('/languages', methods=['GET'])
def route_languages():
  json_text = json.dumps(codesAndNames)
  return json_text


@app.route('/detab', methods=['POST'])
def route_detab():
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


@app.route('/truncate', methods=['POST'])
def route_truncate():
  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)
  truncated_text = truncate_lines(text, 72)

  return truncated_text


@app.route('/unwrap', methods=['POST'])
def route_unwrap():
  language = ''
  if 'language' in request.args:
    language = request.args['language']

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)

  unwrapped_text = unwrap_lines(text, language.lower())

  return unwrapped_text


@app.route('/detect', methods=['POST'])
def route_detect():
  tabsize = 8
  if 'tabsize' in request.args:
    tabsize = request.args['tabsize']

  languages = []
  if 'languages' in request.args:
    languages = request.args['languages'].split(' ')

  if len(languages) == 0:
    languages = list(codesAndNames.keys())

  wide = 'wide' in request.args

  tiebreak_keywords = False
  if 'tiebreak-keywords' in request.args:
    tiebreak_keywords = True

  tiebreak_tokens = False
  if 'tiebreak-tokens' in request.args:
    tiebreak_tokens = True

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)

  http_status = 200
  try:
    detected_languages = identify_language(text, tabsize, wide, tiebreak_keywords, tiebreak_tokens, languages)

    dl2 = {}
    for key in detected_languages:
      new_key = codesAndNames[key]
      dl2[new_key] = detected_languages[key]

    json_text = json.dumps(dl2)
  except CodeStatException as e:
    http_status = 450
    json_text = str(e)

  return Response(json_text, status=http_status, mimetype='application/json')


@app.route('/tokens', methods=['POST'])
def route_tokens():
  language = ''
  if 'language' in request.args:
    language = request.args['language']

  languages = []
  if 'languages' in request.args:
    languages = request.args['languages']

    if len(languages) == 0:
      languages = list(codesAndNames.keys())
    else:
      languages = request.args['languages'].lower().split(' ')

  comment = ''
  if 'comment' in request.args:
    comment = request.args['comment']

  tabsize = 8
  if 'tabsize' in request.args:
    tabsize = request.args['tabsize']

  wide = 'wide' in request.args

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)

  http_status = 200
  try:
    if len(language) > 0:
      tokens = tokenize(text, language.lower(), tabsize, wide, comment)
      token_list = tokens_to_dict(tokens)
      json_text = json.dumps(token_list)
    else:
      if len(languages) > 0:
        # detect for languages
        winning_languages = find_winners(text, tabsize, wide, languages)

        list_of_dicts = []
        # for each winning language
        for language in winning_languages:
          # tokenize
          tokens = tokenize(text, language, tabsize, wide, comment)
          token_list = tokens_to_dict(tokens)

          # dictionary language: language, tokens: tokens
          mydict = {}
          mydict['language'] = language
          mydict['tokens'] = token_list

          # add dictionary to list
          list_of_dicts.append(mydict)

        json_text = json.dumps(list_of_dicts)
      else:
        # tokenize as generic
        tokens = tokenize(text, 'generic', tabsize, wide, comment)
        token_list = tokens_to_dict(tokens)
        json_text = json.dumps(token_list)
  except CodeStatException as e:
    http_status = 450
    json_text = str(e)

  return Response(json_text, status=http_status, mimetype='application/json')


@app.route('/confidence', methods=['POST'])
def route_confidence():
  language = ''
  if 'language' in request.args:
    language = request.args['language']

  languages = []
  if 'languages' in request.args:
    languages = request.args['languages']

    if len(languages) == 0:
      languages = list(codesAndNames.keys())
    else:
      languages = request.args['languages'].lower().split(' ')

  comment = ''
  if 'comment' in request.args:
    comment = request.args['comment']

  tabsize = 8
  if 'tabsize' in request.args:
    tabsize = request.args['tabsize']

  wide = 'wide' in request.args

  get_errors = 'errors' in request.args

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)

  http_status = 200
  try:
    if len(language) > 0:
      token_list = tokenize_confidence(text, language.lower(), tabsize, get_errors, wide, comment)
      json_text = json.dumps(token_list)
    else:
      if len(languages) > 0:
        # detect for languages
        winning_languages = find_winners(text, tabsize, wide, languages)

        list_of_dicts = []
        # for each winning language
        for language in winning_languages:
          # tokenize
          token_list = tokenize_confidence(text, language, tabsize, get_errors, wide, comment)

          # dictionary language: language, tokens: tokens
          mydict = {}
          mydict['language'] = language
          if get_errors:
            mydict['errors'] = token_list
          else:
            mydict['confidences'] = token_list

          # add dictionary to list
          list_of_dicts.append(mydict)

        json_text = json.dumps(list_of_dicts)
      else:
        # tokenize as generic
        tokens = tokenize_confidence(text, 'generic', tabsize, get_errors, wide, comment)
        token_list = tokens_to_dict(tokens)
        json_text = json.dumps(token_list)
  except CodeStatException as e:
    http_status = 450
    json_text = str(e)

  return Response(json_text, status=http_status, mimetype='application/json')


@app.route('/statistics', methods=['POST'])
def route_statistics():
  language = ''
  if 'language' in request.args:
    language = request.args['language']

  languages = []
  if 'languages' in request.args:
    languages = request.args['languages']

    if len(languages) == 0:
      languages = list(codesAndNames.keys())
    else:
      languages = request.args['languages'].lower().split(' ')

  comment = ''
  if 'comment' in request.args:
    comment = request.args['comment']

  tabsize = 8
  if 'tabsize' in request.args:
    tabsize = request.args['tabsize']

  wide = 'wide' in request.args

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)

  http_status = 200
  try:
    if len(language) > 0:
      token_list = tokenize_statistics(text, language.lower(), tabsize, wide, comment)
      json_text = json.dumps(token_list)
    else:
      if len(languages) > 0:
        # detect for languages
        winning_languages = find_winners(text, tabsize, wide, languages)

        list_of_dicts = []
        # for each winning language
        for language in winning_languages:
          # tokenize
          token_list = tokenize_statistics(text, language, tabsize, wide, comment)

          # dictionary language: language, tokens: tokens
          mydict = {}
          mydict['language'] = language
          mydict['statistics'] = token_list

          # add dictionary to list
          list_of_dicts.append(mydict)

        json_text = json.dumps(list_of_dicts)
      else:
        # tokenize as generic
        tokens = tokenize_statistics(text, 'generic', tabsize, wide, comment)
        token_list = tokens_to_dict(tokens)
        json_text = json.dumps(token_list)
  except CodeStatException as e:
    http_status = 450
    json_text = str(e)

  return Response(json_text, status=http_status, mimetype='application/json')


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


def split_lines(text):
  lines = []

  line = ''
  for c in text:
    if c == '\r':
      pass
    else:
      if c == '\n':
        lines.append(line)
        line = ''
      else:
        line += c
  if len(line) > 0:
    lines.append(line)

  return lines


def truncate_lines(text, width):
  truncated_lines = ''

  # break into lines
  lines = split_lines(text)

  for line in lines:
    line = line[:width]
    truncated_lines += line
    truncated_lines += '\n'

  return truncated_lines


def identify_language(code, tabsize, wide, tiebreak_keywords, tiebreak_tokens, languages):
  try:
    tab_size = int(tabsize)
  except ValueError:
    tab_size = 8

  code = code.replace('\r\n','\n')

  examiners = {}

  if 'ada-83' in languages:
    examiners['ada-83'] = AdaExaminer(code, '83')

  if 'ada-95' in languages:
    examiners['ada-95'] = AdaExaminer(code, '95')

  if 'ada-2005' in languages:
    examiners['ada-2005'] = AdaExaminer(code, '2005')

  if 'ada-2012' in languages or 'ada' in languages:
    examiners['ada-2012'] = AdaExaminer(code, '2012')

  if 'basic' in languages:
    examiners['basic'] = BasicExaminer(code)

  if 'cbasic' in languages:
    examiners['cbasic'] = CBasicExaminer(code)

  if 'c' in languages:
    examiners['c'] = CExaminer(code)

  if 'cplusplus' in languages:
    examiners['cplusplus'] = CppExaminer(code)

  if 'csharp' in languages:
    examiners['csharp'] = CsharpExaminer(code)

  if 'cobol-68' in languages:
    examiners['cobol-68'] = CobolFixedFormatExaminer(code, '68', '', tab_size, wide)

  if 'cobol-74' in languages:
    examiners['cobol-74'] = CobolFixedFormatExaminer(code, '74', '', tab_size, wide)

  if 'cobol-85' in languages:
    examiners['cobol-85'] = CobolFixedFormatExaminer(code, '85', '', tab_size, wide)

  if 'cobol-2002' in languages:
    examiners['cobol-2002'] = CobolFreeFormatExaminer(code, '2002', '')

  if 'cobol-2014' in languages or 'cobol' in languages:
    examiners['cobol-2014'] = CobolFreeFormatExaminer(code, '2014', '')

  if 'cobol-2014-acu' in languages:
    examiners['cobol-2014-acu'] = CobolFreeFormatExaminer(code, '2014', 'acu')

  if 'cobol-2014-ibm' in languages:
    examiners['cobol-2014-ibm'] = CobolFreeFormatExaminer(code, '2014', 'ibm')

  if 'cobol-2014-gnu' in languages:
    examiners['cobol-2014-gnu'] = CobolFreeFormatExaminer(code, '2014', 'gnu')

  if 'fortran-66' in languages:
    examiners['fortran-66'] = FortranFixedFormatExaminer(code, '66', tab_size, wide)

  if 'fortran-77' in languages:
    examiners['fortran-77'] = FortranFixedFormatExaminer(code, '77', tab_size, wide)

  if 'fortran-90' in languages:
    examiners['fortran-90'] = FortranFreeFormatExaminer(code, '90')

  if 'fortran-95' in languages:
    examiners['fortran-95'] = FortranFreeFormatExaminer(code, '95')

  if 'fortran-2003' in languages:
    examiners['fortran-2003'] = FortranFreeFormatExaminer(code, '2003')

  if 'fortran-2008' in languages or 'fortran' in languages:
    examiners['fortran-2008'] = FortranFreeFormatExaminer(code, '2008')

  if 'fsharp' in languages:
    examiners['fsharp'] = FsharpExaminer(code)

  if 'go' in languages:
    examiners['go'] = GoExaminer(code)

  if 'html' in languages:
    examiners['html'] = HTMLExaminer(code)

  if 'objective-c' in languages:
    examiners['objective-c'] = ObjectiveCExaminer(code)

  if 'java' in languages:
    examiners['java'] = JavaExaminer(code)

  if 'javascript' in languages:
    examiners['javascript'] = JavaScriptExaminer(code)

  if 'pascal' in languages:
    examiners['pascal'] = PascalExaminer(code)

  if 'pl1-fixed' in languages:
    examiners['pl1-fixed'] = PL1FixedFormatExaminer(code, tab_size, wide)

  if 'pl1-free' in languages:
    examiners['pl1-free'] = PL1FreeFormatExaminer(code)

  if 'prolog' in languages:
    examiners['prolog'] = PrologExaminer(code)

  if 'python' in languages:
    examiners['python'] = PythonExaminer(code)

  if 'r' in languages:
    examiners['r'] = RExaminer(code)

  if 'ruby' in languages:
    examiners['ruby'] = RubyExaminer(code)

  if 'rust' in languages:
    examiners['rust'] = RustExaminer(code)

  if 'sql-92' in languages:
    examiners['sql-92'] = SqlExaminer(code, '92', '')

  if 'sql-99' in languages:
    examiners['sql-99'] = SqlExaminer(code, '99', '')

  if 'sql-2003' in languages:
    examiners['sql-2003'] = SqlExaminer(code, '2003', '')

  if 'sql-2008' in languages:
    examiners['sql-2008'] = SqlExaminer(code, '2008', '')

  if 'sql-2011' in languages:
    examiners['sql-2011'] = SqlExaminer(code, '2011', '')

  if 'sql-2016' in languages or 'sql' in languages:
    examiners['sql-2016'] = SqlExaminer(code, '2016', '')

  if 'swift' in languages:
    examiners['swift'] = SwiftExaminer(code)

  if 'typescript' in languages:
    examiners['typescript'] = TypeScriptExaminer(code)

  if 'visualbasic-6' in languages:
    examiners['visualbasic-6'] = VisualBasic6Examiner(code)

  if 'visualbasic-net' in languages:
    examiners['visualbasic-net'] = VisualBasicNETExaminer(code)

  # get confidence values
  retval = {}
  highest_confidence = 0
  for name in examiners:
    confidence = examiners[name].confidence()
    retval[name] = confidence
    # find the highest value
    if confidence > highest_confidence:
      highest_confidence = confidence

  if tiebreak_keywords:
    # count how many have the greatest confidence
    high_names = []
    for name in examiners:
      confidence = examiners[name].confidence()
      if confidence == highest_confidence:
        high_names.append(name)

    # if a tie among multiple examiners
    if len(high_names) > 1:
      highest_keyword_count = 0
      for name in high_names:
        keyword_count = len(examiners[name].find_keywords())
        if keyword_count > highest_keyword_count:
          highest_keyword_count = keyword_count

      if highest_keyword_count > 0:
        # assign confidence to number of keywords (more is better)
        for name in high_names:
          count = len(examiners[name].find_keywords())
          keyword_count_confidence = count / highest_keyword_count
          examiners[name].confidences['keyword_count'] = keyword_count_confidence

        # recalculate confidence with new factor
        for name in high_names:
          confidence = examiners[name].confidence()
          retval[name] = confidence

  if tiebreak_tokens:
    # count how many have the greatest confidence
    high_names = []
    for name in examiners:
      confidence = examiners[name].confidence()
      if confidence == highest_confidence:
        high_names.append(name)

    # if a tie among multiple examiners
    if len(high_names) > 1:
      lowest_token_count = None
      for name in high_names:
        count = len(examiners[name].tokens)
        if lowest_token_count is None or count < lowest_token_count:
          lowest_token_count = count

      # assign confidence to number of tokens (fewer tokens are better)
      for name in high_names:
        count = len(examiners[name].tokens)
        token_count_confidence = lowest_token_count / count
        examiners[name].confidences['token_count'] = token_count_confidence

      # recalculate confidence with new factor
      for name in high_names:
        confidence = examiners[name].confidence()
        retval[name] = confidence

  return retval


def unwrap_lines(text, language):
  # only the fixed-format versions can be unwrapped
  cobol_names = ['cobol', 'cobol-68', 'cobol-74', 'cobol-85']
  fortran_names = ['fortran', 'fortran-66', 'fortran-77']
  pl1_names = ['pl1-fixed']

  unwrapped_text = text

  if language in fortran_names:
    examiner = FortranFixedFormatExaminer(text, '77', 8, False)
    lines = split_lines(text)
    unwrapped_text = examiner.unwrapped_code(lines)

  if language in cobol_names:
    examiner = CobolFixedFormatExaminer(text, '85', '', 8, False)
    lines = split_lines(text)
    unwrapped_text = examiner.unwrapped_code(lines)

  if language in pl1_names:
    examiner = PL1FixedFormatExaminer(text, 8, False)
    lines = split_lines(text)
    unwrapped_text = examiner.unwrapped_code(lines)

  if language in ['cbasic']:
    examiner = CBasicExaminer(text)
    unwrapped_text = examiner.unwrapped_code()

  if language in ['c']:
    examiner = CExaminer(text)
    unwrapped_text = examiner.unwrapped_code()

  if language in ['c++', 'cplusplus']:
    examiner = CppExaminer(text)
    unwrapped_text = examiner.unwrapped_code()

  if language in ['go', 'golang']:
    examiner = GoExaminer(text)
    unwrapped_text = examiner.unwrapped_code()

  if language in ['objective-c', 'objc']:
    examiner = ObjectiveCExaminer(text)
    unwrapped_text = examiner.unwrapped_code()

  if language in ['python', 'py']:
    examiner = PythonExaminer(text)
    unwrapped_text = examiner.unwrapped_code()

  return unwrapped_text


def tokenize(code, language, tabsize, wide, comment):
  try:
    tab_size = int(tabsize)
  except ValueError:
    tab_size = 8

  examiner = None
  tokens = []

  if language in ['generic']:
    examiner = GenericCodeExaminer(code, comment)
    tokens = examiner.tokens

  if language in ['ada-83']:
    examiner = AdaExaminer(code, '83')
    tokens = examiner.tokens

  if language in ['ada-95']:
    examiner = AdaExaminer(code, '95')
    tokens = examiner.tokens

  if language in ['ada-2005']:
    examiner = AdaExaminer(code, '2005')
    tokens = examiner.tokens

  if language in ['ada-2012', 'ada']:
    examiner = AdaExaminer(code, '2012')
    tokens = examiner.tokens

  if language in ['basic', 'bas']:
    examiner = BasicExaminer(code)
    tokens = examiner.tokens

  if language in ['c']:
    examiner = CExaminer(code)
    tokens = examiner.tokens

  if language in ['c++', 'cplusplus']:
    examiner = CppExaminer(code)
    tokens = examiner.tokens

  if language in ['csharp', 'c#', 'cs']:
    examiner = CsharpExaminer(code)
    tokens = examiner.tokens

  if language in ['cbasic']:
    examiner = CBasicExaminer(code)
    tokens = examiner.tokens

  if language in ['cobol-68']:
    examiner = CobolFixedFormatExaminer(code, '68', '', tab_size, wide)
    tokens = examiner.tokens

  if language in ['cobol-74']:
    examiner = CobolFixedFormatExaminer(code, '74', '', tab_size, wide)
    tokens = examiner.tokens

  if language in ['cobol-85']:
    examiner = CobolFixedFormatExaminer(code, '85', '', tab_size, wide)
    tokens = examiner.tokens

  if language in ['cobol-2002']:
    examiner = CobolFreeFormatExaminer(code, '2002', '')
    tokens = examiner.tokens

  if language in ['cobol-2014', 'cobol', 'cob', 'cbl']:
    examiner = CobolFreeFormatExaminer(code, '2014', '')
    tokens = examiner.tokens

  if language in ['cobol-2014-acu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'acu')
    tokens = examiner.tokens

  if language in ['cobol-2014-ibm']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'ibm')
    tokens = examiner.tokens

  if language in ['cobol-2014-gnu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'gnu')
    tokens = examiner.tokens

  if language in ['fortran-66']:
    examiner = FortranFixedFormatExaminer(code, '66', tab_size, wide)
    tokens = examiner.tokens

  if language in ['fortran-77', 'f77']:
    examiner = FortranFixedFormatExaminer(code, '77', tab_size, wide)
    tokens = examiner.tokens

  if language in ['fortran-90', 'f90']:
    examiner = FortranFreeFormatExaminer(code, '90')
    tokens = examiner.tokens

  if language in ['fortran-95', 'f95']:
    examiner = FortranFreeFormatExaminer(code, '95')
    tokens = examiner.tokens

  if language in ['fortran-2003', 'f03']:
    examiner = FortranFreeFormatExaminer(code, '2003')
    tokens = examiner.tokens

  if language in ['fortran-2008', 'f08', 'fortran', 'for', 'ftn']:
    examiner = FortranFreeFormatExaminer(code, '2008')
    tokens = examiner.tokens

  if language in ['fsharp', 'fs']:
    examiner = FsharpExaminer(code)
    tokens = examiner.tokens

  if language in ['go', 'golang']:
    examiner = GoExaminer(code)
    tokens = examiner.tokens

  if language in ['html', 'php']:
    examiner = HTMLExaminer(code)
    tokens = examiner.tokens

  if language in ['java', 'jav']:
    examiner = JavaExaminer(code)
    tokens = examiner.tokens

  if language in ['javascript', 'js']:
    examiner = JavaScriptExaminer(code)
    tokens = examiner.tokens

  if language in ['objective-c', 'objc']:
    examiner = ObjectiveCExaminer(code)
    tokens = examiner.tokens

  if language in ['pascal', 'pas']:
    examiner = PascalExaminer(code)
    tokens = examiner.tokens

  if language in ['pl1-fixed']:
    examiner = PL1FixedFormatExaminer(code, tab_size, wide)
    tokens = examiner.tokens

  if language in ['pl1-free', 'pl1']:
    examiner = PL1FreeFormatExaminer(code)
    tokens = examiner.tokens

  if language in ['prolog']:
    examiner = PrologExaminer(code)
    tokens = examiner.tokens

  if language in ['python', 'py']:
    examiner = PythonExaminer(code)
    tokens = examiner.tokens

  if language in ['r', 'rmd']:
    examiner = RExaminer(code)
    tokens = examiner.tokens

  if language in ['ruby', 'rb']:
    examiner = RubyExaminer(code)
    tokens = examiner.tokens

  if language in ['rust', 'rs']:
    examiner = RustExaminer(code)
    tokens = examiner.tokens

  if language in ['sql-92']:
    examiner = SqlExaminer(code, '92', '')
    tokens = examiner.tokens

  if language in ['sql-99']:
    examiner = SqlExaminer(code, '99', '')
    tokens = examiner.tokens

  if language in ['sql-2003']:
    examiner = SqlExaminer(code, '2003', '')
    tokens = examiner.tokens

  if language in ['sql-2008']:
    examiner = SqlExaminer(code, '2008', '')
    tokens = examiner.tokens

  if language in ['sql-2011']:
    examiner = SqlExaminer(code, '2011', '')
    tokens = examiner.tokens

  if language in ['sql-2016', 'sql']:
    examiner = SqlExaminer(code, '2016', '')
    tokens = examiner.tokens

  if language in ['swift']:
    examiner = SwiftExaminer(code)
    tokens = examiner.tokens

  if language in ['typescript']:
    examiner = TypeScriptExaminer(code)
    tokens = examiner.tokens

  if language in ['visualbasic-6', 'vb', 'vba']:
    examiner = VisualBasic6Examiner(code)
    tokens = examiner.tokens

  if language in ['visualbasic-net', 'vb.net']:
    examiner = VisualBasicNETExaminer(code)
    tokens = examiner.tokens

  if examiner is None:
    examiner = GenericCodeExaminer(code, '')
    tokens = examiner.tokens

  return tokens


def tokenize_confidence(code, language, tabsize, get_errors, wide, comment):
  try:
    tab_size = int(tabsize)
  except ValueError:
    tab_size = 8

  examiner = None
  confidences = {}
  errors = []

  if language in ['generic']:
    examiner = GenericCodeExaminer(code, comment)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['ada-83']:
    examiner = AdaExaminer(code, '83')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['ada-95']:
    examiner = AdaExaminer(code, '95')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['ada-2005']:
    examiner = AdaExaminer(code, '2005')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['ada-2012', 'ada']:
    examiner = AdaExaminer(code, '2012')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['basic', 'bas']:
    examiner = BasicExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['c']:
    examiner = CExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['c++', 'cplusplus']:
    examiner = CppExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['c#', 'csharp']:
    examiner = CsharpExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cbasic']:
    examiner = CBasicExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-68']:
    examiner = CobolFixedFormatExaminer(code, '68', '', tab_size, wide)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-74']:
    examiner = CobolFixedFormatExaminer(code, '74', '', tab_size, wide)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-85']:
    examiner = CobolFixedFormatExaminer(code, '85', '', tab_size, wide)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-2002']:
    examiner = CobolFreeFormatExaminer(code, '2002', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-2014', 'cobol', 'cob', 'cbl']:
    examiner = CobolFreeFormatExaminer(code, '2014', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-2014-acu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'acu')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-2014-ibm']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'ibm')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['cobol-2014-gnu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'gnu')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['fortran-66']:
    examiner = FortranFixedFormatExaminer(code, '66', tab_size, wide)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['fortran-77', 'f77']:
    examiner = FortranFixedFormatExaminer(code, '77', tab_size, wide)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['fortran-90', 'f90']:
    examiner = FortranFreeFormatExaminer(code, '90')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['fortran-95', 'f95']:
    examiner = FortranFreeFormatExaminer(code, '95')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['fortran-2003', 'f03']:
    examiner = FortranFreeFormatExaminer(code, '2003')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['fortran-2008', 'f08', 'fortran', 'for', 'ftn']:
    examiner = FortranFreeFormatExaminer(code, '2008')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['fsharp', 'fs']:
    examiner = FsharpExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['go', 'golang']:
    examiner = GoExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['html', 'php']:
    examiner = HTMLExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['java', 'jav']:
    examiner = JavaExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['javascript', 'js']:
    examiner = JavaScriptExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['objective-c', 'objc']:
    examiner = ObjectiveCExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['pascal', 'pas']:
    examiner = PascalExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['pl1-fixed']:
    examiner = PL1FixedFormatExaminer(code, tab_size, wide)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['pl1-free', 'pl1']:
    examiner = PL1FreeFormatExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['prolog']:
    examiner = PrologExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['python', 'py']:
    examiner = PythonExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['r', 'rmd']:
    examiner = RExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['ruby', 'rb']:
    examiner = RubyExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['rust', 'rs']:
    examiner = RustExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['sql-92']:
    examiner = SqlExaminer(code, '92', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['sql-99']:
    examiner = SqlExaminer(code, '99', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['sql-2003']:
    examiner = SqlExaminer(code, '2003', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['sql-2008']:
    examiner = SqlExaminer(code, '2008', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['sql-2011']:
    examiner = SqlExaminer(code, '2011', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['sql-2016', 'sql']:
    examiner = SqlExaminer(code, '2016', '')
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['swift']:
    examiner = SwiftExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['typescript']:
    examiner = TypeScriptExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['visualbasic-6']:
    examiner = VisualBasic6Examiner(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if language in ['visualbasic-net']:
    examiner = VisualBasicNETExaminer(code)
    confidences = examiner.confidences
    errors = examiner.errors

  if examiner is None:
    examiner = GenericCodeExaminer(code, '')
    confidences = examiner.confidences
    errors = examiner.errors

  if get_errors:
    retval = errors
  else:
    retval = confidences

  return retval


def tokenize_statistics(code, language, tabsize, wide, comment):
  try:
    tab_size = int(tabsize)
  except ValueError:
    tab_size = 8

  examiner = None
  statistics = {}

  if language in ['generic']:
    examiner = GenericCodeExaminer(code, comment)
    statistics = examiner.statistics

  if language in ['ada-83']:
    examiner = AdaExaminer(code, '83')
    statistics = examiner.statistics

  if language in ['ada-95']:
    examiner = AdaExaminer(code, '95')
    statistics = examiner.statistics

  if language in ['ada-2005']:
    examiner = AdaExaminer(code, '2005')
    statistics = examiner.statistics

  if language in ['ada-2012', 'ada']:
    examiner = AdaExaminer(code, '2012')
    statistics = examiner.statistics

  if language in ['basic', 'bas']:
    examiner = BasicExaminer(code)
    statistics = examiner.statistics

  if language in ['c']:
    examiner = CExaminer(code)
    statistics = examiner.statistics

  if language in ['c++', 'cplusplus']:
    examiner = CppExaminer(code)
    statistics = examiner.statistics

  if language in ['c#', 'csharp']:
    examiner = CsharpExaminer(code)
    statistics = examiner.statistics

  if language in ['cbasic']:
    examiner = CBasicExaminer(code)
    statistics = examiner.statistics

  if language in ['cobol-68']:
    examiner = CobolFixedFormatExaminer(code, '68', '', tab_size, wide)
    statistics = examiner.statistics

  if language in ['cobol-74']:
    examiner = CobolFixedFormatExaminer(code, '74', '', tab_size, wide)
    statistics = examiner.statistics

  if language in ['cobol-85']:
    examiner = CobolFixedFormatExaminer(code, '85', '', tab_size, wide)
    statistics = examiner.statistics

  if language in ['cobol-2002']:
    examiner = CobolFreeFormatExaminer(code, '2002', '')
    statistics = examiner.statistics

  if language in ['cobol-2014', 'cobol', 'cob', 'cbl']:
    examiner = CobolFreeFormatExaminer(code, '2014', '')
    statistics = examiner.statistics

  if language in ['cobol-2014-acu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'acu')
    statistics = examiner.statistics

  if language in ['cobol-2014-ibm']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'ibm')
    statistics = examiner.statistics

  if language in ['cobol-2014-gnu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'gnu')
    statistics = examiner.statistics

  if language in ['fortran-66']:
    examiner = FortranFixedFormatExaminer(code, '66', tab_size, wide)
    statistics = examiner.statistics

  if language in ['fortran-77', 'f77']:
    examiner = FortranFixedFormatExaminer(code, '77', tab_size, wide)
    statistics = examiner.statistics

  if language in ['fortran-90', 'f90']:
    examiner = FortranFreeFormatExaminer(code, '90')
    statistics = examiner.statistics

  if language in ['fortran-95', 'f95']:
    examiner = FortranFreeFormatExaminer(code, '95')
    statistics = examiner.statistics

  if language in ['fortran-2003', 'f03']:
    examiner = FortranFreeFormatExaminer(code, '2003')
    statistics = examiner.statistics

  if language in ['fortran-2008', 'f08', 'fortran', 'for', 'ftn']:
    examiner = FortranFreeFormatExaminer(code, '2008')
    statistics = examiner.statistics

  if language in ['fsharp', 'fs']:
    examiner = FsharpExaminer(code)
    statistics = examiner.statistics

  if language in ['go', 'golang']:
    examiner = GoExaminer(code)
    statistics = examiner.statistics

  if language in ['html', 'php']:
    examiner = HTMLExaminer(code)
    statistics = examiner.statistics

  if language in ['java', 'jav']:
    examiner = JavaExaminer(code)
    statistics = examiner.statistics

  if language in ['javascript', 'js']:
    examiner = JavaScriptExaminer(code)
    statistics = examiner.statistics

  if language in ['objective-c', 'objc']:
    examiner = ObjectiveCExaminer(code)
    statistics = examiner.statistics

  if language in ['pascal', 'pas']:
    examiner = PascalExaminer(code)
    statistics = examiner.statistics

  if language in ['pl1-fixed']:
    examiner = PL1FixedFormatExaminer(code, tab_size, wide)
    statistics = examiner.statistics

  if language in ['pl1-free', 'pl1']:
    examiner = PL1FreeFormatExaminer(code)
    statistics = examiner.statistics

  if language in ['prolog']:
    examiner = PrologExaminer(code)
    statistics = examiner.statistics

  if language in ['python', 'py']:
    examiner = PythonExaminer(code)
    statistics = examiner.statistics

  if language in ['r', 'rmd']:
    examiner = RExaminer(code)
    statistics = examiner.statistics

  if language in ['ruby', 'rb']:
    examiner = RubyExaminer(code)
    statistics = examiner.statistics

  if language in ['rust', 'rs']:
    examiner = RustExaminer(code)
    statistics = examiner.statistics

  if language in ['sql-92']:
    examiner = SqlExaminer(code, '92', '')
    statistics = examiner.statistics

  if language in ['sql-99']:
    examiner = SqlExaminer(code, '99', '')
    statistics = examiner.statistics

  if language in ['sql-2003']:
    examiner = SqlExaminer(code, '2003', '')
    statistics = examiner.statistics

  if language in ['sql-2008']:
    examiner = SqlExaminer(code, '2008', '')
    statistics = examiner.statistics

  if language in ['sql-2011']:
    examiner = SqlExaminer(code, '2011', '')
    statistics = examiner.statistics

  if language in ['sql-2016', 'sql']:
    examiner = SqlExaminer(code, '2016', '')
    statistics = examiner.statistics

  if language in ['swift']:
    examiner = SwiftExaminer(code)
    statistics = examiner.statistics

  if language in ['typescript']:
    examiner = TypeScriptExaminer(code)
    statistics = examiner.statistics

  if language in ['visualbasic-6']:
    examiner = VisualBasic6Examiner(code)
    statistics = examiner.statistics

  if language in ['visualbasic-net']:
    examiner = VisualBasicNETExaminer(code)
    statistics = examiner.statistics

  if examiner is None:
    examiner = GenericCodeExaminer(code, '')
    statistics = examiner.statistics

  return statistics


if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  parser.add_argument('--tokenize', action='store', dest='tokenize')
  parser.add_argument('--confidence', action='store', dest='confidence')
  parser.add_argument('--detect', action='store', dest='detect')
  args = parser.parse_args()

  if args.tokenize is not None:
    print('Tokenizing file ' + args.tokenize)
    in_file = open(args.tokenize, 'r')
    code = in_file.read()
    in_file.close()
    language = 'ruby'
    tabsize = 4
    wide = False
    cProfile.run("tokenize(code, language, tabsize, wide, '')")
  else:
    if args.confidence is not None:
      print('Confidencing file ' + args.confidence)
      in_file = open(args.confidence, 'r')
      code = in_file.read()
      in_file.close()
      language = 'ruby'
      tabsize = 4
      wide = False
      cProfile.run("tokenize_confidence(code, language, False, tabsize, wide, '')")
    else:
      if args.detect is not None:
        print('Detecting file ' + args.detect)
        in_file = open(args.detect, 'r')
        code = in_file.read()
        in_file.close()
        tabsize = 4
        wide = False
        tiebreak_keywords = True
        tiebreak_tokens = False
        languages = list(codesAndNames.keys())
        cProfile.run('identify_language(code, tabsize, wide, tiebreak_keywords, tiebreak_tokens, languages)')
      else:
        app.run()
