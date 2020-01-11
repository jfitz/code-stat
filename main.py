import argparse
import cProfile
import json
import codecs

from flask import Flask, request, render_template, Response

from codestat_exception import CodeStatException
from generic_code_examiner import GenericCodeExaminer
from ada_examiner import AdaExaminer
from awk_examiner import AwkExaminer
from basic_examiner import BasicExaminer
from basica_examiner import BasicaExaminer
from cbasic_examiner import CBasicExaminer
from c_examiner import CExaminer
from cobol_fixedformat_examiner import CobolFixedFormatExaminer
from cobol_freeformat_examiner import CobolFreeFormatExaminer
from cpp_examiner import CppExaminer
from csharp_examiner import CsharpExaminer
from fortran_fixedformat_examiner import FortranFixedFormatExaminer
from fortran_freeformat_examiner import FortranFreeFormatExaminer
from fsharp_examiner import FsharpExaminer
from go_examiner import GoExaminer
from html_examiner import HTMLExaminer
from java_examiner import JavaExaminer
from javascript_examiner import JavaScriptExaminer
from kotlin_examiner import KotlinExaminer
from objectivec_examiner import ObjectiveCExaminer
from pascal_examiner import PascalExaminer
from pl1_fixedformat_examiner import PL1FixedFormatExaminer
from pl1_freeformat_examiner import PL1FreeFormatExaminer
from prolog_examiner import PrologExaminer
from python_examiner import PythonExaminer
from r_examiner import RExaminer
from ruby_examiner import RubyExaminer
from rust_examiner import RustExaminer
from sql_examiner import SqlExaminer
from swift_examiner import SwiftExaminer
from typescript_examiner import TypeScriptExaminer
from visualbasic6_examiner import VisualBasic6Examiner
from visualbasicnet_examiner import VisualBasicNETExaminer

GenericCodeExaminer.__escape_z__()
AdaExaminer.__escape_z__()
AwkExaminer.__escape_z__()
BasicExaminer.__escape_z__()
BasicaExaminer.__escape_z__()
CBasicExaminer.__escape_z__()
CExaminer.__escape_z__()
CobolFixedFormatExaminer.__escape_z__()
CobolFreeFormatExaminer.__escape_z__()
CppExaminer.__escape_z__()
CsharpExaminer.__escape_z__()
FortranFixedFormatExaminer.__escape_z__()
FortranFreeFormatExaminer.__escape_z__()
FsharpExaminer.__escape_z__()
GoExaminer.__escape_z__()
HTMLExaminer.__escape_z__()
JavaExaminer.__escape_z__()
JavaScriptExaminer.__escape_z__()
KotlinExaminer.__escape_z__()
ObjectiveCExaminer.__escape_z__()
PascalExaminer.__escape_z__()
PL1FixedFormatExaminer.__escape_z__()
PL1FreeFormatExaminer.__escape_z__()
PrologExaminer.__escape_z__()
PythonExaminer.__escape_z__()
RExaminer.__escape_z__()
RubyExaminer.__escape_z__()
RustExaminer.__escape_z__()
SqlExaminer.__escape_z__()
SwiftExaminer.__escape_z__()
TypeScriptExaminer.__escape_z__()
VisualBasic6Examiner.__escape_z__()
VisualBasicNETExaminer.__escape_z__()

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


def extract_params(request_args):
  languages = ''
  if 'language' in request_args:
    languages = request_args['language'].lower().split(' ')

  comment = ''
  if 'comment' in request_args:
    comment = request_args['comment']

  tabsize = 8
  if 'tabsize' in request_args:
    tabsize = request_args['tabsize']

  try:
    tab_size = int(tabsize)
  except ValueError:
    tab_size = 8

  wide = 'wide' in request_args

  get_errors = 'errors' in request_args

  return languages, comment, tab_size, wide, get_errors


def find_winners(text, tab_size, wide, comment, languages):
  tiebreak_keywords = False
  tiebreak_tokens = False
  detected_languages, examiners = identify_language(text, tab_size, wide, comment, tiebreak_keywords, tiebreak_tokens, languages)

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

  return winning_languages, examiners


def build_language_list(languages, text, tab_size, wide, comment):
  examiners = None

  if len(languages) > 0:
    # detect for specified languages, pick the most confident
    winning_languages, examiners = find_winners(text, tab_size, wide, comment, languages)
  else:
    # tokenize as generic
    winning_languages = ['generic']
    examiners = {}
    examiner = make_one_examiner('generic', text, tab_size, wide, comment)
    examiners['generic'] = examiner

  return winning_languages, examiners


def dicts_to_json(list_of_dicts, languages, operation):
  if len(list_of_dicts) == 0:
    json_text = ''
  else:
    if len(list_of_dicts) == 1 and len(languages) == 1:
      # only 1 language, and the requestor knows it
      mydict = list_of_dicts[0]
      tokens = mydict[operation]
      json_text = json.dumps(tokens)
    else:
      # 1 or more languages, the requestor did not request a specific language
      json_text = json.dumps(list_of_dicts)
  
  return json_text


app = Flask(__name__)

codesAndNames = {
  'ada-83': 'Ada-83',
  'ada-95': 'Ada-95',
  'ada-2005': 'Ada-2005',
  'ada-2012': 'Ada-2012',
  'awk': 'Awk',
  'basic': 'BASIC',
  'basica': 'BASICA',
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
  'gawk': 'GNU-Awk',
  'go': 'Go',
  'java': 'Java',
  'javascript': 'JavaScript',
  'kotlin': 'Kotlin',
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


codesAndGroups = {
  'ada-83': 'Ada',
  'ada-95': 'Ada',
  'ada-2005': 'Ada',
  'ada-2012': 'Ada',
  'awk': 'Awk',
  'basic': 'BASIC',
  'basica': 'BASIC',
  'c': 'C',
  'cplusplus': 'C',
  'csharp': 'C#',
  'cbasic': 'CBASIC',
  'cobol-68': 'COBOL',
  'cobol-74': 'COBOL',
  'cobol-85': 'COBOL',
  'cobol-2002': 'COBOL',
  'cobol-2014': 'COBOL',
  'cobol-2014-acu': 'COBOL',
  'cobol-2014-ibm': 'COBOL',
  'cobol-2014-gnu': 'COBOL',
  'fortran-66': 'Fortran',
  'fortran-77': 'Fortran',
  'fortran-90': 'Fortran',
  'fortran-95': 'Fortran',
  'fortran-2003': 'Fortran',
  'fortran-2008': 'Fortran',
  'fsharp': 'F#',
  'gawk': 'Awk',
  'go': 'Go',
  'java': 'Java',
  'javascript': 'JavaScript',
  'kotlin': 'Kotlin',
  'html': 'HTML',
  'objective-c': 'Objective-C',
  'pascal': 'Pascal',
  'pl1-fixed': 'PL/1',
  'pl1-free': 'PL/1',
  'prolog': 'Prolog',
  'python': 'Python',
  'r': 'R',
  'ruby': 'Ruby',
  'rust': 'Rust',
  'sql-92': 'SQL',
  'sql-99': 'SQL',
  'sql-2003': 'SQL',
  'sql-2008': 'SQL',
  'sql-2011': 'SQL',
  'sql-2016': 'SQL',
  'swift': 'Swift',
  'typescript': 'TypeScript',
  'visualbasic-6': 'VisualBasic',
  'visualbasic-net': 'VisualBasic'
}


codesAndYears = {
  'ada-83': '1983',
  'ada-95': '1995',
  'ada-2005': '2005',
  'ada-2012': '2012',
  'awk': '1977',
  'basic': '1965',
  'basica': '1982',
  'c': '1970',
  'cplusplus': '1990',
  'csharp': '2000',
  'cbasic': '1980',
  'cobol-68': '1968',
  'cobol-74': '1974',
  'cobol-85': '1985',
  'cobol-2002': '2002',
  'cobol-2014': '2014',
  'cobol-2014-acu': '2014',
  'cobol-2014-ibm': '2014',
  'cobol-2014-gnu': '2014',
  'fortran-66': '1966',
  'fortran-77': '1977',
  'fortran-90': '1990',
  'fortran-95': '1995',
  'fortran-2003': '2003',
  'fortran-2008': '2008',
  'fsharp': '2010',
  'gawk': '1986',
  'go': '2010',
  'html': '1990',
  'java': '1995',
  'javascript': '1995',
  'kotlin': '2011',
  'objective-c': '1984',
  'pascal': '1970',
  'pl1-fixed': '1964',
  'pl1-free': '1992',
  'prolog': '1972',
  'python': '1991',
  'r': '1976',
  'ruby': '1995',
  'rust': '2010',
  'sql-92': '1992',
  'sql-99': '1999',
  'sql-2003': '2003',
  'sql-2008': '2008',
  'sql-2011': '2011',
  'sql-2016': '2016',
  'swift': '2014',
  'typescript': '2012',
  'visualbasic-6': '1998',
  'visualbasic-net': '2001'
}


@app.route('/languages', methods=['GET'])
def route_languages():
  json_text = json.dumps(codesAndNames)
  return json_text


@app.route('/detab', methods=['POST'])
def route_detab():
  _, _, tab_size, _, _ = extract_params(request.args)

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
  languages, _, _, _, _ = extract_params(request.args)

  if len(languages) > 0:
    language = languages[0]
  else:
    language = 'generic'

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)

  unwrapped_text = unwrap_lines(text, language.lower())

  return unwrapped_text


@app.route('/detect', methods=['POST'])
def route_detect():
  languages, _, tab_size, wide, comment = extract_params(request.args)

  if len(languages) == 0:
    languages = list(codesAndNames.keys())

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
    detected_languages, _ = identify_language(text, tab_size, wide, comment, tiebreak_keywords, tiebreak_tokens, languages)

    mydict = {}
    for key in detected_languages:
      new_key = codesAndNames[key]
      mydict[new_key] = detected_languages[key]

    json_text = json.dumps(mydict)

  except CodeStatException as e:
    http_status = 450
    json_text = str(e)

  return Response(json_text, status=http_status, mimetype='application/json')


@app.route('/tokens', methods=['POST'])
def route_tokens():
  languages, comment, tab_size, wide, _ = extract_params(request.args)

  if len(languages) == 0:
    languages = list(codesAndNames.keys())

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)

  http_status = 200
  try:
    winning_languages, examiners = build_language_list(languages, text, tab_size, wide, comment)

    list_of_dicts = []

    if examiners is not None:
      for winning_language in winning_languages:
        mydict = {}
        mydict['language'] = winning_language
        tokens = examiners[winning_language].tokens
        mydict['tokens'] = tokens_to_dict(tokens)

        list_of_dicts.append(mydict)

    json_text = dicts_to_json(list_of_dicts, languages, 'tokens')

  except CodeStatException as e:
    http_status = 450
    json_text = str(e)

  return Response(json_text, status=http_status, mimetype='application/json')


@app.route('/confidence', methods=['POST'])
def route_confidence():
  languages, comment, tab_size, wide, get_errors = extract_params(request.args)

  if len(languages) == 0:
    languages = list(codesAndNames.keys())

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)

  http_status = 200
  try:
    winning_languages, examiners = build_language_list(languages, text, tab_size, wide, comment)

    operation = ''
    list_of_dicts = []
    if examiners is not None:
      for winning_language in winning_languages:
        mydict = {}
        mydict['language'] = winning_language
        if get_errors:
          mydict['errors'] = examiners[winning_language].errors
          operation = 'errors'
        else:
          mydict['confidences'] = examiners[winning_language].confidences
          operation = 'confidences'

        list_of_dicts.append(mydict)

    json_text = dicts_to_json(list_of_dicts, languages, operation)

  except CodeStatException as e:
    http_status = 450
    json_text = str(e)

  return Response(json_text, status=http_status, mimetype='application/json')


@app.route('/statistics', methods=['POST'])
def route_statistics():
  languages, comment, tab_size, wide, _ = extract_params(request.args)

  if len(languages) == 0:
    languages = list(codesAndNames.keys())

  request_bytes = request.get_data()
  text = decode_bytes(request_bytes)

  http_status = 200
  try:
    winning_languages, examiners = build_language_list(languages, text, tab_size, wide, comment)

    list_of_dicts = []

    if examiners is not None:
      for winning_language in winning_languages:
        mydict = {}
        mydict['language'] = winning_language
        mydict['statistics'] = examiners[winning_language].statistics

        list_of_dicts.append(mydict)

    json_text = dicts_to_json(list_of_dicts, languages, 'statistics')

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


def make_one_examiner(language, code, tab_size, wide, comment):
  examiner = None

  if language in ['generic']:
    examiner = GenericCodeExaminer(code, comment)

  if language in ['ada-83']:
    examiner = AdaExaminer(code, '83')

  if language in ['ada-95']:
    examiner = AdaExaminer(code, '95')

  if language in ['ada-2005']:
    examiner = AdaExaminer(code, '2005')

  if language in ['ada-2012', 'ada']:
    examiner = AdaExaminer(code, '2012')

  if language in ['awk']:
    examiner = AwkExaminer(code, '')

  if language in ['gawk']:
    examiner = AwkExaminer(code, 'gnu')

  if language in ['basic', 'bas']:
    examiner = BasicExaminer(code)

  if language in ['basica']:
    examiner = BasicaExaminer(code)

  if language in ['c']:
    examiner = CExaminer(code)

  if language in ['c++', 'cplusplus']:
    examiner = CppExaminer(code)

  if language in ['c#', 'csharp']:
    examiner = CsharpExaminer(code)

  if language in ['cbasic']:
    examiner = CBasicExaminer(code)

  if language in ['cobol-68']:
    examiner = CobolFixedFormatExaminer(code, '68', '', tab_size, wide)

  if language in ['cobol-74']:
    examiner = CobolFixedFormatExaminer(code, '74', '', tab_size, wide)

  if language in ['cobol-85']:
    examiner = CobolFixedFormatExaminer(code, '85', '', tab_size, wide)

  if language in ['cobol-2002']:
    examiner = CobolFreeFormatExaminer(code, '2002', '')

  if language in ['cobol-2014', 'cobol', 'cob', 'cbl']:
    examiner = CobolFreeFormatExaminer(code, '2014', '')

  if language in ['cobol-2014-acu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'acu')

  if language in ['cobol-2014-ibm']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'ibm')

  if language in ['cobol-2014-gnu']:
    examiner = CobolFreeFormatExaminer(code, '2014', 'gnu')

  if language in ['fortran-66']:
    examiner = FortranFixedFormatExaminer(code, '66', tab_size, wide)

  if language in ['fortran-77', 'f77']:
    examiner = FortranFixedFormatExaminer(code, '77', tab_size, wide)

  if language in ['fortran-90', 'f90']:
    examiner = FortranFreeFormatExaminer(code, '90')

  if language in ['fortran-95', 'f95']:
    examiner = FortranFreeFormatExaminer(code, '95')

  if language in ['fortran-2003', 'f03']:
    examiner = FortranFreeFormatExaminer(code, '2003')

  if language in ['fortran-2008', 'f08', 'fortran', 'for', 'ftn']:
    examiner = FortranFreeFormatExaminer(code, '2008')

  if language in ['fsharp', 'fs']:
    examiner = FsharpExaminer(code)

  if language in ['go', 'golang']:
    examiner = GoExaminer(code)

  if language in ['html', 'php']:
    examiner = HTMLExaminer(code)

  if language in ['java', 'jav']:
    examiner = JavaExaminer(code)

  if language in ['javascript', 'js']:
    examiner = JavaScriptExaminer(code)

  if language in ['kotlin', 'kt']:
    examiner = KotlinExaminer(code)

  if language in ['objective-c', 'objc']:
    examiner = ObjectiveCExaminer(code)

  if language in ['pascal', 'pas']:
    examiner = PascalExaminer(code)

  if language in ['pl1-fixed']:
    examiner = PL1FixedFormatExaminer(code, tab_size, wide)

  if language in ['pl1-free', 'pl1']:
    examiner = PL1FreeFormatExaminer(code)

  if language in ['prolog']:
    examiner = PrologExaminer(code)

  if language in ['python', 'py']:
    examiner = PythonExaminer(code)

  if language in ['r', 'rmd']:
    examiner = RExaminer(code)

  if language in ['ruby', 'rb']:
    examiner = RubyExaminer(code)

  if language in ['rust', 'rs']:
    examiner = RustExaminer(code)

  if language in ['sql-92']:
    examiner = SqlExaminer(code, '92', '')

  if language in ['sql-99']:
    examiner = SqlExaminer(code, '99', '')

  if language in ['sql-2003']:
    examiner = SqlExaminer(code, '2003', '')

  if language in ['sql-2008']:
    examiner = SqlExaminer(code, '2008', '')

  if language in ['sql-2011']:
    examiner = SqlExaminer(code, '2011', '')

  if language in ['sql-2016', 'sql']:
    examiner = SqlExaminer(code, '2016', '')

  if language in ['swift']:
    examiner = SwiftExaminer(code)

  if language in ['typescript']:
    examiner = TypeScriptExaminer(code)

  if language in ['visualbasic-6']:
    examiner = VisualBasic6Examiner(code)

  if language in ['visualbasic-net']:
    examiner = VisualBasicNETExaminer(code)

  if examiner is None:
    examiner = GenericCodeExaminer(code, '')

  return examiner


def make_multiple_examiners(code, tab_size, wide, comment, languages):
  examiners = {}

  if 'generic' in languages:
    examiners['generic'] = GenericCodeExaminer(code, comment)

  if 'ada-83' in languages:
    examiners['ada-83'] = AdaExaminer(code, '83')

  if 'ada-95' in languages:
    examiners['ada-95'] = AdaExaminer(code, '95')

  if 'ada-2005' in languages:
    examiners['ada-2005'] = AdaExaminer(code, '2005')

  if 'ada-2012' in languages or 'ada' in languages:
    examiners['ada-2012'] = AdaExaminer(code, '2012')

  if 'awk' in languages:
    examiners['awk'] = AwkExaminer(code, '')

  if 'gawk' in languages:
    examiners['gawk'] = AwkExaminer(code, 'gnu')

  if 'basic' in languages or 'bas' in languages:
    examiners['basic'] = BasicExaminer(code)

  if 'basica' in languages:
    examiners['basica'] = BasicaExaminer(code)

  if 'cbasic' in languages:
    examiners['cbasic'] = CBasicExaminer(code)

  if 'c' in languages:
    examiners['c'] = CExaminer(code)

  if 'cplusplus' in languages or 'c++' in languages:
    examiners['cplusplus'] = CppExaminer(code)

  if 'csharp' in languages or 'cs' in languages or 'c#' in languages:
    examiners['csharp'] = CsharpExaminer(code)

  if 'cobol-68' in languages:
    examiners['cobol-68'] = CobolFixedFormatExaminer(code, '68', '', tab_size, wide)

  if 'cobol-74' in languages:
    examiners['cobol-74'] = CobolFixedFormatExaminer(code, '74', '', tab_size, wide)

  if 'cobol-85' in languages:
    examiners['cobol-85'] = CobolFixedFormatExaminer(code, '85', '', tab_size, wide)

  if 'cobol-2002' in languages:
    examiners['cobol-2002'] = CobolFreeFormatExaminer(code, '2002', '')

  if 'cobol-2014' in languages or 'cobol' in languages or 'cob' in languages:
    examiners['cobol-2014'] = CobolFreeFormatExaminer(code, '2014', '')

  if 'cobol-2014-acu' in languages:
    examiners['cobol-2014-acu'] = CobolFreeFormatExaminer(code, '2014', 'acu')

  if 'cobol-2014-ibm' in languages:
    examiners['cobol-2014-ibm'] = CobolFreeFormatExaminer(code, '2014', 'ibm')

  if 'cobol-2014-gnu' in languages:
    examiners['cobol-2014-gnu'] = CobolFreeFormatExaminer(code, '2014', 'gnu')

  if 'fortran-66' in languages or 'f66' in languages:
    examiners['fortran-66'] = FortranFixedFormatExaminer(code, '66', tab_size, wide)

  if 'fortran-77' in languages or 'f77' in languages:
    examiners['fortran-77'] = FortranFixedFormatExaminer(code, '77', tab_size, wide)

  if 'fortran-90' in languages or 'f90' in languages:
    examiners['fortran-90'] = FortranFreeFormatExaminer(code, '90')

  if 'fortran-95' in languages or 'f95' in languages:
    examiners['fortran-95'] = FortranFreeFormatExaminer(code, '95')

  if 'fortran-2003' in languages or 'f03' in languages:
    examiners['fortran-2003'] = FortranFreeFormatExaminer(code, '2003')

  if 'fortran-2008' in languages or 'fortran' in languages:
    examiners['fortran-2008'] = FortranFreeFormatExaminer(code, '2008')

  if 'fsharp' in languages or 'fs' in languages or 'f#' in languages:
    examiners['fsharp'] = FsharpExaminer(code)

  if 'go' in languages or 'golang' in languages:
    examiners['go'] = GoExaminer(code)

  if 'html' in languages:
    examiners['html'] = HTMLExaminer(code)

  if 'objective-c' in languages:
    examiners['objective-c'] = ObjectiveCExaminer(code)

  if 'java' in languages:
    examiners['java'] = JavaExaminer(code)

  if 'javascript' in languages or 'js' in languages:
    examiners['javascript'] = JavaScriptExaminer(code)

  if 'kotlin' in languages:
    examiners['kotlin'] = KotlinExaminer(code)

  if 'pascal' in languages or 'pas' in languages:
    examiners['pascal'] = PascalExaminer(code)

  if 'pl1-fixed' in languages:
    examiners['pl1-fixed'] = PL1FixedFormatExaminer(code, tab_size, wide)

  if 'pl1-free' in languages or 'pl1' in languages:
    examiners['pl1-free'] = PL1FreeFormatExaminer(code)

  if 'prolog' in languages:
    examiners['prolog'] = PrologExaminer(code)

  if 'python' in languages or 'py' in languages:
    examiners['python'] = PythonExaminer(code)

  if 'r' in languages:
    examiners['r'] = RExaminer(code)

  if 'ruby' in languages or 'rb' in languages:
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

  if 'visualbasic-net' in languages or 'vb' in languages:
    examiners['visualbasic-net'] = VisualBasicNETExaminer(code)

  return examiners


def identify_language(code, tab_size, wide, comment, tiebreak_keywords, tiebreak_tokens, languages):
  examiners = make_multiple_examiners(code, tab_size, wide, comment, languages)

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

  return retval, examiners


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


def tokenize(code, language, tab_size, wide, comment):
  retval = []

  examiner = make_one_examiner(language, code, tab_size, wide, comment)

  if examiner is not None:
    retval = examiner.tokens

  return retval


def tokenize_confidence(code, language, tab_size, get_errors, wide, comment):
  retval = []

  examiner = make_one_examiner(language, code, tab_size, wide, comment)

  if examiner is not None:
    if get_errors:
      retval = examiner.errors
    else:
      retval = examiner.confidences

  return retval


def tokenize_statistics(code, language, tab_size, wide, comment):
  retval = []

  examiner = make_one_examiner(language, code, tab_size, wide, comment)

  if examiner is not None:
    retval = examiner.statistics

  return retval


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
    language = 'rust'
    tab_size = 4
    wide = False
    tokenize(code, language, tab_size, wide, '')
    # cProfile.run("tokenize(code, language, tab_size, wide, '')")
  else:
    if args.confidence is not None:
      print('Confidencing file ' + args.confidence)
      in_file = open(args.confidence, 'r')
      code = in_file.read()
      in_file.close()
      language = 'ruby'
      tab_size = 4
      wide = False
      cProfile.run("tokenize_confidence(code, language, False, tab_size, wide, '')")
    else:
      if args.detect is not None:
        print('Detecting file ' + args.detect)
        in_file = open(args.detect, 'r')
        code = in_file.read()
        in_file.close()
        tab_size = 4
        wide = False
        tiebreak_keywords = True
        tiebreak_tokens = False
        comment = ''
        languages = list(codesAndNames.keys())
        cProfile.run('identify_language(code, tab_size, wide, comment, tiebreak_keywords, tiebreak_tokens, languages)')
      else:
        app.run()
