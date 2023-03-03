import argparse
import cProfile
import json
import codecs

from flask import Flask, request, render_template, Response

from codestat_exception import CodeStatException
from generic_code_examiner import GenericCodeExaminer
from assembly_generic_examiner import AssemblyGenericExaminer
from assembly_ibm_examiner import AssemblyIBMExaminer
from assembly_examiner import AssemblyExaminer
from ada_examiner import AdaExaminer
from awk_examiner import AwkExaminer
from basic_examiner import BasicExaminer
from basic_bbc_examiner import BasicBbcExaminer
from cbasic_examiner import CBasicExaminer
from c_examiner import CExaminer
from cobol_examiner import CobolExaminer
from coffeescript_examiner import CoffeeScriptExaminer
from cpp_examiner import CppExaminer
from csharp_examiner import CsharpExaminer
from d_examiner import DExaminer
from dart_examiner import DartExaminer
from dbase_examiner import DbaseExaminer
from delphi_examiner import DelphiExaminer
from dibol_examiner import DibolExaminer
from eiffel_examiner import EiffelExaminer
from erlang_examiner import ErlangExaminer
from flowmatic_examiner import FlowmaticExaminer
from fortran_examiner import FortranExaminer
from go_examiner import GoExaminer
from groovy_examiner import GroovyExaminer
from haskell_examiner import HaskellExaminer
from html_examiner import HTMLExaminer
from intercal_examiner import IntercalExaminer
from java_examiner import JavaExaminer
from javascript_examiner import JavaScriptExaminer
from julia_examiner import JuliaExaminer
from kotlin_examiner import KotlinExaminer
from latino_examiner import LatinoExaminer
from lua_examiner import LuaExaminer
from matlab_examiner import MatlabExaminer
from ml_examiner import MlExaminer
from modula2_examiner import Modula2Examiner
from objectivec_examiner import ObjectiveCExaminer
from pascal_examiner import PascalExaminer
from perl_examiner import PerlExaminer
from pl1_examiner import PL1Examiner
from plm_examiner import PLMExaminer
from prolog_examiner import PrologExaminer
from python_examiner import PythonExaminer
from r_examiner import RExaminer
from ruby_examiner import RubyExaminer
from rust_examiner import RustExaminer
from scala_examiner import ScalaExaminer
from sql_examiner import SqlExaminer
from swift_examiner import SwiftExaminer
from typescript_examiner import TypeScriptExaminer
from visualbasic6_examiner import VisualBasic6Examiner
from visualbasicnet_examiner import VisualBasicNETExaminer

GenericCodeExaminer.__escape_z__()
AdaExaminer.__escape_z__()
AssemblyGenericExaminer.__escape_z__()
AssemblyIBMExaminer.__escape_z__()
AssemblyExaminer.__escape_z__()
AwkExaminer.__escape_z__()
BasicExaminer.__escape_z__()
BasicBbcExaminer.__escape_z__()
CBasicExaminer.__escape_z__()
CExaminer.__escape_z__()
CobolExaminer.__escape_z__()
CoffeeScriptExaminer.__escape_z__()
CppExaminer.__escape_z__()
CsharpExaminer.__escape_z__()
DExaminer.__escape_z__()
DartExaminer.__escape_z__()
DbaseExaminer.__escape_z__()
DelphiExaminer.__escape_z__()
DibolExaminer.__escape_z__()
EiffelExaminer.__escape_z__()
ErlangExaminer.__escape_z__()
FlowmaticExaminer.__escape_z__()
FortranExaminer.__escape_z__()
GoExaminer.__escape_z__()
GroovyExaminer.__escape_z__()
HaskellExaminer.__escape_z__()
HTMLExaminer.__escape_z__()
IntercalExaminer.__escape_z__()
JavaExaminer.__escape_z__()
JavaScriptExaminer.__escape_z__()
JuliaExaminer.__escape_z__()
KotlinExaminer.__escape_z__()
LatinoExaminer.__escape_z__()
LuaExaminer.__escape_z__()
MatlabExaminer.__escape_z__()
MlExaminer.__escape_z__()
Modula2Examiner.__escape_z__()
ObjectiveCExaminer.__escape_z__()
PascalExaminer.__escape_z__()
PerlExaminer.__escape_z__()
PL1Examiner.__escape_z__()
PLMExaminer.__escape_z__()
PrologExaminer.__escape_z__()
PythonExaminer.__escape_z__()
RExaminer.__escape_z__()
RubyExaminer.__escape_z__()
RustExaminer.__escape_z__()
ScalaExaminer.__escape_z__()
SqlExaminer.__escape_z__()
SwiftExaminer.__escape_z__()
TypeScriptExaminer.__escape_z__()
VisualBasic6Examiner.__escape_z__()
VisualBasicNETExaminer.__escape_z__()


def decode_bytes(in_bytes):
  text = ''
  i = len(in_bytes)

  if i > 0:
    if in_bytes[0] not in [0xff, 0xfe]:
      # remove trailing NUL bytes
      while i > 0 and in_bytes[i - 1] == 0:
        i -= 1

      in_bytes = in_bytes[0:i]

    # convert to text
    try:
      text = in_bytes.decode('ascii')
    except UnicodeDecodeError:
      try:
        text = in_bytes.decode('utf-8')
      except UnicodeDecodeError:
        try:
          text = in_bytes.decode('utf-16')
        except UnicodeDecodeError:
          raise CodeStatException('Code cannot be decoded as ASCII or UNICODE')

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

  format = 'better'
  if 'format' in request_args:
    format = request_args['format']

  return languages, comment, tab_size, format


def find_winners(text, format, tab_size, comment, block_comment_limit, languages):
  tiebreak_keywords = False
  tiebreak_tokens = False
  tiebreak_simple = False
  detected_languages, examiners = identify_language(
    text, format, tab_size, comment, block_comment_limit, tiebreak_keywords,
    tiebreak_tokens, tiebreak_simple, languages)

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


def build_language_list(languages, text, format, tab_size, comment, block_comment_limit):
  examiners = None

  if len(languages) > 0:
    # detect for specified languages, pick the most confident
    winning_languages, examiners = find_winners(
      text, format, tab_size, comment, block_comment_limit, languages)
  else:
    # tokenize as generic
    winning_languages = ['generic']
    examiners = {}
    examiner = make_one_examiner('generic', text, format, tab_size, comment, block_comment_limit)
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

codes_and_names = {
  'ada-83': 'Ada-83',
  'ada-95': 'Ada-95',
  'ada-2005': 'Ada-2005',
  'ada-2012': 'Ada-2012',
  'assembly': 'Assembly',
  'asm-360': 'ASM-360',
  'asm-370': 'ASM-370',
  'asm-390': 'ASM-390',
  'asm-system-z': 'ASM-System-Z',
  'asm-1802': 'ASM-1802',
  'asm-6502': 'ASM-6502',
  'asm-6800': 'ASM-6800',
  'asm-68000': 'ASM-68000',
  'asm-8080': 'ASM-8080',
  'asm-z-80': 'ASM-Z-80',
  'asm-8086': 'ASM-8086',
  'asm-80286': 'ASM-80286',
  'asm-80386': 'ASM-80386',
  'asm-80486': 'ASM-80486',
  'asm-pdp-8': 'ASM-PDP-8',
  'asm-pdp-11': 'ASM-PDP-11',
  'awk': 'Awk',
  'basic': 'BASIC',
  'basica': 'BASICA',
  'basic-80': 'BASIC-80',
  'bbc-basic': 'BBC-Basic',
  'basic-1965': 'BASIC-1965',
  'basic-1973': 'BASIC-1973',
  'basic-1978': 'BASIC-1978',
  'c-78': 'C-78',
  'c-89': 'C-89',
  'c-99': 'C-99',
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
  'coffeescript': 'CoffeeScript',
  'd': 'D',
  'dart': 'Dart',
  'dbase-ii': 'dBase-II',
  'dbase-iii': 'dBase-III',
  'delphi': 'Delphi',
  'dibol': 'Dibol',
  'eiffel': 'Eiffel',
  'erlang': 'Erlang',
  'flowmatic': 'Flowmatic',
  'fortran-66': 'FORTRAN-66',
  'fortran-77': 'FORTRAN-77',
  'fortran-90': 'Fortran-90',
  'fortran-95': 'Fortran-95',
  'fortran-2003': 'Fortran-2003',
  'fortran-2008': 'Fortran-2008',
  'fsharp': 'F#',
  'gawk': 'GNU-Awk',
  'go': 'Go',
  'groovy': 'Groovy',
  'haskell': 'Haskell',
  'html': 'HTML',
  'intercal': 'INTERCAL',
  'java': 'Java',
  'javascript': 'JavaScript',
  'julia': 'Julia',
  'kotlin': 'Kotlin',
  'latino': 'Latino',
  'lua': 'Lua',
  'matlab': 'Matlab',
  'modula-2': 'Modula-2',
  'objective-c': 'Objective-C',
  'ocaml': 'OCaml',
  'octave': 'Octave',
  'pascal': 'Pascal',
  'perl': 'Perl',
  'pl1': 'PL/1',
  'plm': 'PL/M',
  'prolog': 'Prolog',
  'python': 'Python',
  'r': 'R',
  'ruby': 'Ruby',
  'rust': 'Rust',
  'scala': 'Scala',
  'sql': 'SQL',
  't-sql': 'T-SQL',
  'pl-sql': 'PL/SQL',
  'swift': 'Swift',
  'typescript': 'TypeScript',
  'visualbasic-6': 'VisualBasic-6',
  'visualbasic-net': 'VisualBasic-NET'
}


codes_and_groups = {
  'ada-83': 'Ada',
  'ada-95': 'Ada',
  'ada-2005': 'Ada',
  'ada-2012': 'Ada',
  'assembly': 'Assembly',
  'asm-360': 'Assembly',
  'asm-370': 'Assembly',
  'asm-390': 'Assembly',
  'asm-system-z': 'Assembly',
  'asm-1802': 'Assembly',
  'asm-6502': 'Assembly',
  'asm-6800': 'Assembly',
  'asm-68000': 'Assembly',
  'asm-8080': 'Assembly',
  'asm-z-80': 'Assembly',
  'asm-8086': 'Assembly',
  'asm-80286': 'Assembly',
  'asm-80386': 'Assembly',
  'asm-80486': 'Assembly',
  'asm-pdp-8': 'Assembly',
  'asm-pdp-11': 'Assembly',
  'awk': 'Awk',
  'basic': 'BASIC',
  'basica': 'BASIC',
  'basic-80': 'BASIC',
  'bbc-basic': 'BASIC',
  'basic-1965': 'BASIC',
  'basic-1973': 'BASIC',
  'basic-1978': 'BASIC',
  'c-78': 'C',
  'c-89': 'C',
  'c-99': 'C',
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
  'coffeescript': 'JavaScript',
  'd': 'D',
  'dart': 'Dart',
  'dbase-ii': 'dBase',
  'dbase-iii': 'dBase',
  'delphi': 'Pascal',
  'dibol': 'Dibol',
  'eiffel': 'Eiffel',
  'erlang': 'Erlang',
  'flowmatic': 'Flowmatic',
  'fortran-66': 'Fortran',
  'fortran-77': 'Fortran',
  'fortran-90': 'Fortran',
  'fortran-95': 'Fortran',
  'fortran-2003': 'Fortran',
  'fortran-2008': 'Fortran',
  'fsharp': 'ML',
  'gawk': 'Awk',
  'go': 'Go',
  'groovy': 'Groovy',
  'haskell': 'Haskell',
  'html': 'HTML',
  'intercal': 'INTERCAL',
  'java': 'Java',
  'javascript': 'JavaScript',
  'julia': 'Julia',
  'kotlin': 'Kotlin',
  'latino': 'Latino',
  'lua': 'Lua',
  'matlab': 'Matlab',
  'modula-2': 'Modula-2',
  'objective-c': 'Objective-C',
  'ocaml': 'ML',
  'octave': 'matlab',
  'pascal': 'Pascal',
  'perl': 'Perl',
  'pl1': 'PL/1',
  'plm': 'PL/1',
  'prolog': 'Prolog',
  'python': 'Python',
  'r': 'R',
  'ruby': 'Ruby',
  'rust': 'Rust',
  'scala': 'Scala',
  'sql': 'SQL',
  't-sql': 'SQL',
  'pl-sql': 'SQL',
  'swift': 'Swift',
  'typescript': 'JavaScript',
  'visualbasic-6': 'VisualBasic',
  'visualbasic-net': 'VisualBasic'
}


codes_and_years = {
  'ada-83': 1983,
  'ada-95': 1995,
  'ada-2005': 2005,
  'ada-2012': 2012,
  'assembly': 1952,
  'asm-360': 1964,
  'asm-370': 1968,
  'asm-390': 1990,
  'asm-system-z': 2000,
  'asm-1802': 1973,
  'asm-6502': 1974,
  'asm-6800': 1974,
  'asm-68000': 1979,
  'asm-8080': 1974,
  'asm-z-80': 1976,
  'asm-8086': 1979,
  'asm-80286': 1982,
  'asm-80386': 1986,
  'asm-80486': 1990,
  'asm-pdp-8': 1968,
  'asm-pdp-11': 1970,
  'awk': 1977,
  'basic': 1965,
  'basica': 1982,
  'basic-80': 1980,
  'bbc-basic': 1981,
  'basic-1965': 1965,
  'basic-1973': 1973,
  'basic-1978': 1978,
  'c-78': 1978,
  'c-89': 1989,
  'c-99': 1999,
  'caml': 1987,
  'cplusplus': 1990,
  'csharp': 2000,
  'cbasic': 1980,
  'cobol-68': 1968,
  'cobol-74': 1974,
  'cobol-85': 1985,
  'cobol-2002': 2002,
  'cobol-2014': 2014,
  'cobol-2014-acu': 2014,
  'cobol-2014-ibm': 2014,
  'cobol-2014-gnu': 2014,
  'coffeescript': 2009,
  'd': 2001,
  'dart': 2011,
  'dbase-ii': 1982,
  'dbase-iii': 1985,
  'delphi': 1995,
  'dibol': 1970,
  'eiffel': 1985,
  'erlang': 1985,
  'flowmatic': 1957,
  'fortran-66': 1966,
  'fortran-77': 1977,
  'fortran-90': 1990,
  'fortran-95': 1995,
  'fortran-2003': 2003,
  'fortran-2008': 2008,
  'fsharp': 2010,
  'gawk': 1986,
  'go': 2010,
  'groovy': 2007,
  'haskell': 1998,
  'html': 1990,
  'intercal': 1972,
  'java': 1995,
  'javascript': 1995,
  'julia': 2009,
  'kotlin': 2011,
  'latino': 2015,
  'lua': 1993,
  'matlab': 1984,
  'modula-2': 1977,
  'objective-c': 1984,
  'ocaml': 2011,
  'octave': 1993,
  'pascal': 1970,
  'perl': 1980,
  'pl1': 1964,
  'plm': 1978,
  'prolog': 1972,
  'python': 1991,
  'r': 1976,
  'ruby': 1995,
  'rust': 2010,
  'scala': 2001,
  'sql': 1972,
  't-sql': 1989,
  'pl-sql': 1979,
  'swift': 2014,
  'typescript': 2012,
  'visualbasic-6': 1998,
  'visualbasic-net': 2001
}

simpler_languages = {
  'ada-83': None,
  'ada-95': 'ada-83',
  'ada-2005': 'ada-95',
  'ada-2012': 'ada-2005',
  'assembly': None,
  'asm-360': None,
  'asm-370': 'asm-360',
  'asm-390': 'asm-370',
  'asm-system-z': 'asm-390',
  'asm-1802': None,
  'asm-6502': None,
  'asm-6800': None,
  'asm-68000': None,
  'asm-8080': None,
  'asm-z-80': None,
  'asm-8086': None,
  'asm-80286': 'asm-8086',
  'asm-80386': 'asm-80286',
  'asm-80486': 'asm-80386',
  'asm-pdp-8': None,
  'asm-pdp-11': None,
  'awk': None,
  'basic': None,
  'basica': 'basic-80',
  'basic-80': 'basic',
  'bbc-basic': 'basic-80',
  'basic-1965': 'basic',
  'basic-1973': 'basic-1965',
  'basic-1978': 'basic-1973',
  'c-78': None,
  'c-89': 'c-78',
  'c-99': 'c-89',
  'cplusplus': 'c-99',
  'csharp': 'java',
  'cbasic': 'basic',
  'cobol-68': None,
  'cobol-74': 'cobol-68',
  'cobol-85': 'cobol-74',
  'cobol-2002': 'cobol-85',
  'cobol-2014': 'cobol-2002',
  'cobol-2014-acu': 'cobol-2014',
  'cobol-2014-ibm': 'cobol-2014',
  'cobol-2014-gnu': 'cobol-2014',
  'coffeescript': 'javascript',
  'd': None,
  'dart': None,
  'dbase-ii': None,
  'dbase-iii': 'dbase-ii',
  'delphi': 'pascal',
  'dibol': None,
  'eiffel': None,
  'erlang': None,
  'fortran-66': None,
  'fortran-77': 'fortran-66',
  'fortran-90': 'fortran-77',
  'fortran-95': 'fortran-90',
  'fortran-2003': 'fortran-95',
  'fortran-2008': 'fortran-2003',
  'fsharp': 'ocaml',
  'gawk': 'awk',
  'go': 'pascal',
  'groovy': None,
  'haskell': None,
  'html': None,
  'intercal': None,
  'java': None,
  'javascript': None,
  'julia': None,
  'kotlin': None,
  'latino': None,
  'lua': None,
  'matlab': None,
  'modula-2': 'pascal',
  'objective-c': 'cplusplus',
  'octave': 'matlab',
  'ocaml': None,
  'pascal': None,
  'perl': 'awk',
  'pl1': None,
  'plm': 'pl1',
  'prolog': None,
  'python': None,
  'r': None,
  'ruby': None,
  'rust': None,
  'scala': None,
  'sql': None,
  't-sql': 'sql',
  'pl-sql': 'sql',
  'swift': None,
  'typescript': 'javascript',
  'visualbasic-6': None,
  'visualbasic-net': 'visualbasic-6'
}

override_language = {
  'asm-360': 'assembly',
  'asm-370': 'assembly',
  'asm-390': 'assembly',
  'asm-system-z': 'assembly',
  'asm-1802': 'assembly',
  'asm-6502': 'assembly',
  'asm-6800': 'assembly',
  'asm-68000': 'assembly',
  'asm-8080': 'assembly',
  'asm-8086': 'assembly',
  'asm-80286': 'assembly',
  'asm-80386': 'assembly',
  'asm-80486': 'assembly',
  'asm-z-80': 'assembly',
  'asm-pdp-8': 'assembly',
  'asm-pdp-11': 'assembly',
  'asm-ibm-360': 'assembly',
  'asm-ibm-370': 'assembly',
  'asm-ibm-390': 'assembly',
  'asm-ibm-series-z': 'assembly'
}


@app.route('/languages', methods=['GET'])
def route_languages():
  json_text = json.dumps(codes_and_names)
  return Response(json_text, mimetype='application/json')


@app.route('/simple', methods=['GET'])
def route_simple():
  json_text = json.dumps(simpler_languages)
  return Response(json_text, mimetype='application/json')


@app.route('/override', methods=['GET'])
def route_override():
  json_text = json.dumps(override_language)
  return Response(json_text, mimetype='application/json')


@app.route('/detab', methods=['POST'])
def route_detab():
  _, _, tab_size, _ = extract_params(request.args)

  request_bytes = request.get_data()

  http_status = 200
  mime_type = 'text/plain'
  try:
    text = decode_bytes(request_bytes)
    response_text = tabs_to_spaces(text, tab_size)

  except CodeStatException as e:
    http_status = 450
    mime_type = 'application/json'
    response_text = str(e)

  return Response(response_text, status=http_status, mimetype=mime_type)


@app.route('/truncate', methods=['POST'])
def route_truncate():
  request_bytes = request.get_data()

  http_status = 200
  mime_type = 'text/plain'
  try:
    text = decode_bytes(request_bytes)
    response_text = truncate_lines(text, 72)

  except CodeStatException as e:
    http_status = 450
    mime_type = 'application/json'
    response_text = str(e)

  return Response(response_text, status=http_status, mimetype=mime_type)


@app.route('/unwrap', methods=['POST'])
def route_unwrap():
  languages, _, _, _ = extract_params(request.args)

  if len(languages) > 0:
    language = languages[0]
  else:
    language = 'generic'

  request_bytes = request.get_data()

  http_status = 200
  mime_type = 'text/plain'
  try:
    text = decode_bytes(request_bytes)
    response_text = unwrap_lines(text, language.lower())

  except CodeStatException as e:
    http_status = 450
    mime_type = 'application/json'
    response_text = str(e)

  return Response(response_text, status=http_status, mimetype=mime_type)


@app.route('/detect', methods=['POST'])
def route_detect():
  languages, comment, tab_size, format = extract_params(request.args)

  if len(languages) == 0:
    languages = list(codes_and_names.keys())

  tiebreak_keywords = True
  tiebreak_tokens = False
  tiebreak_simple = True

  if 'notiebreak' in request.args:
    tiebreak_keywords = False
    tiebreak_tokens = False
    tiebreak_simple = False

  block_comment_limit = 2048

  request_bytes = request.get_data()

  http_status = 200
  try:
    text = decode_bytes(request_bytes)
    format = 'better'
    detected_languages, _ = identify_language(text, format, tab_size, comment, block_comment_limit, tiebreak_keywords, tiebreak_tokens, tiebreak_simple, languages)

    mydict = {}
    for key in detected_languages:
      new_key = codes_and_names[key]
      mydict[new_key] = detected_languages[key]

    json_text = json.dumps(mydict)

  except CodeStatException as e:
    http_status = 450
    json_text = str(e)

  return Response(json_text, status=http_status, mimetype='application/json')


@app.route('/tokens', methods=['POST'])
def route_tokens():
  languages, comment, tab_size, format = extract_params(request.args)

  if len(languages) == 0:
    languages = list(codes_and_names.keys())

  block_comment_limit = 2048

  request_bytes = request.get_data()

  http_status = 200
  try:
    text = decode_bytes(request_bytes)
    winning_languages, examiners = build_language_list(languages, text, format, tab_size, comment, block_comment_limit)

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
  languages, comment, tab_size, format = extract_params(request.args)
  get_errors = False

  if len(languages) == 0:
    languages = list(codes_and_names.keys())

  block_comment_limit = 2048

  request_bytes = request.get_data()

  http_status = 200
  try:
    text = decode_bytes(request_bytes)
    winning_languages, examiners = build_language_list(languages, text, format, tab_size, comment, block_comment_limit)

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


@app.route('/confidence-errors', methods=['POST'])
def route_confidence_errors():
  languages, comment, tab_size, format = extract_params(request.args)
  get_errors = True

  if len(languages) == 0:
    languages = list(codes_and_names.keys())

  block_comment_limit = 2048

  request_bytes = request.get_data()

  http_status = 200
  try:
    text = decode_bytes(request_bytes)
    winning_languages, examiners = build_language_list(languages, text, format, tab_size, comment, block_comment_limit)

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
  languages, comment, tab_size, format = extract_params(request.args)

  if len(languages) == 0:
    languages = list(codes_and_names.keys())

  request_bytes = request.get_data()

  block_comment_limit = 2048

  http_status = 200
  try:
    text = decode_bytes(request_bytes)
    winning_languages, examiners = build_language_list(languages, text, format, tab_size, comment, block_comment_limit)

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


def make_one_examiner(language, code, format, tab_size, comment, block_comment_limit):
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

  if language in ['assembly']:
    examiner = AssemblyGenericExaminer(code, tab_size, format)

  if language in ['asm-360']:
    examiner = AssemblyIBMExaminer(code, tab_size, '360', format)

  if language in ['asm-370']:
    examiner = AssemblyIBMExaminer(code, tab_size, '370', format)

  if language in ['asm-390']:
    examiner = AssemblyIBMExaminer(code, tab_size, '390', format)

  if language in ['asm-system-z']:
    examiner = AssemblyIBMExaminer(code, tab_size, 'system-z', format)

  if language in ['asm-1802']:
    examiner = AssemblyExaminer(code, tab_size, '1802', format)

  if language in ['asm-6502']:
    examiner = AssemblyExaminer(code, tab_size, '6502', format)

  if language in ['asm-6800']:
    examiner = AssemblyExaminer(code, tab_size, '6800', format)

  if language in ['asm-68000']:
    examiner = AssemblyExaminer(code, tab_size, '68000', format)

  if language in ['asm-8080']:
    examiner = AssemblyExaminer(code, tab_size, '8080', format)

  if language in ['asm-z-80']:
    examiner = AssemblyExaminer(code, tab_size, 'z80', format)

  if language in ['asm-8086', 'asm-8088']:
    examiner = AssemblyExaminer(code, tab_size, '8086', format)

  if language in ['asm-80286']:
    examiner = AssemblyExaminer(code, tab_size, '80286', format)

  if language in ['asm-80386', '80388']:
    examiner = AssemblyExaminer(code, tab_size, '80386', format)

  if language in ['asm-80486', 'asm-80488']:
    examiner = AssemblyExaminer(code, tab_size, '80486', format)

  if language in ['asm-pdp-8']:
    examiner = AssemblyExaminer(code, tab_size, 'pdp-8', format)

  if language in ['asm-pdp-11']:
    examiner = AssemblyExaminer(code, tab_size, 'pdp-11', format)

  if language in ['awk']:
    examiner = AwkExaminer(code, '')

  if language in ['gawk']:
    examiner = AwkExaminer(code, 'gnu')

  if language in ['basic', 'bas']:
    examiner = BasicExaminer(code, '')

  if language in ['basic-80', 'mbasic', 'mba']:
    examiner = BasicExaminer(code, 'basic-80')

  if language in ['basica']:
    examiner = BasicExaminer(code, 'basica')

  if language in ['basic-1965']:
    examiner = BasicExaminer(code, 'basic-1965')

  if language in ['basic-1973']:
    examiner = BasicExaminer(code, 'basic-1973')

  if language in ['basic-1978']:
    examiner = BasicExaminer(code, 'basic-1978')

  if language in ['bbc-basic']:
    examiner = BasicBbcExaminer(code)

  if language in ['c-78', 'c', 'c-k&r']:
    examiner = CExaminer(code, '78')

  if language in ['c-89', 'c']:
    examiner = CExaminer(code, '89')

  if language in ['c-99', 'c']:
    examiner = CExaminer(code, '99')

  if language in ['c++', 'cplusplus']:
    examiner = CppExaminer(code)

  if language in ['c#', 'csharp']:
    examiner = CsharpExaminer(code)

  if language in ['cbasic']:
    examiner = CBasicExaminer(code)

  if language in ['cobol-68']:
    examiner = CobolExaminer(code, '68', '', tab_size, format)

  if language in ['cobol-74']:
    examiner = CobolExaminer(code, '74', '', tab_size, format)

  if language in ['cobol-85']:
    examiner = CobolExaminer(code, '85', '', tab_size, format)

  if language in ['cobol-2002']:
    examiner = CobolExaminer(code, '2002', '', tab_size, format)

  if language in ['cobol-2014', 'cobol']:
    examiner = CobolExaminer(code, '2014', '', tab_size, format)

  if language in ['cobol-2014-acu']:
    examiner = CobolExaminer(code, '2014', 'acu', tab_size, format)

  if language in ['cobol-2014-ibm']:
    examiner = CobolExaminer(code, '2014', 'ibm', tab_size, format)

  if language in ['cobol-2014-gnu']:
    examiner = CobolExaminer(code, '2014', 'gnu', tab_size, format)

  if language in ['coffeescript', 'coffee']:
    examiner = CoffeeScriptExaminer(code)

  if language in ['d']:
    examiner = DExaminer(code, block_comment_limit)

  if language in ['dart']:
    examiner = DartExaminer(code)

  if language in ['dbase-ii', 'dbase', 'prg']:
    examiner = DbaseExaminer(code, 'ii', format)

  if language in ['dbase-iii', 'dbase', 'prg']:
    examiner = DbaseExaminer(code, 'iii', format)

  if language in ['delphi']:
    examiner = DelphiExaminer(code)

  if language in ['dibol']:
    examiner = DibolExaminer(code)

  if language in ['eiffel']:
    examiner = EiffelExaminer(code)

  if language in ['erlang']:
    examiner = ErlangExaminer(code)

  if language in ['flowmatic']:
    examiner = FlowmaticExaminer(code)

  if language in ['fortran-66']:
    examiner = FortranExaminer(code, '66', tab_size, format)

  if language in ['fortran-77', 'f77']:
    examiner = FortranExaminer(code, '77', tab_size, format)

  if language in ['fortran-90', 'f90']:
    examiner = FortranExaminer(code, '90', tab_size, format)

  if language in ['fortran-95', 'f95']:
    examiner = FortranExaminer(code, '95', tab_size, format)

  if language in ['fortran-2003', 'fortran', 'f03']:
    examiner = FortranExaminer(code, '2003', tab_size, format)

  if language in ['fortran-2008', 'fortran', 'f08', 'for', 'ftn']:
    examiner = FortranExaminer(code, '2008', tab_size, format)

  if language in ['fsharp', 'fs']:
    examiner = MlExaminer(code, 'fsharp')

  if language in ['go', 'golang']:
    examiner = GoExaminer(code)

  if language in ['groovy']:
    examiner = GroovyExaminer(code)

  if language in ['haskell']:
    examiner = HaskellExaminer(code)

  if language in ['html', 'php']:
    examiner = HTMLExaminer(code)

  if language in ['intercal']:
    examiner = IntercalExaminer(code)

  if language in ['java', 'jav']:
    examiner = JavaExaminer(code)

  if language in ['javascript', 'js']:
    examiner = JavaScriptExaminer(code)

  if language in ['julia', 'jl']:
    examiner = JuliaExaminer(code, block_comment_limit)

  if language in ['kotlin', 'kt']:
    examiner = KotlinExaminer(code)

  if language in ['latino']:
    examiner = LatinoExaminer(code)

  if language in ['lua']:
    examiner = LuaExaminer(code)

  if language in ['matlab', 'm']:
    examiner = MatlabExaminer(code, 'matlab')

  if language in ['modula-2', 'mod']:
    examiner = Modula2Examiner(code)

  if language in ['objective-c', 'objc']:
    examiner = ObjectiveCExaminer(code)

  if language in ['ocaml']:
    examiner = MlExaminer(code, 'ocaml')

  if language in ['octave']:
    examiner = MatlabExaminer(code, 'octave')

  if language in ['pascal', 'pas']:
    examiner = PascalExaminer(code)

  if language in ['perl', 'pl', 'pm']:
    examiner = PerlExaminer(code)

  if language in ['pl1']:
    examiner = PL1Examiner(code, tab_size, format)

  if language in ['plm']:
    examiner = PLMExaminer(code, tab_size, format)

  if language in ['prolog']:
    examiner = PrologExaminer(code)

  if language in ['python', 'py']:
    examiner = PythonExaminer(code)

  if language in ['r', 'rmd']:
    examiner = RExaminer(code)

  if language in ['ruby', 'rb']:
    examiner = RubyExaminer(code)

  if language in ['rust', 'rs']:
    examiner = RustExaminer(code, block_comment_limit)

  if language in ['scala']:
    examiner = ScalaExaminer(code)

  if language in ['sql']:
    examiner = SqlExaminer(code, 'sql')

  if language in ['t-sql']:
    examiner = SqlExaminer(code, 't-sql')

  if language in ['pl-sql']:
    examiner = SqlExaminer(code, 'pl-sql')

  if language in ['swift']:
    examiner = SwiftExaminer(code)

  if language in ['typescript']:
    examiner = TypeScriptExaminer(code)

  if language in ['visualbasic-6', 'visualbasic', 'vb']:
    examiner = VisualBasic6Examiner(code)

  if language in ['visualbasic-net', 'visualbasic', 'vb']:
    examiner = VisualBasicNETExaminer(code)

  if examiner is None:
    examiner = GenericCodeExaminer(code, '')

  return examiner


def make_multiple_examiners(code, format, tab_size, comment, block_comment_limit, languages):
  examiners = {}

  if 'generic' in languages:
    examiners['generic'] = GenericCodeExaminer(code, comment)

  if 'ada-83' in languages or 'ada' in languages:
    examiners['ada-83'] = AdaExaminer(code, '83')

  if 'ada-95' in languages or 'ada' in languages:
    examiners['ada-95'] = AdaExaminer(code, '95')

  if 'ada-2005' in languages or 'ada' in languages:
    examiners['ada-2005'] = AdaExaminer(code, '2005')

  if 'ada-2012' in languages or 'ada' in languages:
    examiners['ada-2012'] = AdaExaminer(code, '2012')

  if 'assembly' in languages:
    examiners['assembly'] = AssemblyGenericExaminer(code, tab_size, format)

  if 'asm-360' in languages:
    examiners['asm-360'] = AssemblyIBMExaminer(code, tab_size, '360', format)

  if 'asm-370' in languages:
    examiners['asm-370'] = AssemblyIBMExaminer(code, tab_size, '370', format)

  if 'asm-390' in languages:
    examiners['asm-390'] = AssemblyIBMExaminer(code, tab_size, 'system-z', format)

  if 'asm-system-z' in languages:
    examiners['asm-system-z'] = AssemblyIBMExaminer(code, tab_size, 'system-z', format)

  if 'asm-1802' in languages:
    examiners['asm-1802'] = AssemblyExaminer(code, tab_size, '1802', format)

  if 'asm-6502' in languages:
    examiners['asm-6502'] = AssemblyExaminer(code, tab_size, '6502', format)

  if 'asm-6800' in languages:
    examiners['asm-6800'] = AssemblyExaminer(code, tab_size, '6800', format)

  if 'asm-68000' in languages:
    examiners['asm-68000'] = AssemblyExaminer(code, tab_size, '68000', format)

  if 'asm-8080' in languages:
    examiners['asm-8080'] = AssemblyExaminer(code, tab_size, '8080', format)

  if 'asm-z-80' in languages:
    examiners['asm-z-80'] = AssemblyExaminer(code, tab_size, 'z80', format)

  if 'asm-8086' in languages or 'asm-8088' in languages:
    examiners['asm-8086'] = AssemblyExaminer(code, tab_size, '8086', format)

  if 'asm-80286' in languages:
    examiners['asm-80286'] = AssemblyExaminer(code, tab_size, '80286', format)

  if 'asm-80386' in languages or 'asm-80388' in languages:
    examiners['asm-80386'] = AssemblyExaminer(code, tab_size, '80386', format)

  if 'asm-80486' in languages or 'asm-80488' in languages:
    examiners['asm-80486'] = AssemblyExaminer(code, tab_size, '80486', format)

  if 'asm-pdp-8' in languages:
    examiners['asm-pdp-8'] = AssemblyExaminer(code, tab_size, 'pdp-8', format)

  if 'asm-pdp-11' in languages:
    examiners['asm-pdp-11'] = AssemblyExaminer(code, tab_size, 'pdp-11', format)

  if 'awk' in languages:
    examiners['awk'] = AwkExaminer(code, '')

  if 'gawk' in languages:
    examiners['gawk'] = AwkExaminer(code, 'gnu')

  if 'basic' in languages or 'bas' in languages:
    examiners['basic'] = BasicExaminer(code, '')

  if 'basic-80' in languages or 'mbasic' in languages or 'mba' in languages:
    examiners['basic-80'] = BasicExaminer(code, 'basic-80')

  if 'basica' in languages:
    examiners['basica'] = BasicExaminer(code, 'basica')

  if 'basic-1965' in languages:
    examiners['basic-1965'] = BasicExaminer(code, 'basic-1965')

  if 'basic-1973' in languages:
    examiners['basic-1973'] = BasicExaminer(code, 'basic-1973')

  if 'basic-1978' in languages:
    examiners['basic-1978'] = BasicExaminer(code, 'basic-1978')

  if 'bbc-basic' in languages:
    examiners['bbc-basic'] = BasicBbcExaminer(code)

  if 'cbasic' in languages:
    examiners['cbasic'] = CBasicExaminer(code)

  if 'c-78' in languages or 'c' in languages or 'c-k&r' in languages:
    examiners['c-78'] = CExaminer(code, '78')

  if 'c-89' in languages or 'c' in languages:
    examiners['c-89'] = CExaminer(code, '89')

  if 'c-99' in languages or 'c' in languages:
    examiners['c-99'] = CExaminer(code, '99')

  if 'cplusplus' in languages or 'c++' in languages:
    examiners['cplusplus'] = CppExaminer(code)

  if 'csharp' in languages or 'cs' in languages or 'c#' in languages:
    examiners['csharp'] = CsharpExaminer(code)

  if 'cobol-68' in languages or 'cobol' in languages:
    examiners['cobol-68'] = CobolExaminer(code, '68', '', tab_size, format)

  if 'cobol-74' in languages or 'cobol' in languages:
    examiners['cobol-74'] = CobolExaminer(code, '74', '', tab_size, format)

  if 'cobol-85' in languages or 'cobol' in languages:
    examiners['cobol-85'] = CobolExaminer(code, '85', '', tab_size, format)

  if 'cobol-2002' in languages or 'cobol' in languages:
    examiners['cobol-2002'] = CobolExaminer(code, '2002', '', tab_size, format)

  if 'cobol-2014' in languages or 'cobol' in languages or 'cob' in languages:
    examiners['cobol-2014'] = CobolExaminer(code, '2014', '', tab_size, format)

  if 'cobol-2014-acu' in languages or 'cobol' in languages:
    examiners['cobol-2014-acu'] = CobolExaminer(code, '2014', 'acu', tab_size, format)

  if 'cobol-2014-ibm' in languages or 'cobol' in languages:
    examiners['cobol-2014-ibm'] = CobolExaminer(code, '2014', 'ibm', tab_size, format)

  if 'cobol-2014-gnu' in languages or 'cobol' in languages:
    examiners['cobol-2014-gnu'] = CobolExaminer(code, '2014', 'gnu', tab_size, format)

  if 'coffeescript' in languages or 'coffee' in languages:
    examiners['coffeescript'] = CoffeeScriptExaminer(code)

  if 'd' in languages:
    examiners['d'] = DExaminer(code, block_comment_limit)

  if 'dart' in languages:
    examiners['dart'] = DartExaminer(code)

  if 'dbase-ii' in languages or 'dbase' in languages or 'prg' in languages:
    examiners['dbase-ii'] = DbaseExaminer(code, 'ii', format)

  if 'dbase-iii' in languages or 'dbase' in languages or 'prg' in languages:
    examiners['dbase-iii'] = DbaseExaminer(code, 'iii', format)

  if 'delphi' in languages:
    examiners['delphi'] = DelphiExaminer(code)

  if 'dibol' in languages:
    examiners['dibol'] = DibolExaminer(code)

  if 'eiffel' in languages:
    examiners['eiffel'] = EiffelExaminer(code)

  if 'erlang' in languages:
    examiners['erlang'] = ErlangExaminer(code)

  if 'flowmatic' in languages:
    examiners['flowmatic'] = FlowmaticExaminer(code)

  if 'fortran-66' in languages or 'fortran' in languages or 'f66' in languages:
    examiners['fortran-66'] = FortranExaminer(code, '66', tab_size, format)

  if 'fortran-77' in languages or 'fortran' in languages or 'f77' in languages:
    examiners['fortran-77'] = FortranExaminer(code, '77', tab_size, format)

  if 'fortran-90' in languages or 'fortran' in languages or'f90' in languages:
    examiners['fortran-90'] = FortranExaminer(code, '90', tab_size, format)

  if 'fortran-95' in languages or 'fortran' in languages or 'f95' in languages:
    examiners['fortran-95'] = FortranExaminer(code, '95', tab_size, format)

  if 'fortran-2003' in languages or 'fortran' in languages or 'f03' in languages:
    examiners['fortran-2003'] = FortranExaminer(code, '2003', tab_size, format)

  if 'fortran-2008' in languages or 'fortran' in languages:
    examiners['fortran-2008'] = FortranExaminer(code, '2008', tab_size, format)

  if 'fsharp' in languages or 'fs' in languages or 'f#' in languages:
    examiners['fsharp'] = MlExaminer(code, 'fsharp')

  if 'go' in languages or 'golang' in languages:
    examiners['go'] = GoExaminer(code)

  if 'groovy' in languages:
    examiners['go'] = GroovyExaminer(code)

  if 'haskell' in languages:
    examiners['haskell'] = HaskellExaminer(code)

  if 'html' in languages:
    examiners['html'] = HTMLExaminer(code)

  if 'intercal' in languages:
    examiners['intercal'] = IntercalExaminer(code)

  if 'java' in languages:
    examiners['java'] = JavaExaminer(code)

  if 'javascript' in languages or 'js' in languages:
    examiners['javascript'] = JavaScriptExaminer(code)

  if 'julia' in languages:
    examiners['julia'] = JuliaExaminer(code, block_comment_limit)

  if 'kotlin' in languages:
    examiners['kotlin'] = KotlinExaminer(code)

  if 'latino' in languages:
    examiners['latino'] = LatinoExaminer(code)

  if 'lua' in languages:
    examiners['lua'] = LuaExaminer(code)

  if 'matlab' in languages or 'm' in languages:
    examiners['matlab'] = MatlabExaminer(code, 'matlab')

  if 'modula-2' in languages or 'mod' in languages:
    examiners['modula-2'] = Modula2Examiner(code)

  if 'objective-c' in languages:
    examiners['objective-c'] = ObjectiveCExaminer(code)

  if 'ocaml' in languages:
    examiners['ocaml'] = MlExaminer(code, 'ocaml')

  if 'octave' in languages:
    examiners['octave'] = MatlabExaminer(code, 'octave')

  if 'pascal' in languages or 'pas' in languages:
    examiners['pascal'] = PascalExaminer(code)

  if 'perl' in languages or 'pl' in languages or 'pm' in languages:
    examiners['perl'] = PerlExaminer(code)

  if 'pl1' in languages:
    examiners['pl1'] = PL1Examiner(code, tab_size, format)

  if 'plm' in languages:
    examiners['plm'] = PLMExaminer(code, tab_size, format)

  if 'prolog' in languages:
    examiners['prolog'] = PrologExaminer(code)

  if 'python' in languages or 'py' in languages:
    examiners['python'] = PythonExaminer(code)

  if 'r' in languages:
    examiners['r'] = RExaminer(code)

  if 'ruby' in languages or 'rb' in languages:
    examiners['ruby'] = RubyExaminer(code)

  if 'rust' in languages:
    examiners['rust'] = RustExaminer(code, block_comment_limit)

  if 'scala' in languages:
    examiners['scala'] = ScalaExaminer(code)

  if 'sql' in languages:
    examiners['sql'] = SqlExaminer(code, 'sql')

  if 't-sql' in languages:
    examiners['t-sql'] = SqlExaminer(code, 't-sql')

  if 'pl-sql' in languages:
    examiners['pl-sql'] = SqlExaminer(code, 'pl-sql')

  if 'swift' in languages:
    examiners['swift'] = SwiftExaminer(code)

  if 'typescript' in languages:
    examiners['typescript'] = TypeScriptExaminer(code)

  if 'visualbasic-6' in languages or 'visualbasic' in languages or 'vb' in languages:
    examiners['visualbasic-6'] = VisualBasic6Examiner(code)

  if 'visualbasic-net' in languages or 'visualbasic' in languages or 'vb' in languages:
    examiners['visualbasic-net'] = VisualBasicNETExaminer(code)

  return examiners


def identify_language(code, format, tab_size, comment, block_comment_limit, tiebreak_keywords, tiebreak_tokens, tiebreak_simple, languages):
  tiebreak_override = True
  examiners = make_multiple_examiners(code, format, tab_size, comment, block_comment_limit, languages)

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
        group = ['keyword', 'type', 'common function', 'register']
        keyword_count = examiners[name].count_my_tokens(group)
        if keyword_count > highest_keyword_count:
          highest_keyword_count = keyword_count

      if highest_keyword_count > 0:
        # assign confidence to number of keywords and types (more is better)
        for name in high_names:
          count = examiners[name].count_my_tokens(group)
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

  if tiebreak_simple:
    # count how many have the greatest confidence
    high_names = []
    for name in examiners:
      confidence = examiners[name].confidence()
      if confidence == highest_confidence:
        high_names.append(name)

    # if a tie among multiple examiners
    if len(high_names) > 1:
      for name in high_names:
        b = name
        a = simpler_languages[name]
        while a is not None:
          if a in high_names:
            # when there is a simpler language in the high names list
            # decrease confidence for this language
            examiners[b].confidences['simplest'] = 0.99
            b = a
          a = simpler_languages[a]

      # recalculate confidence with new factor
      for name in high_names:
        confidence = examiners[name].confidence()
        retval[name] = confidence

  if tiebreak_override:
    # count how many have the greatest confidence
    high_names = []
    for name in examiners:
      confidence = examiners[name].confidence()
      if confidence == highest_confidence:
        high_names.append(name)

    # if a tie among multiple examiners
    if len(high_names) > 1:
      for name in high_names:
        if name in override_language:
          loser = override_language[name]
          if loser in high_names:
            # decrease confidence for loser language
            examiners[loser].confidences['overridden'] = 0.99

      # recalculate confidence with new factor
      for name in high_names:
        confidence = examiners[name].confidence()
        retval[name] = confidence

  return retval, examiners


def unwrap_lines(text, language):
  # only the fixed-format versions can be unwrapped
  format = 'fixed'
  cobol_names = ['cobol', 'cobol-68', 'cobol-74', 'cobol-85']
  fortran_names = ['fortran', 'fortran-66', 'fortran-77']
  pl1_names = ['pl1']

  unwrapped_text = text

  if language in fortran_names:
    examiner = FortranExaminer(text, '77', 8, False, format)
    lines = split_lines(text)
    unwrapped_text = examiner.unwrapped_code(lines)

  if language in cobol_names:
    examiner = CobolExaminer(text, '85', '', 8, False, format)
    lines = split_lines(text)
    unwrapped_text = examiner.unwrapped_code(lines)

  if language in pl1_names:
    examiner = PL1Examiner(text, 8, False, format)
    lines = split_lines(text)
    unwrapped_text = examiner.unwrapped_code(lines)

  if language in ['cbasic']:
    examiner = CBasicExaminer(text)
    unwrapped_text = examiner.unwrapped_code()

  if language in ['c']:
    examiner = CExaminer(text, '78')
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


def tokenize(code, format, language, tab_size, comment, block_comment_limit):
  retval = []

  examiner = make_one_examiner(language, code, format, tab_size, comment, block_comment_limit)

  if examiner is not None:
    retval = examiner.tokens

  return retval


def tokenize_confidence(code, format, language, tab_size, get_errors, comment, block_comment_limit):
  retval = []

  examiner = make_one_examiner(language, code, format, tab_size, comment, block_comment_limit)

  if examiner is not None:
    if get_errors:
      retval = examiner.errors
    else:
      retval = examiner.confidences

  return retval


def tokenize_statistics(code, format, language, tab_size, comment, block_comment_limit):
  retval = []

  examiner = make_one_examiner(language, code, format, tab_size, comment, block_comment_limit)

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
    comment = ''
    block_comment_limit = 2048
    # tokenize(code, format, language, tab_size, comment, block_comment_limit)
    cProfile.run("tokenize(code, format, language, tab_size, comment, block_comment_limit)")
  else:
    if args.confidence is not None:
      print('Confidencing file ' + args.confidence)
      in_file = open(args.confidence, 'r')
      code = in_file.read()
      in_file.close()
      language = 'ruby'
      tab_size = 4
      comment = ''
      block_comment_limit = 2048
      cProfile.run("tokenize_confidence(code, format, language, False, tab_size, comment, block_comment_limit)")
    else:
      if args.detect is not None:
        print('Detecting file ' + args.detect)
        in_file = open(args.detect, 'r')
        code = in_file.read()
        in_file.close()
        tab_size = 4
        tiebreak_keywords = True
        tiebreak_tokens = False
        tiebreak_simple = False
        comment = ''
        block_comment_limit = 2048
        languages = list(codes_and_names.keys())
        format = 'better'
        cProfile.run('identify_language(code, format, tab_size, comment, block_comment_limit, tiebreak_keywords, tiebreak_tokens, tiebreak_simple, languages)')
      else:
        app.run()
