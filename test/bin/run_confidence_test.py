import argparse
import requests
import json
import sys

# build set of confidence factors, one group for each language
def make_confidences(code, params, languages):
  confidences = {}
  target = 'localhost:5000'

  for language in languages:
    # send request, get response
    params2 = params.copy()
    params2.append('language=' + language)
    paramstext = '&'.join(params2)
    url = "http://" + target + "/" + "confidence" + "?" + paramstext
    resp = requests.post(url, data=code)
    content = resp.content
    if len(content) > 0:
      confidence = json.loads(content)
      confidences[language] = confidence

  return confidences


# build set of confidence errors, one group for each language
def make_confidence_errors(code, params, languages):
  confidence_errors = {}
  target = 'localhost:5000'

  for language in languages:
    # send request, get response
    params2 = params.copy()
    params2.append('errors')
    params2.append('language=' + language)
    paramstext = '&'.join(params2)
    url = "http://" + target + "/" + "confidence" + "?" + paramstext
    resp = requests.post(url, data=code)
    content = resp.content
    if len(content) > 0:
      confidence = json.loads(content)
      confidence_errors[language] = confidence

  return confidence_errors


# compute confidence (product of all elements)
def calc_confidence(confidence):
  value = 1.0

  for name in confidence:
    value *= confidence[name]

  return value


# count tokens in statistics
def count_tokens(statistic, groups):
  count = 0

  for group in statistic:
    if group in groups:
      count += statistic[group]

  return count


# parse command line
parser = argparse.ArgumentParser()
parser.add_argument('-i', '--input')
parser.add_argument('--tabsize')
parser.add_argument('--wide', action='store_true')
parser.add_argument('--comment')
parser.add_argument('--block-comment-limit')
parser.add_argument('--languages')
args = parser.parse_args()

# read code (input)
filename = args.input.strip()
try:
  with open(filename, 'rb') as f:
    contents = f.read()
except UnicodeDecodeError:
  print('"Cannot read text"')
  sys.exit()

try:
  code = contents.decode('ASCII')
  sys.stderr.write('ENCODE ASCII\n')
except UnicodeDecodeError:
#  try:
#    code = contents.decode('Latin-1')
#    sys.stderr.write('ENCODE LATIN-1\n')
#  except UnicodeDecodeError:
    try:
      code = contents.decode('UTF-8')
      sys.stderr.write('ENCODE UTF-8\n')
    except UnicodeDecodeError:
      try:
        code = contents.decode('UTF-16')
        sys.stderr.write('ENCODE UTF-16\n')
      except UnicodeDecodeError:
        print('"Cannot encode text as ASCII, UTF-8, or UTF-16"')
        sys.exit()

sys.stderr.write(code)
