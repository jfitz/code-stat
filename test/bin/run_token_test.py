import argparse
import requests
import json
import sys

# build set of confidence factors, one group for each language
def make_tokens(code, params, languages):
  confidences = {}
  target = 'localhost:5000'

  for language in languages:
    # send request, get response
    params2 = params.copy()
    params2.append('language=' + language)
    paramstext = '&'.join(params2)
    url = "http://" + target + "/" + "tokens" + "?" + paramstext
    resp = requests.post(url, data=code)
    content = resp.content
    if len(content) > 0:
      confidence = json.loads(content)
      confidences[language] = confidence

  return confidences


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
with open(filename, 'rb') as f:
  contents = f.read()

params = []

tabsize = args.tabsize
if tabsize:
  tabsize = int(tabsize)
  params.append('tabsize=' + str(tabsize))

wide = args.wide
if wide:
  params.append('wide')

tiebreak_keywords = True
tiebreak_tokens = False
tiebreak_simple = True

comment = args.comment
if comment:
  params.append('comment=' + comment)

block_comment_limit = args.block_comment_limit
if block_comment_limit:
  block_comment_limit = int(block_comment_limit)
  params.append('block_comment_limit=' + str(block_comment_limit))

target = 'localhost:5000'

url = "http://" + target + "/" + "languages"
# request languages
resp = requests.get(url)
content = resp.content
codes_and_names = json.loads(content)

# get list of languages
languages = args.languages
if languages is not None:
  languages = languages.split(',')
else:
  # if languages not specified, get list from web service
  languages = codes_and_names.keys()

# get set of confidence factors
tokens = make_tokens(contents, params, languages)

# print result
mydict = {}
for key in tokens:
  new_key = codes_and_names[key]
  mydict[new_key] = tokens[key]
  print('*** ' + new_key)
  print(mydict[new_key])

json_text = json.dumps(mydict)
sys.stdout.write(json_text)
