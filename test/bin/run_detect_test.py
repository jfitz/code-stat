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


# identify language with highest confidence
# break ties
def identify_language(code, params, tiebreak_keywords, tiebreak_tokens, tiebreak_simple, languages):
  confidences = make_confidences(code, params, languages)

  # get confidence values
  retval = {}
  highest_confidence = 0
  for language in confidences:
    confidence = calc_confidence(confidences[language])
    retval[language] = confidence
    # find the highest value
    if confidence > highest_confidence:
      highest_confidence = confidence

  if tiebreak_keywords:
    # count how many have the greatest confidence
    high_languages = []
    for language in confidences:
      confidence = calc_confidence(confidences[language])
      if confidence == highest_confidence:
        high_languages.append(language)

    # if a tie among multiple examiners
    if len(high_languages) > 1:
      statistics = {}
      highest_keyword_count = 0

      for language in high_languages:
        # todo: get statistics, save in dictionary
        params2 = params.copy()
        params2.append('language=' + language)
        paramstext = '&'.join(params2)
        url = "http://" + target + "/" + "statistics" + "?" + paramstext
        resp = requests.post(url, data=code)
        content = resp.content
        statistic = json.loads(content)
        statistics[language] = statistic

        groups = ['keyword', 'type', 'function']
        keyword_count = count_tokens(statistic, groups)
        if keyword_count > highest_keyword_count:
          highest_keyword_count = keyword_count

      if highest_keyword_count > 0:
        # assign confidence to number of keywords and types (more is better)
        for language in high_languages:
          count = count_tokens(statistics[language], groups)
          keyword_count_confidence = count / highest_keyword_count
          confidences[language]['keyword_count'] = keyword_count_confidence

        # recalculate confidence with new factor
        for language in high_languages:
          confidence = calc_confidence(confidences[language])
          retval[language] = confidence

  if tiebreak_simple:
    # count how many have the greatest confidence
    high_languages = []
    for language in confidences:
      confidence = calc_confidence(confidences[language])
      if confidence == highest_confidence:
        high_languages.append(language)

    # if a tie among multiple examiners
    if len(high_languages) > 1:
      url = "http://" + target + "/" + "simple"
      # request languages
      resp = requests.get(url)
      content = resp.content
      simpler_languages = json.loads(content)

      for language in high_languages:
        b = language
        a = simpler_languages[language]
        while a is not None:
          if a in high_languages:
            # when there is a simpler language in the high names list
            # decrease confidence for this language
            confidences[b]['simplest'] = 0.99
            b = a
          a = simpler_languages[a]

      # recalculate confidence with new factor
      for language in high_languages:
        confidence = calc_confidence(confidences[language])
        retval[language] = confidence

  return retval, confidences

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
  with open(filename) as f:
    contents = f.read()
except UnicodeDecodeError:
  print('"Cannot decode text as ASCII, UTF-8, or UTF-16"')
  sys.exit()

try:
  code = contents.encode('ASCII')
except UnicodeEncodeError:
  try:
    code = contents.encode('UTF-8')
  except UnicodeEncodeError:
    try:
      code = contents.encode('UTF-16')
    except UnicodeEncodeError:
      print('"Cannot encode text as ASCII, UTF-8, or UTF-16"')
      sys.exit()

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
detected_languages, __ = identify_language(code, params, tiebreak_keywords, tiebreak_tokens, tiebreak_simple, languages)

# print result
mydict = {}
for key in detected_languages:
  new_key = codes_and_names[key]
  mydict[new_key] = detected_languages[key]

json_text = json.dumps(mydict)
sys.stdout.write(json_text)
