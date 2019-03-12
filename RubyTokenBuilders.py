from Token import Token
from TokenBuilders import TokenBuilder

# token reader for identifier
class RubyIdentifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = None

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier')]

  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c.isalpha() or c == '_' or c == '@'

    if len(candidate) == 1:
      if candidate == '@':
        result = c.isalpha() or c == '_' or c == '@'
      else:
        result = c.isalpha() or c.isdigit() or c == '_' or c == '?'

    if len(candidate) > 1:
      result = c.isalpha() or c.isdigit() or c == '_' or c == '?'

    if len(candidate) > 1 and candidate[-1] == '?':
      result = False

    return result


# token reader for identifier
class RubyParamGrouperTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = None

  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'group')]

  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c == '|'

    return result


  def token_is_do(self, token):
    return (token.group == 'keyword' and token.text == 'do') or\
      (token.group == 'group' and token.text == '{')


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    boost = 0.0

    if len(line_printable_tokens) > 0 and self.token_is_do(line_printable_tokens[-1]):
      # simple case: previous token is keyword:'do' or group:'{'
      boost = 0.5

    else:
      # complex case: previous token is identifier,
      #  all tokens to prior 'do' or '{' are identifier or group
      #  exactly one group:'|' in prior
      param_bar_count = 0

      for token in reversed(line_printable_tokens):
        if self.token_is_do(token):
          break

        if token.group not in ['identifier', 'group']:
          return 0

        if token.group == 'group' and token.text == '|':
          param_bar_count += 1

      if param_bar_count == 1:
        boost = 0.5

    return len(self.text) + boost


# token reader for identifier
class HereDocTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = None
    self.oper = '<<-'

  def get_tokens(self):
    if self.text is None:
      return None

    # split the text into operator, marker, content, and marker tokens
    lines = self.text.split('\n')
    oper = lines[0][:3]
    marker = lines[-1]
    content = Token('\n'.join(lines[1:-1]), 'here doc')
    op_token = Token(oper, 'operator')
    mark_token = Token(marker, 'doc marker')
    newline_token = Token('\n', 'newline')

    # the marker token is used twice - once at beginning and once at end
    return [
      op_token,
      mark_token,
      newline_token,
      content,
      newline_token,
      mark_token
    ]

  def accept(self, candidate, c):
    result = False
 
    if len(candidate) < len(self.oper):
      result = self.oper.startswith(candidate)
    else:
      if candidate.startswith(self.oper):
        result = True

        # if the last line begins with the marker from the first line
        # stop accepting characters
        lines = candidate.split('\n')
        if len(lines) > 1:
          first_line = lines[0]
          last_line = lines[-1]
          marker = first_line[len(self.oper):].rstrip()
          if last_line.startswith(marker):
            result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    lines = self.text.split('\n')

    if len(lines) < 2:
      return 0

    line0 = lines[0].rstrip()
    if len(line0) < 4:
      return 0

    marker = lines[0][3:].rstrip()

    last_line = lines[-1].rstrip()

    if last_line != marker:
      return 0
    
    return len(self.text)


# accept characters to match item in list
class RubyFollowTokenBuilder(TokenBuilder):
  def __init__(self, legals, group, case_sensitive, follow_group, follow_text):
    self.legals = legals
    self.group = group
    self.case_sensitive = case_sensitive
    self.follow_group = follow_group
    self.follow_text = follow_text
    self.text = ''


  def attempt(self, text):
    self.text = None
    best_candidate = ''
    candidate = ''
    i = 0
    accepted = True

    while i < len(text) and accepted:
      c = text[i]
      accepted = self.accept(candidate, c)

      if accepted:
        candidate += c
        i += 1
        if self.case_sensitive:
          if candidate in self.legals:
            best_candidate = candidate
        else:
          if candidate.lower() in (legal.lower() for legal in self.legals):
            best_candidate = candidate

    if len(best_candidate) > 0:
      self.text = best_candidate


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, self.group)]


  def accept(self, candidate, c):
    token = candidate + c
    count = len(token)
    result = False

    if self.case_sensitive:
      for legal in self.legals:
        if legal[:count] == token:
          result = True
          break
    else:
      for legal in self.legals:
        if legal[:count].lower() == token.lower():
          result = True
          break

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    boost = 0.0

    if len(line_printable_tokens) > 0 and\
       line_printable_tokens[-1].group == self.follow_group and\
       line_printable_tokens[-1].text == self.follow_text:
      boost = 0.5

    return len(self.text) + boost
