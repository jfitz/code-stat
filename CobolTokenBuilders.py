from Token import Token
from TokenBuilders import TokenBuilder


# token reader for identifier
class CobolIdentifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = None


  def get_tokens(self):
    if self.token is None:
      return None

    return [Token(self.token, 'identifier')]


  def accept(self, candidate, c):
    result = False

    if c.isalpha():
      result = True

    if len(candidate) > 0 and c.isdigit():
      result = True

    if len(candidate) > 0 and c == '-':
      result = True

    return result


# token reader for PIC descriptor
class PictureTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''


  def get_tokens(self):
    if self.token is None:
      return None

    tokens = None
    if self.token[-1] == '.':
      token1 = Token(self.token[:-1], 'picture')
      token2 = Token('.', 'operator')
      tokens = [token1, token2]
    else:
      tokens = [Token(self.token, 'picture')]

    return tokens


  def accept(self, candidate, c):
    result = False
    num_lparens = candidate.count('(')
    num_rparens = candidate.count(')')

    if candidate == '' and c == 'S':
      result = True

    if num_rparens == num_lparens:
      if c in ['9', 'X', 'Z', 'V', ',', '.'] and len(candidate) > 0:
        result = True

      if c in ['9', 'X', 'Z', 'V'] and candidate =='':
        result = True

      if c == '(' and len(candidate) > 0:
        result = True

    if num_rparens == num_lparens - 1:
      if c.isdigit():
        result = True

      if c == ')':
        result = True

    return result

  def get_score(self, last_printable_token):
    if self.token is None:
      return 0

    boost = 0

    if last_printable_token.group == 'keyword' and \
    last_printable_token.text in (name.upper() for name in ['PIC', 'PICTURE']):
      boost += 0.5

    return len(self.token) + boost


# token reader for *> comment
class StarCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''


  def get_tokens(self):
    if self.token is None:
      return None

    if self.token.startswith('*>'):
      return [Token(self.token, 'comment')]

    return None


  def accept(self, candidate, c):
    result = False

    if candidate.startswith('*>'):
      result = True

    if c == '>' and candidate == '*':
      result = True

    if c == '*' and candidate == '':
      result = True

    if c == '\n':
      result = False

    return result


# token reader for *> comment
class CobolPreprocessorTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''


  def get_tokens(self):
    if self.token is None:
      return None

    if self.token.startswith('>>'):
      return [Token(self.token, 'preprocessor')]

    return None


  def accept(self, candidate, c):
    result = False

    if candidate.startswith('>>'):
      result = True

    if c == '>' and candidate == '>':
      result = True

    if c == '>' and candidate == '':
      result = True

    if c == '\n':
      result = False

    return result
