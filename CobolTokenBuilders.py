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
    result = c.isalpha() or c.isdigit()

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
      # a terminating dot is not part of the PIC
      token1 = Token(self.token[:-1], 'picture')
      token2 = Token('.', 'statement terminator')
      tokens = [token1, token2]
    else:
      tokens = [Token(self.token, 'picture')]

    return tokens


  def accept(self, candidate, c):
    result = False
    num_lparens = candidate.count('(')
    num_rparens = candidate.count(')')

    if num_rparens == num_lparens:
      if len(candidate) == 0:
        result = c in ['S', '$', '9', 'X', 'Z', 'V']

      if len(candidate) > 0:
        result = c in ['$', '9', 'X', 'Z', 'V', ',', '.', '-', '(']

    if num_rparens == num_lparens - 1:
      result = c.isdigit() or c == ')'

    return result

  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    num_lparens = self.token.count('(')
    num_rparens = self.token.count(')')

    if num_lparens != num_rparens:
      return 0

    boost = 0

    if len(line_printable_tokens) > 0 and \
     line_printable_tokens[-1].group == 'keyword' and \
     line_printable_tokens[-1].text in (name.upper() for name in ['PIC', 'PICTURE']):
      boost += 0.5

    return len(self.token) + boost


# token reader for PIC descriptor
class CRPictureTokenBuilder(TokenBuilder):
  def __init__(self):
    self.token = ''


  def get_tokens(self):
    if self.token is None:
      return None

    tokens = None
    if self.token[-1] == '.':
      # a terminating dot is not part of the PIC
      token1 = Token(self.token[:-1], 'picture')
      token2 = Token('.', 'statement terminator')
      tokens = [token1, token2]
    else:
      tokens = [Token(self.token, 'picture')]

    return tokens


  def accept(self, candidate, c):
    result = False
    num_lparens = candidate.count('(')
    num_rparens = candidate.count(')')

    if num_rparens == num_lparens:
      if len(candidate) == 0:
        result = c in ['S', '$', '9', 'X', 'Z', 'V']

      if len(candidate) > 0:
        if candidate[-1] == 'C':
          result = c == 'R'
        else:
          if candidate[-1] != 'R':
            result = c in ['$', '9', 'X', 'Z', 'V', ',', '.', '-', '(', 'C']

    if num_rparens == num_lparens - 1:
      result = c.isdigit() or c == ')'

    return result

  def get_score(self, line_printable_tokens):
    if self.token is None:
      return 0

    if self.token[-1] != 'R':
      return 0

    num_lparens = self.token.count('(')
    num_rparens = self.token.count(')')

    if num_lparens != num_rparens:
      return 0

    boost = 0

    if len(line_printable_tokens) > 0 and \
     line_printable_tokens[-1].group == 'keyword' and \
     line_printable_tokens[-1].text in (name.upper() for name in ['PIC', 'PICTURE']):
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

    if c in ['\n', '\r']:
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

    if c in ['\n', '\r']:
      result = False

    return result
