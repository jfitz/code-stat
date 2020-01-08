from codestat_token import Token
from token_builders import TokenBuilder


# token reader for identifier
class CobolIdentifierTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier')]


  def accept(self, candidate, c):
    result = c.isalpha() or c.isdigit()

    if len(candidate) > 0 and c == '-':
      result = True

    return result


# token reader for PIC descriptor
class PictureTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    tokens = None
    if self.text[-1] == '.':
      # a terminating dot is not part of the PIC
      token1 = Token(self.text[:-1], 'picture')
      token2 = Token('.', 'statement terminator')
      tokens = [token1, token2]
    else:
      tokens = [Token(self.text, 'picture')]

    return tokens


  def accept(self, candidate, c):
    result = False
    num_lparens = candidate.count('(')
    num_rparens = candidate.count(')')

    if num_rparens == num_lparens:
      if len(candidate) == 0:
        result = c in ['S', '$', '9', 'A', 'N', 'X', 'Z', 'V']

      if len(candidate) > 0:
        result = c in ['$', '9', 'A', 'N', 'X', 'Z', 'V', ',', '.', '-', '(']

    if num_rparens == num_lparens - 1:
      result = c.isdigit() or c == ')'

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    num_lparens = self.text.count('(')
    num_rparens = self.text.count(')')

    if num_lparens != num_rparens:
      return 0

    return len(self.text)


# token reader for PIC descriptor
class CRPictureTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    tokens = None
    if self.text[-1] == '.':
      # a terminating dot is not part of the PIC
      token1 = Token(self.text[:-1], 'picture')
      token2 = Token('.', 'statement terminator')
      tokens = [token1, token2]
    else:
      tokens = [Token(self.text, 'picture')]

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
    if self.text is None:
      return 0

    if self.text[-1] != 'R':
      return 0

    num_lparens = self.text.count('(')
    num_rparens = self.text.count(')')

    if num_lparens != num_rparens:
      return 0

    return len(self.text)


# token reader for >> directive
class CobolPreprocessorTokenBuilder(TokenBuilder):
  def __init__(self):
    self.prefix = '>>'
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    if self.text.startswith(self.prefix):
      return [Token(self.text, 'preprocessor')]

    return None


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c == self.prefix[0]

    if len(candidate) == 1:
      result = c == self.prefix[1]

    if candidate.startswith(self.prefix):
      result = True

    if c in ['\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text.startswith(self.prefix):
      return len(self.text)
    
    return 0


# token reader for asterisk comment
class AsteriskCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c == '*'

    if len(candidate) > 0:
      result = True

    if c in ['\r', '\n']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(line_printable_tokens) > 0:
      return 0

    return len(self.text)
