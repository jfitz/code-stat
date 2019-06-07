from Token import Token
from TokenBuilders import TokenBuilder

# token reader for // comment
class SlashSlashCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment')]


  def accept(self, candidate, c):
    result = False

    if candidate.startswith('//'):
      result = True

    if c == '/' and candidate == '/':
      result = True

    if c == '/' and candidate == '':
      result = True

    if c in ['\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text.startswith('//'):
      return len(self.text)
    
    return 0


# token reader for /// comment
class TripleSlashCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment')]


  def accept(self, candidate, c):
    result = False

    if candidate == '':
      result = c == '/'

    if candidate == '/':
      result = c == '/'

    if candidate == '//':
      result = c == '/'

    if candidate.startswith('///'):
      result = True

    if c in ['\n', '\r']:
      result = False

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text.startswith('///'):
      return len(self.text)
    
    return 0


# token reader for /* */ comment
class SlashStarCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment')]


  def accept(self, candidate, c):
    result = False

    if c == '/' and len(candidate) == 0:
      result = True

    if c == '*' and len(candidate) == 1:
      result = True

    if c == '/' and len(candidate) > 2 and candidate[-1] == '*':
      result = True

    if candidate.startswith('/*') and (candidate[-2] != '*' or candidate[-1] != '/'):
      result = True

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text.startswith('/*') and self.text.endswith('*/'):
      return len(self.text)
    
    return 0


# token reader for preprocessor directives
class CPreProcessorTokenBuilder(TokenBuilder):
  def __init__(self, prefixes, continuation_chars):
    self.text = ''
    self.prefixes = prefixes
    self.continuation_chars = continuation_chars


  def get_tokens(self):
    if self.text is None:
      return None

    if self.text.startswith('#'):
      return [Token(self.text, 'preprocessor')]

    return None


  def accept(self, candidate, c):
    result = False

    newline_chars = ['\n', '\r']

    if len(candidate) == 0:
      result = c == '#'

    if len(candidate) > 0:
      result = True

      if c in newline_chars:
        result = False

        if candidate[-1] in self.continuation_chars:
          result = True

        if len(candidate) > 1 and candidate[-2] in self.continuation_chars:
          result = candidate[-1] in newline_chars and c != candidate[-1]

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text.startswith(self.prefixes):
      return len(self.text)
    
    return 0


# token reader for <name> class identifier
class ClassTypeTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'type')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0:
      result = c == '<'

    if len(candidate) > 0 and candidate[-1] != '>':
      result = c != ' '

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text[0] == '<' and self.text[-1] == '>':
      return len(self.text)
    
    return 0
