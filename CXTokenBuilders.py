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


# token reader for /* */ comment that allows nesting
class NestedSlashStarCommentTokenBuilder(TokenBuilder):
  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment')]


  def accept(self, candidate, c):
    result = False

    if len(candidate) == 0 and c == '/':
      result = True

    if len(candidate) == 1 and c == '*':
      result = True

    if len(candidate) == 2:
      result = True

    # walk through candidate and verify open delimiters
    if len(candidate) > 2:
      level = 0
      prev = ''
      for c in candidate:
        if prev == '/' and c == '*':
          level += 1

        if prev == '*' and c == '/':
          level -= 1

        if level < 0:
          result = False

        prev = c

      if level > 0:
        result = True

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if not self.text.startswith('/*'):
      return 0

    if not self.text.endswith('*/'):
      return 0

    # walk through text and verify matched delimiters
    level = 0
    prev = ''
    for c in self.text:
      if prev == '/' and c == '*':
        level += 1

      if prev == '*' and c == '/':
        level -= 1

      if level < 0:
        return 0

      prev = c

    if level != 0:
      return 0

    return len(self.text)


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
      result = c.isalpha() or c.isdigit() or c in "/\\ ,_.>'"

    return result


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if self.text[0] == '<' and self.text[-1] == '>':
      return len(self.text)
    
    return 0
