from string import punctuation
from codestat_token import Token
from token_builders import TokenBuilder

# token reader for comment
class AssemblyCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, legals):
    self.legals = legals
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment', False)]


  def accept(self, candidate, c):
    if c in ['\n', '\r']:
      return False

    if candidate == '':
      return c in self.legals

    return True


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(line_printable_tokens) > 0:
      return 0

    if self.text[0] in self.legals:
      return len(self.text)
    
    return 0


# token reader for identifier
class LabelTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, lead_extras, extras, suffixes):
    self.lead_extras = lead_extras
    self.suffixes = suffixes
    self.extras = extras
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'label', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isalpha() or c in self.lead_extras

    if len(candidate) > 1 and candidate[-1] in self.suffixes:
      return False

    return c.isalpha() or c.isdigit() or c in self.extras or c in self.suffixes


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(line_printable_tokens) > 0:
      return 0

    if self.text[-1] in self.suffixes:
      return len(self.text)
    
    return 0


# token reader for prefixed text literal (string)
class MultilineCommentTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.prefix = 'comment'
    self.text = ''


  def attempt(self, text, start):
    self.text = None

    len_text = len(text)

    n1 = len(self.prefix)
    n2 = n1 + start
    t3 = text[start:n2].lower()
    if t3 != self.prefix:
      return

    # end of prefix
    index = start + len(self.prefix)

    if index >= len_text:
      return

    # at least one space
    n_spaces = 0
    while index < len_text and text[index] in [' ', '\t']:
      index += 1
      n_spaces += 1

    # if no spaces, return
    if n_spaces == 0:
      return

    # at least one nonspace
    delimiter = ''
    while index < len_text and not text[index].isspace():
      delimiter += text[index]
      index += 1

    # if newline no nonspace, return
    if len(delimiter) == 0:
      return

    # find delimiter text after index
    e = text.find(delimiter, index)

    # if not found, return
    if e == -1:
      return

    # extract all text as comment token
    end = e + len(delimiter)

    self.text = text[start:end]


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'comment', True)]


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    return len(self.text)


# token reader for identifier
class IbmAsmIdentifierTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, lead_extras, extras):
    self.lead_extras = lead_extras
    self.extras = extras
    self.text = None


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'identifier', True)]


  def accept(self, candidate, c):
    if len(candidate) == 0:
      return c.isalpha() or c in self.lead_extras

    return c.isalpha() or c.isdigit() or c in self.extras


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    # if at least one alpha character
    for i in self.text:
        if i.isalpha():
            return len(self.text)

    return 0


# token reader for comment
class HashQuoteCharTokenBuilder(TokenBuilder):
  @staticmethod
  def __escape_z__():
    Token.__escape_z__()
    return 'Escape ?Z'


  def __init__(self):
    self.text = ''


  def get_tokens(self):
    if self.text is None:
      return None

    return [Token(self.text, 'value', True)]


  def accept(self, candidate, c):
    if candidate == '':
      return c == '#'

    if candidate == '#':
      return c in punctuation

    return len(candidate) == 2


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) != 3:
      return 0
      
    return len(self.text)
