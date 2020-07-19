from codestat_token import Token

class Tokenizer():
  def __init__(self, tokenbuilders):
    self.tokenbuilders = tokenbuilders


  def tokenize(self, text):
    tokens = []

    line_printable_tokens = []

    while len(text) > 0:
      new_tokens = None

      tokenbuilder = self.try_tokenbuilders(text, line_printable_tokens)

      if tokenbuilder is not None:
        new_tokens = tokenbuilder.get_tokens()

      if new_tokens == None:
        raise Exception("Cannot tokenize '" + text + "'")

      for token in new_tokens:
        if token.group not in ['whitespace', 'comment']:
          line_printable_tokens.append(token)

        if token.group == 'newline':
          line_printable_tokens = []

      tokens += new_tokens
      count = tokenbuilder.get_count()
      text = text[count:]
    
    return tokens


  def tokenize_and_stop(self, text, stop_tokens):
    tokens = []

    line_printable_tokens = []

    go = True
    while len(text) > 0 and go:
      new_tokens = None

      tokenbuilder = self.try_tokenbuilders(text, line_printable_tokens)

      if tokenbuilder is not None:
        new_tokens = tokenbuilder.get_tokens()

      if new_tokens == None:
        raise Exception("Cannot tokenize '" + text + "'")

      for token in new_tokens:
        if token.group not in ['whitespace', 'comment']:
          line_printable_tokens.append(token)

        if token.group == 'newline':
          line_printable_tokens = []

      tokens += new_tokens
      count = tokenbuilder.get_count()
      text = text[count:]

      for token in new_tokens:
        if token.text in stop_tokens:
          go = False

    return tokens, text


  def try_tokenbuilders(self, text, line_printable_tokens):
    # try all tokenbuilders
    for tokenbuilder in self.tokenbuilders:
      tokenbuilder.attempt(text)

    # see which tokenbuilder found the highest score
    winner = None
    winner_score = 0
    for tokenbuilder in self.tokenbuilders:
      score = tokenbuilder.get_score(line_printable_tokens)

      if score > winner_score:
        winner = tokenbuilder
        winner_score = score

    return winner


  @staticmethod
  def tabs_to_spaces(text, tab_size):
    if tab_size is None:
      tab_size = 8

    column = 0
    detabbed_text = ''

    for c in text:
      if c == '\n':
        detabbed_text += c
        column = 0
      elif c == '\r':
        detabbed_text += c
      elif c == '\t':
        next_tab_stop = int((column + tab_size) / tab_size) * tab_size
        while column < next_tab_stop:
          detabbed_text += ' '
          column += 1
      else:
        detabbed_text += c
        column += 1
    
    return detabbed_text


  # check token matches rules for label
  @staticmethod
  def check_label_format(label, label_leads, label_mids, label_ends):
    l0 = label[0]
    if not (l0.isalpha() or l0 in label_leads):
      return False

    llast = label[-1]
    if not (llast.isalnum() or llast in label_mids or llast in label_ends):
      return False

    lmid = label[1:-1]
    for lc in lmid:
      if not (lc.isalnum() or lc in label_mids):
        return False
  
    return True


  # check token matches rules for opcode
  @staticmethod
  def check_opcode_format(opcode, extras):
    for oc in opcode:
      if not (oc.isalnum() or oc in extras):
        return False

    return True
  
  
  # combine numbers followed by identfiers to identifiers
  @staticmethod
  def combine_number_and_adjacent_identifier(tokens):
    new_list = []

    new_token = None
  
    for token in tokens:
      if token.group == 'identifier' and \
        new_token is not None and new_token.group == 'number':
        new_token = Token(new_token.text + token.text, 'identifier', True)
      else:
        if new_token is not None:
          new_list.append(new_token)
        new_token = token

    if new_token is not None:
      new_list.append(new_token)

    return new_list
