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


  @staticmethod
  def tokenize_asm_code(code, tab_size, tokenizer):
    lines = code.split('\n')

    tokens = []
    indents = []

    for line in lines:
      newline = '\n'
      if len(line) > 0 and line[-1] == '\r':
        newline = '\r\n'
      line = line.rstrip('\r')
      line = line.rstrip()
      line = Tokenizer.tabs_to_spaces(line, tab_size)

      # get tokens and indents
      line_tokens, line_indents = Tokenizer.tokenize_asm_line(line, tokenizer)
      tokens += line_tokens
      indents.append(line_indents)

      tokens.append(Token(newline, 'newline', False))

    # return tokens and indents
    return tokens, indents


  @staticmethod
  def tokenize_asm_line(line, tokenizer):
    # break apart the line based on format
    # The line format is:
    # label
    # opcode
    # arguments
    # comment

    label = ''
    label_indent = None
    lo_space = ''
    opcode = ''
    opcode_indent = None
    oa_space = ''
    args = ''
    args_indent = None
    ac_space = ''
    comment = ''
    comment_indent = None
    line_comment = ''
    line_comment_indent = None
    quotes = ['"', "'", '`']
    in_quote = False
    quote_char = None
    parens_level = 0
    state = 1
    column = 0
    for c in line:
      # 1 - start (label or label-opcode whitespace)
      if state == 1:
        if c in '*;!':
          line_comment = c
          line_comment_indent = column
          state = 2
        elif c.isspace():
          # state change to label-opcode whitespace
          lo_space = c
          state = 4
        else:
          # state change to in label
          label = c
          label_indent = column
          state = 3
      # 2 - in line_comment
      elif state == 2:
        line_comment += c
      # 3 - in label
      elif state == 3:
        if c in '*;!':
          line_comment = c
          line_comment_indent = column
          state = 2
        elif c.isspace():
          # state change to in label-opcode whitespace
          lo_space = c
          state = 4
        else:
          label += c
      # 4 - in label-opcode whitespace
      elif state == 4:
        if c in '*;!':
          line_comment = c
          line_comment_indent = column
          state = 2
        elif c.isspace():
          lo_space += c
        else:
          # state change to in opcode
          opcode = c
          opcode_indent = column
          state = 5
      # 5 - in opcode
      elif state == 5:
        if c in '*;!':
          line_comment = c
          line_comment_indent = column
          state = 2
        elif c.isspace():
          # state change to in op-args whitespace
          oa_space = c
          state = 6
        else:
          opcode += c
      # 6 - in op-args whitespace
      elif state == 6:
        if c in '*;!':
          line_comment = c
          line_comment_indent = column
          state = 2
        elif c.isspace():
          oa_space += c
        else:
          # state change to in args
          args = c
          args_indent = column
          state = 7
          if c in quotes:
            in_quote = True
            quote_char = c
          if c == '(':
            parens_level = 1
      # 7 - in args
      elif state == 7:
        if c in '*;!' and not in_quote and parens_level <= 0:
          line_comment = c
          line_comment_indent = column
          state = 2
        elif c.isspace() and not in_quote and parens_level <= 0:
          # state change to in args-comment whitespace
          ac_space = c
          state = 8
        else:
          args += c
          if in_quote:
            if c == quote_char:
              in_quote = False
              quote_char = None
          else:
            if c in quotes:
              in_quote = True
              quote_char = c
            if c == '(':
              parens_level += 1
            if c == ')':
              parens_level -= 1
      # 8 - in args-comment whitespace
      elif state == 8:
        if c in '*;!':
          line_comment = c
          line_comment_indent = column
          state = 2
        elif c.isspace():
          ac_space += c
        else:
          # state change to in comment
          comment = c
          comment_indent = column
          state = 9
      # 9 - in comment
      elif state == 9:
        comment += c

      column += 1

    indents = {
      'label': label_indent,
      'opcode': opcode_indent,
      'args': args_indent,
      'comment': comment_indent,
      'line_comment': line_comment_indent
    }

    # tokenize the code
    tokens = []

    if len(label) > 0:
      tokens.append(Token(label, 'label', False))

    if len(lo_space) > 0:
      tokens.append(Token(lo_space, 'whitespace', False))

    if len(opcode) > 0:
      tokens.append(Token(opcode, 'opcode', False))

    if len(oa_space) > 0:
      tokens.append(Token(oa_space, 'whitespace', False))

    if len(args) > 0:
      tokens += tokenizer.tokenize(args)

    if len(ac_space) > 0:
      tokens.append(Token(ac_space, 'whitespace', False))

    if len(comment) > 0:
      tokens.append(Token(comment, 'comment', False))

    if len(line_comment) > 0:
      tokens.append(Token(line_comment, 'comment', False))

    return tokens, indents


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
