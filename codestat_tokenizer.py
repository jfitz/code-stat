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

  # tokenize according to space layout
  # label at left
  # opcode
  # arguments (no spaces within, or only in quoted strings)
  # comment
  @staticmethod
  def tokenize_asm_code(text, tab_size, tokenizer, opcode_extras, label_leads, label_mids, label_ends, comment_leads):
    lines = text.split('\n')

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
      line_tokens, line_indents = Tokenizer.tokenize_asm_line(line, tokenizer, opcode_extras, label_leads, label_mids, label_ends, comment_leads)
      tokens += line_tokens
      indents.append(line_indents)

      tokens.append(Token(newline, 'newline', False))

    # return tokens and indents
    return tokens, indents


  @staticmethod
  def tokenize_asm_line(line, tokenizer, opcode_extras, label_leads, label_mids, label_ends, comment_leads):
    # break apart the line based on format
    # The line format is:
    # label
    # opcode
    # arguments
    # comment

    START_LABEL_OR_WHITESPACE = 1
    AFTER_LABEL = 2
    IN_LINE_COMMENT = 3
    IN_LABEL = 4
    LABEL_OPCODE_WHITESPACE = 5
    IN_OPCODE = 6
    OPCODE_ARGS_WHITESPACE = 7
    IN_ARGS = 8
    ARGS_COMMENT_WHITESPACE = 9
    IN_COMMENT = 10

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
    state = START_LABEL_OR_WHITESPACE
    column = 0
    prev_args_indent = 16
    prev_comment_indent = 24
    prev_c = ' '
    for c in line:
      # start (label or label-opcode whitespace)
      if state == START_LABEL_OR_WHITESPACE:
        if c in comment_leads:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace():
          # state change to label-opcode whitespace
          lo_space = c
          state = LABEL_OPCODE_WHITESPACE
        else:
          # state change to in label
          label = c
          label_indent = column
          state = IN_LABEL
      # in line_comment
      elif state == IN_LINE_COMMENT:
        line_comment += c
      # in label
      elif state == IN_LABEL:
        if c in comment_leads:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace():
          # state change to in label-opcode whitespace
          lo_space = c
          state = LABEL_OPCODE_WHITESPACE
        elif c in label_ends:
          label += c
          state = AFTER_LABEL
        else:
          label += c
      # after label and first character of whitespace or opcode
      elif state == AFTER_LABEL:
        if c.isspace():
          lo_space = c
          state = LABEL_OPCODE_WHITESPACE
        else:
          opcode = c
          state = IN_OPCODE
      # in label-opcode whitespace
      elif state == LABEL_OPCODE_WHITESPACE:
        if c in comment_leads:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace():
          lo_space += c
        else:
          # state change to in opcode
          opcode = c
          opcode_indent = column
          state = IN_OPCODE
      # in opcode
      elif state == IN_OPCODE:
        if c in comment_leads:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace():
          # state change to in op-args whitespace
          oa_space = c
          state = OPCODE_ARGS_WHITESPACE
        else:
          opcode += c
      # in op-args whitespace
      elif state == OPCODE_ARGS_WHITESPACE:
        if c in comment_leads:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace():
          oa_space += c
        else:
          # state change to in args or in comment, depending on position
          if abs(column - prev_args_indent) < abs(column - prev_comment_indent):
            args = c
            args_indent = column
            state = IN_ARGS
            prev_args_indent = column
          else:
            comment = c
            comment_indent = column
            state = IN_COMMENT
            prev_comment_indent = column
          if c in quotes:
            in_quote = True
            quote_char = c
          if c == '(':
            parens_level = 1
      # in args
      elif state == IN_ARGS:
        if c in comment_leads and not in_quote and parens_level <= 0:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace() and not in_quote and parens_level <= 0:
          # state change to in args-comment whitespace
          ac_space = c
          state = ARGS_COMMENT_WHITESPACE
        else:
          args += c
          if in_quote:
            if c == quote_char:
              in_quote = False
              quote_char = None
          else:
            if c in quotes and prev_c not in 'LN':
              in_quote = True
              quote_char = c
            if c == '(':
              parens_level += 1
            if c == ')':
              parens_level -= 1
      # in args-comment whitespace
      elif state == ARGS_COMMENT_WHITESPACE:
        if c in comment_leads:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace():
          ac_space += c
        else:
          # state change to in comment
          comment = c
          comment_indent = column
          state = IN_COMMENT
      # in comment
      elif state == IN_COMMENT:
        comment += c

      column += 1
      prev_c = c

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
      if label.isdigit():
        tokens.append(Token(label, 'number', True))
      elif Tokenizer.check_label_format(label, label_leads, label_mids, label_ends):
        tokens.append(Token(label, 'label', False))
      else:
        tokens.append(Token(label, 'invalid', False))

    if len(lo_space) > 0:
      tokens.append(Token(lo_space, 'whitespace', False))

    if len(opcode) > 0:
      # TODO: more than one token reduces confidence
      opcode_tokens = tokenizer.tokenize(opcode)
      for token in opcode_tokens:
        if token.group == 'identifier':
          token.group = 'opcode'
      tokens += opcode_tokens

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
