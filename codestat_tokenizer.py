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
