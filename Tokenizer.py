from Token import Token

class Tokenizer():
  def __init__(self, tokenbuilders, invalid_tokenbuilder):
    self.tokenbuilders = tokenbuilders
    self.invalid_tokenbuilder = invalid_tokenbuilder

  def tokenize(self, text):
    tokens = []

    last_printable_token = Token('\n', 'newline')

    while len(text) > 0:
      new_tokens = None

      tokenbuilder = self.try_tokenbuilders(text, last_printable_token)

      if tokenbuilder is not None:
        new_tokens = tokenbuilder.get_tokens()
      else:
        if self.invalid_tokenbuilder != None:
          tokenbuilder = self.invalid_tokenbuilder
          new_tokens = self.try_invalid(text)

      if new_tokens == None:
        raise Exception("Cannot tokenize '" + text + "'")

      for token in new_tokens:
        if token.group not in ['whitespace', 'comment']:
          last_printable_token = token

      tokens += new_tokens
      count = tokenbuilder.get_count()
      text = text[count:]
    
    return tokens
    
  def try_tokenbuilders(self, text, last_printable_token):
    # try all tokenbuilders
    for tokenbuilder in self.tokenbuilders:
      tokenbuilder.attempt(text)

    # see which tokenbuilder found the highest score
    winner = None
    winner_score = 0
    for tokenbuilder in self.tokenbuilders:
      t = tokenbuilder.get_tokens()
      score = tokenbuilder.get_score(last_printable_token)

      if t is not None:
        if score > winner_score:
          winner = tokenbuilder
          winner_score = winner.get_score(last_printable_token)

    return winner

  def try_invalid(self, text):
    self.invalid_tokenbuilder.attempt(text)
    tokens = self.invalid_tokenbuilder.get_tokens()
    if tokens is None:
      return None

    return tokens
