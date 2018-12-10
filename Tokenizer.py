class Tokenizer():
  def __init__(self, tokenbuilders, invalid_tokenbuilder):
    self.tokenbuilders = tokenbuilders
    self.invalid_tokenbuilder = invalid_tokenbuilder

  def tokenize(self, text):
    tokens = []

    while len(text) > 0:
      new_tokens = None

      tokenbuilder = self.try_tokenbuilders(text)

      if tokenbuilder is not None:
        new_tokens = tokenbuilder.get_tokens()
      else:
        if self.invalid_tokenbuilder != None:
          tokenbuilder = self.invalid_tokenbuilder
          new_tokens = self.try_invalid(text)

      if new_tokens == None:
        raise Exception("Cannot tokenize '" + text + "'")

      tokens += new_tokens
      count = tokenbuilder.get_count()
      text = text[count:]
    
    return tokens
    
  def try_tokenbuilders(self, text):
    # try all tokenbuilders
    for tokenbuilder in self.tokenbuilders:
      tokenbuilder.attempt(text)

    # see which tokenbuilder found the highest score
    winner = None
    winner_score = 0
    for tokenbuilder in self.tokenbuilders:
      t = tokenbuilder.get_tokens()
      score = tokenbuilder.get_score()

      if t is not None:
        if score > winner_score:
          winner = tokenbuilder
          winner_score = winner.get_score()

    return winner

  def try_invalid(self, text):
    self.invalid_tokenbuilder.attempt(text)
    tokens = self.invalid_tokenbuilder.get_tokens()
    if tokens is None:
      return None

    return tokens
