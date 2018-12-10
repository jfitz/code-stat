class Tokenizer():
  def __init__(self, tokenizers, invalid_tokenizer):
    self.tokenizers = tokenizers
    self.invalid_tokenizer = invalid_tokenizer

  def tokenize(self, text):
    tokens = []

    while len(text) > 0:
      new_tokens = None

      tokenizer = self.try_tokenizers(text)

      if tokenizer is not None:
        new_tokens = tokenizer.get_tokens()
      else:
        if self.invalid_tokenizer != None:
          tokenizer = self.invalid_tokenizer
          new_tokens = self.try_invalid(text)

      if new_tokens == None:
        raise Exception("Cannot tokenize '" + text + "'")

      tokens += new_tokens
      count = tokenizer.get_count()
      text = text[count:]
    
    return tokens
    
  def try_tokenizers(self, text):
    # try all tokenbuilders
    for tokenizer in self.tokenizers:
      tokenizer.attempt(text)

    # see which tokenbuilder found the longest
    winner = None
    winner_score = 0
    for tokenizer in self.tokenizers:
      t = tokenizer.get_tokens()
      score = tokenizer.get_score()

      if t is not None:
        if score > winner_score:
          winner = tokenizer
          winner_score = winner.get_score()

    return winner

  def try_invalid(self, text):
    self.invalid_tokenizer.attempt(text)
    tokens = self.invalid_tokenizer.get_tokens()
    if tokens is None:
      return None

    return tokens
