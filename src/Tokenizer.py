class Tokenizer():
  def __init__(self, tokenizers, invalid_tokenizer):
    self.tokenizers = tokenizers
    self.invalid_tokenizer = invalid_tokenizer

  def tokenize(self, text):
    tokens = []
    while len(text) > 0:
      token = None
      tokenizer = self.try_tokenizers(text)

      if tokenizer is not None:
        token = tokenizer.get_token()
      else:
        if self.invalid_tokenizer != None:
          tokenizer = self.invalid_tokenizer
          token = self.try_invalid(text)

      if token == None:
        raise Exception("Cannot tokenize '#{text}'")

      tokens.append(token)
      count = tokenizer.get_count()
      text = text[count:]
    
    return tokens
    
  def try_tokenizers(self, text):
    # try all tokenbuilders
    for tokenizer in self.tokenizers:
      tokenizer.attempt(text)

    # see which tokenbuilder found the longest
    winner = None
    token = None
    count = 0
    for tokenizer in self.tokenizers:
      t = tokenizer.get_token()
      c = tokenizer.get_count()

      if t is not None:
        if c > count:
          token = t
          count = c
          winner = tokenizer

    return winner

  def try_invalid(self, text):
    self.invalid_tokenizer.attempt(text)
    token = self.invalid_tokenizer.get_token()
    return token
