class Tokenizer():
  def __init__(self, tokenizers, invalid_tokenizer):
    self.tokenizers = tokenizers
    self.invalid_tokenizer = invalid_tokenizer

  def tokenize(self, text):
    tokens = []
    while len(text) > 0:
      token = self.try_tokenizers(text)

      if token == None and self.invalid_tokenizer != None:
        token = self.try_invalid(text)

      if token == None:
          raise Exception("Cannot tokenize '#{text}'")

      tokens.append(token)
      count = token.count()
      text = text[count:]
    
    return tokens
    
  def try_tokenizers(self, text):
    # try all tokenbuilders
    for tokenizer in self.tokenizers:
      tokenizer.attempt(text)

    # see which tokenbuilder found the longest
    token = None
    for tokenizer in self.tokenizers:
      t = tokenizer.get_token()

      if t is not None:
        if token is None:
          token = t
        else:
          if t.count() > token.count():
            token = t

    return token

  def try_invalid(self, text):
    self.invalid_tokenizer.attempt(text)
    token = self.invalid_tokenizer.get_token()
    return token
