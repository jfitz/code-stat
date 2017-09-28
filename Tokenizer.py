class Tokenizer():
  def __init__(self, tokenizers, invalid_tokenizer):
    self.tokenizers = tokenizers
    self.invalid_tokenizer = invalid_tokenizer

  def tokenize(self, text):
    tokens = []
    while len(text) > 0:
      token = self.try_tokenizers(text)

      if token == '' and self.invalid_tokenizer != None:
          token = self.try_invalid(text)

      if token == '':
          raise Exception("Cannot tokenize '#{text}'")

      tokens.append(token)

      count = len(token)

      text = text[count:]
    
    return tokens
    
  def try_tokenizers(self, text):
    for tokenizer in self.tokenizers:
      tokenizer.attempt(text)

    token = ''
    # general tokenizers
    for tokenizer in self.tokenizers:
      t = tokenizer.token

      if len(t) > len(token):
        token = t

    return token

  def try_invalid(self, text):
    self.invalid_tokenizer.attempt(text)
    token = self.invalid_tokenizer.token
    return token
