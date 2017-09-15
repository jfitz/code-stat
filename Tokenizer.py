class Tokenizer():
  def __init__(self, tokenizers, invalid_tokenizer):
    self.tokenizers = tokenizers
    self.invalid_tokenizer = invalid_tokenizer

  def tokenize(self, text):
    tokens = []
    while len(text) > 0:
      token, count = self.try_tokenizers(text)

      if token == None and self.invalid_tokenizer != None:
          token, count = self.try_invalid(text)
          
      if token == None:
          raise Exception("Cannot tokenize '#{text}'")

      if isinstance(token, list):
        tokens += token
      else:
        tokens.append(token)

      text = text[count:-1]
    
    return tokens
    
  def try_tokenizers(self, text):
    for tokenizer in self.tokenizers:
        tokenizer.attempt(text)
    count = 0
    token = None
    # general tokenizers
    for tokenizer in self.tokenizers:
      if tokenizer.count > count:
        token = tokenizer.token()
        count = tokenizer.count()

    return [token, count]

  def try_invalid(self, text):
    self.invalid_tokenizer.attempt(text)
    token = self.invalid_tokenizer.token()
    count = self.invalid_tokenizer.count()
    return [token, count]
