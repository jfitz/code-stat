import string

class Examiner:
  def remove_string_literals(self, line):
    result = ''
    in_string = False
    for c in line:
      if c == '"' and not in_string:
        in_string = True
        c = ''

      if not in_string:
        result += c

      if c == '"' and in_string:
        in_string = False
      
    return result
  
  def split_to_tokens(self, line):
    tokens = []
    st = 0
    token = ''
    for c in line:
      t = self.chartype(c)
      if st == 0 or t == st:
        token = token + c
        st = t
      else:
        if len(token) > 0 and (st == 1 or st == 4):
          tokens.append(token)
        token = c
        st = t
    if len(token) > 0 and (st == 1 or st == 4):
      tokens.append(token)
    return tokens

  def chartype(self, c):
    retval = 4
    if c.isalpha():
      retval = 1
    if c.isdigit():
      retval = 2
    if c.isspace():
      retval = 3
    if c in string.punctuation:
      retval = 4
    return retval

  def drop_numbers(self, tokens):
    results = []
    for token in tokens:
      if not token[0].isdigit():
        results.append(token)
    return results
