class Token:
  def __init__(self, text, group):
    self.text = text
    self.group = group

  def __str__(self):
    return self.text

  def count(self):
    return len(self.text)

  def whitespace(self):
    return self.text[0].isspace()

  def comment(self):
    return self.text.startswith('(*') or self.text[0] == '{'