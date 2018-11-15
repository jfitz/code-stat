import json

class Token:
  def __init__(self, text, group):
    self.text = text
    self.group = group

  def __str__(self):
    return self.text

  def count(self):
    return len(self.text)

  def whitespace(self):
    return self.group == 'whitespace' or self.group == 'newline'

  def comment(self):
    return self.group == 'comment'

  def toDict(self):
    data = {
      "type": self.group,
      "value": self.text
    }

    return data
