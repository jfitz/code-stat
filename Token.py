import json

class Token:
  def __init__(self, text, group):
    self.text = text
    self.group = group


  def __str__(self):
    return self.text


  def toDict(self):
    data = {
      "type": self.group,
      "value": self.text
    }

    return data
