import json

class Token:
  @staticmethod
  def __escape_z__():
    return 'Escape ?Z'


  def __init__(self, text, group, is_operand):
    self.text = text
    self.group = group
    self.is_operand = is_operand


  def __str__(self):
    return self.text


  def toDict(self):
    data = {
      "type": self.group,
      "value": self.text
    }

    return data
