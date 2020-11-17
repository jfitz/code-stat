from token_builders import EscapedStringTokenBuilder

# token reader for text literal (string)
class MatlabStringTokenBuilder(EscapedStringTokenBuilder):
  @staticmethod
  def __escape_z__():
    EscapedStringTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, quotes, allow_newline):
    super().__init__(quotes, allow_newline)


  def get_score(self, line_printable_tokens):
    if self.text is None:
      return 0

    if len(self.text) < 2:
      return 0

    if self.text[-1] != self.text[0]:
      return 0

    operand_groups = ['number', 'variable', 'identifier']

    if len(line_printable_tokens) > 0 and \
      line_printable_tokens[-1].group in operand_groups:
      return 0

    return len(self.text)
