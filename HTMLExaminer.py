import string
from Examiner import Examiner
from TokenBuilders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder,
  BlockTokenBuilder
)
from HTMLTokenBuilders import (
  HTMLIdentifierTokenBuilder,
  HTMLListTokenBuilder,
  HTMLAttributeTokenBuilder
)
from Tokenizer import Tokenizer

class HTMLExaminer(Examiner):
  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(False)
    integer_exponent_tb = IntegerExponentTokenBuilder()
    real_tb = RealTokenBuilder(False, False)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E')
    string_tb = StringTokenBuilder(['"', "'"], True, False)
    attribute_tb = HTMLAttributeTokenBuilder()

    groupers_tb = ListTokenBuilder(['<', '</', '>', '/>'], 'group', False)
    identifier_tb = HTMLIdentifierTokenBuilder()

    known_operators = [
      '='
    ]

    known_operator_tb = ListTokenBuilder(known_operators, 'operator', True)

    punctuation = [
      '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '+',
      '`', '~', '[', ']', '{', '}', ';', ':', "'", '"',
      ',', '.', '?', '>', '/', '\\', '|' # omit '<'
    ]

    punctuation_tb = ListTokenBuilder(punctuation, 'punctuation', False)

    keywords = [
      'a', 'abbr', 'address', 'area', 'article', 'aside', 'audio',
      'b', 'base', 'bdi', 'bdo', 'blockquote', 'body', 'br', 'button',
      'canvas', 'caption', 'cite', 'code', 'col', 'colgroup',
      'data', 'datalist', 'dd', 'del', 'details', 'dfn', 'dialog',
      'div', 'dl', 'dt',
      'em', 'embed',
      'fieldset', 'figcaption', 'figure', 'footer', 'form',
      'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'head', 'header',
      'hgroup', 'hr', 'html',
      'i', 'iframe', 'img', 'input', 'ins',
      'kbd', 'keygen',
      'label', 'legend', 'li', 'link',
      'main', 'map', 'mark', 'math', 'menu', 'menuitem',
      'meta', 'meter',
      'nav', 'noscript',
      'object', 'ol', 'optgroup', 'option', 'output',
      'p', 'param', 'picture', 'pre', 'progress',
      'q',
      'rb', 'rp', 'rt', 'rtc', 'ruby',
      's', 'samp', 'script', 'section', 'select', 'slot', 'small',
      'source', 'span', 'strong', 'style', 'sub', 'summary', 'sup', 'svg',
      'table', 'tbody', 'td', 'template', 'textarea', 'tfoot', 'th', 'thead',
      'time', 'title', 'tr', 'track',
      'u', 'ul',
      'var', 'video',
      'wbr'
    ]

    keyword_tb = HTMLListTokenBuilder(keywords, 'keyword', False)

    comment_tb = BlockTokenBuilder('<!--', '-->', 'comment')

    script_tb = BlockTokenBuilder('<script', '</script>', 'script')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      whitespace_tb,
      newline_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      string_tb,
      attribute_tb,
      known_operator_tb,
      groupers_tb,
      keyword_tb,
      identifier_tb,
      punctuation_tb,
      comment_tb,
      script_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # self.calc_operator_3_confidence()
    # self.calc_operand_confidence()
    self.calc_keyword_confidence()
