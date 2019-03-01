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

    groupers_tb = ListTokenBuilder(['<', '</', '>', '/>'], 'grouper', False)
    identifier_tb = HTMLIdentifierTokenBuilder()
    # comment <!-- .* --> (spans lines)
    # punctuation
    # attribute &...;

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

    keyword_tb = ListTokenBuilder(keywords, 'keyword', True)

    comment_tb = BlockTokenBuilder('<!--', '-->', 'comment')

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
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    self.tokens = tokenizer.tokenize(code)

    # TODO: combine adjacent identifiers and punctuation into text
    # TODO: combine text and whitespace into text

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    # do not check for two operands in a row
