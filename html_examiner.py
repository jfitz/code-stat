import string

from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  StuffedQuoteStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  ListTokenBuilder,
  BlockTokenBuilder
)
from html_token_builders import (
  HTMLIdentifierTokenBuilder,
  HTMLListTokenBuilder,
  HTMLAttributeTokenBuilder,
  HTMLUnicodeTokenBuilder
)
from examiner import Examiner

class HTMLExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    StuffedQuoteStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    ListTokenBuilder.__escape_z__()
    BlockTokenBuilder.__escape_z__()
    HTMLIdentifierTokenBuilder.__escape_z__()
    HTMLListTokenBuilder.__escape_z__()
    HTMLAttributeTokenBuilder.__escape_z__()
    HTMLUnicodeTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    quotes = ['"', "'", "’"]
    string_tb = StuffedQuoteStringTokenBuilder(quotes, False)
    attribute_tb = HTMLAttributeTokenBuilder()
    unicode_tb = HTMLUnicodeTokenBuilder()

    groupers_tb = ListTokenBuilder(['<', '</', '>', '/>'], 'group', False)
    group_starts = ['<']
    group_ends = ['>', '/>']

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
      newline_tb,
      whitespace_tb,
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
      unicode_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)
    tokens = tokenizer.tokenize(code)
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    self.tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    self.calc_token_confidence()
    self.calc_operator_confidence()
    self.calc_operator_2_confidence()
    self.calc_operator_3_confidence(group_ends)
    self.calc_operator_4_confidence(group_starts)
    # self.calc_operand_confidence(operand_types)
    self.calc_keyword_confidence()
    self.calc_statistics()
