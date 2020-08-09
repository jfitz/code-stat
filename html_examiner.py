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
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
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
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    BlockTokenBuilder.__escape_z__()
    HTMLIdentifierTokenBuilder.__escape_z__()
    HTMLListTokenBuilder.__escape_z__()
    HTMLAttributeTokenBuilder.__escape_z__()
    HTMLUnicodeTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder(None)
    integer_exponent_tb = IntegerExponentTokenBuilder(None)
    real_tb = RealTokenBuilder(False, False, None)
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', None)
    operand_types.append('number')

    quotes = ['"', "'", "’"]
    string_tb = StuffedQuoteStringTokenBuilder(quotes, False)
    operand_types.append('string')

    attribute_tb = HTMLAttributeTokenBuilder()
    operand_types.append('attribute')

    unicode_tb = HTMLUnicodeTokenBuilder()
    operand_types.append('character')

    groupers_tb = CaseInsensitiveListTokenBuilder(['<', '</', '>', '/>'], 'group', False)
    group_starts = ['<']
    group_ends = ['>', '/>']

    identifier_tb = HTMLIdentifierTokenBuilder()
    operand_types.append('identifier')

    known_operators = [
      '='
    ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)

    punctuation = [
      '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '+',
      '`', '~', '[', ']', '{', '}', ';', ':', "'", '"',
      ',', '.', '?', '>', '/', '\\', '|' # omit '<'
    ]

    punctuation_tb = CaseInsensitiveListTokenBuilder(punctuation, 'punctuation', False)

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

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    # self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    # self.calc_group_confidence(tokens, group_mids)

    # self.calc_operand_n_confidence(tokens, operand_types, 2)
    # self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()
