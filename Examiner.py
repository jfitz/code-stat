from codestat_token import Token
from token_builders import ListTokenBuilder

class Examiner:
  def __init__(self):
    operators = [
      '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
      '-', '+', '=',
      '[', ']', '{', '}', '/', '|', '\\',
      ',', '.', ':', ';',
      '>', '<',
      '?'
    ]

    self.tokens = []
    self.unknown_operator_tb = ListTokenBuilder(operators, 'invalid operator', True)
    self.unary_operators = []
    self.postfix_operators = []
    self.adjective_operators = []
    self.keyword_postfix = []
    self.confidences = {}
    self.errors = []
    self.statistics = {}
    self.newlines_important = 'never'


  def confidence(self):
    value = 1.0

    for name in self.confidences:
      value *= self.confidences[name]

    return value


  def tabs_to_spaces(self, text, tab_size):
    if tab_size is None:
      tab_size = 8

    column = 0
    detabbed_text = ''

    for c in text:
      if c == '\n':
        detabbed_text += c
        column = 0
      else:
        if c == '\t':
          next_tab_stop = int((column + tab_size) / tab_size) * tab_size
          while column < next_tab_stop:
            detabbed_text += ' '
            column += 1
        else:
          detabbed_text += c
          column += 1
    
    return detabbed_text


  def invalid_operators(self):
    tokens = []
    for token in self.tokens:
      if token.group == 'invalid operator':
        tokens.append(token)

    return tokens


  def count_invalid_operators(self):
    num = 0
    for token in self.tokens:
      if token.group == 'invalid operator':
        num += 1

    return num


  def count_known_operators(self):
    num = 0
    for token in self.tokens:
      if token.group == 'operator':
        num += 1

    return num


  def count_tokens(self, group):
    count = 0

    for token in self.tokens:
      if token.group in group:
        count += 1

    return count


  def convert_identifiers_to_functions(self):
    prev_token = Token('\n', 'newline')

    for token in self.tokens:
      if token.group == 'identifier' and\
        prev_token.group == 'group' and prev_token.text == '(':
        token.group = 'function'

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token


  def check_paired_tokens(self, tokens, open_tokens, close_tokens):
    level = 0
    min_level = 0
    num_open = 0
    num_close = 0

    for token in tokens:
      token_lower = token.text.lower()

      if token_lower in open_tokens:
        num_open += 1
        level += 1

      if token_lower in close_tokens:
        num_close += 1
        level -= 1
        if level < min_level:
          min_level = level

    ok = level == 0 and min_level == 0
    return ok, num_open, num_close


  def split_tokens_into_lines(self, tokens):
    token_groups = []

    token_group = []

    for token in tokens:
      if token.group == 'newline':
        if len(token_group) > 0:
          token_groups.append(token_group)
          token_group = []
      else:
        token_group.append(token)
    
    if len(token_group) > 0:
      token_groups.append(token_group)

    return token_groups


  @staticmethod
  def drop_tokens(tokens, types):
    new_list = []

    for token in tokens:
      if token.group not in types:
        new_list.append(token)
    
    return new_list


  @staticmethod
  def combine_adjacent_identical_tokens(tokens, group):
    new_list = []

    new_token = None
    for token in tokens:
      if token.group == group and new_token is not None and new_token.group == group:
        new_token = Token(new_token.text + token.text, group)
      else:
        if new_token is not None:
            new_list.append(new_token)
        new_token = token
    if new_token is not None:
      new_list.append(new_token)

    return new_list


  def convert_keywords_to_identifiers(self):
    prev_token = Token('\n', 'newline')

    for token in self.tokens:
      if token.group == 'keyword' and\
        prev_token.group == 'operator' and prev_token.text == '.':
        token.group = 'identifier'

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token


  def calc_line_length_confidence(self, code, width):
    num_ok_lines = 0
    lines = code.split('\n')
    for line in lines:
      line = line.rstrip('\r')
      line = line.rstrip()
      if len(line) <= width:
        num_ok_lines += 1
      else:
        self.errors.append({
          'TYPE': 'LENGTH',
          'LONG': line,
          'WIDTH': width
        })


    line_length_confidence = 1.0
    if len(lines) > 0:
      line_length_confidence = num_ok_lines / len(lines)

    self.confidences['line_length'] = line_length_confidence


  # count invalid tokens
  @staticmethod
  def count_invalid_tokens(tokens):
    num_invalid_tokens = 0

    for token in tokens:
      if token.group.startswith('invalid'):
        num_invalid_tokens += 1

    return num_invalid_tokens


  # unknown tokens reduce confidence
  def calc_token_confidence(self):
    num_invalid_tokens = 0

    for token in self.tokens:
      if token.group.startswith('invalid'):
        num_invalid_tokens += 1

        self.errors.append({
          'TYPE': 'TOKEN',
          'INVALID': token.text,
          'FIRST': '',
          'SECOND': ''
        })

    token_confidence = 1.0

    if len(self.tokens) > 0:
      num_known_tokens = len(self.tokens) - num_invalid_tokens
      token_confidence = num_known_tokens / len(self.tokens)

    self.confidences['token'] = token_confidence


  #  unknown operators reduce confidence
  def calc_operator_confidence(self):
    invalid_operators = self.invalid_operators()
    num_invalid_operators = self.count_invalid_operators()
    num_known_operators = self.count_known_operators()
    num_operators = num_known_operators + num_invalid_operators

    for operator in invalid_operators:
      self.errors.append({
        'TYPE': 'OPERATOR',
        'INVALID': operator.text,
        'FIRST': '',
        'SECOND': ''
      })

    operator_confidence = 1.0

    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    self.confidences['operator'] = operator_confidence


  # binary operators that follow operators reduce confidence
  def calc_operator_2_confidence(self, tokens):
    num_invalid_operators = self.count_invalid_operators()
    num_known_operators = self.count_known_operators()
    num_operators = num_known_operators + num_invalid_operators

    operator_confidence_2 = 1.0

    if num_operators > 0:
      errors = 0
      prev_token = Token('\n', 'newline')

      for token in tokens:
        if token.group == 'operator' and\
          prev_token.group == 'operator' and\
          prev_token.text not in self.adjective_operators and\
          prev_token.text not in self.postfix_operators and\
          token.text.lower() not in (op.lower() for op in self.unary_operators):
          errors += 1
          self.errors.append({
            'TYPE': 'OPERATOR2',
            'FIRST': prev_token.text,
            'SECOND': token.text
            })

        prev_token = token

      operator_confidence_2 = 1.0 - errors / num_operators

    self.confidences['operator_2'] = operator_confidence_2


  # binary operators that follow non-operands reduce confidence
  def calc_operator_3_confidence(self, tokens, group_ends):
    num_invalid_operators = self.count_invalid_operators()
    num_known_operators = self.count_known_operators()
    num_operators = num_known_operators + num_invalid_operators

    operator_confidence_3 = 1.0

    if num_operators > 0:
      errors = 0
      prev_token = Token('\n', 'newline')

      operand_types = [
        'number',
        'string',
        'variable',
        'identifier',
        'function',
        'symbol',
        'regex',
        'type',
        'value'
      ]

      for token in tokens:
        prev_token_operand = prev_token.group in operand_types or \
          (prev_token.group == 'group' and prev_token.text in group_ends) or \
          prev_token.text.lower() == 'end'
  
        token_unary_operator = token.text.lower() in (op.lower() for op in self.unary_operators)
  
        if token.group == 'operator' and\
          not prev_token_operand and\
          prev_token.text not in self.adjective_operators and\
          prev_token.text not in self.postfix_operators and\
          not (prev_token.group == 'keyword' and token.text in self.keyword_postfix) and\
          not token_unary_operator:
          errors += 1
          self.errors.append({
            'TYPE': 'OPERATOR3',
            'FIRST': prev_token.text,
            'SECOND': token.text
            })

        prev_token = token

      operator_confidence_3 = 1.0 - errors / num_operators

    self.confidences['operator_3'] = operator_confidence_3


  # binary operators that precede non-operands reduce confidence
  def calc_operator_4_confidence(self, tokens, group_starts):
    num_invalid_operators = self.count_invalid_operators()
    num_known_operators = self.count_known_operators()
    num_operators = num_known_operators + num_invalid_operators

    operator_confidence_4 = 1.0

    if num_operators > 0:
      operand_types = [
        'number',
        'string',
        'variable',
        'identifier',
        'function',
        'symbol',
        'regex',
        'type',
        'value',
        'picture'
      ]

      errors = 0
      prev_token = Token('\n', 'newline')

      for token in tokens:
        token_unary_operator = token.text.lower() in (op.lower() for op in self.unary_operators)
        prev_token_postfix_operator = prev_token.text.lower() in (op.lower() for op in self.postfix_operators)
  
        if prev_token.group == 'operator' and \
          not prev_token_postfix_operator and \
          token.group not in operand_types and \
          not token_unary_operator and \
          token.text not in group_starts:
          errors += 1
          self.errors.append({
            'TYPE': 'OPERATOR4',
            'FIRST': prev_token.text,
            'SECOND': token.text
            })

        prev_token = token

      operator_confidence_4 = 1.0 - errors / num_operators

    self.confidences['operator_4'] = operator_confidence_4


  def calc_paired_blockers_confidence(self, openers, closers):
    # consider the number of matches for begin/end
    ok, num_begin, num_end = self.check_paired_tokens(self.tokens, openers, closers)
    num_begin_end = num_begin + num_end
    paired_blocker_confidence = 1.0

    if num_begin_end > 0:
      paired_blocker_confidence = (num_begin + num_end) / num_begin_end

    if not ok:
      paired_blocker_confidence *= 0.75

    self.confidences['paired_blockers_match'] = paired_blocker_confidence


  def calc_line_format_confidence(self):
    self.confidences['line format'] = 1.0


  # unwrap lines (drop line continuation tokens and tokens including newline)
  @staticmethod
  def unwrap_code_lines(tokens):
    unwrapped_tokens = []
    include = True
    prev_tokens = [
      Token('', 'newline'),
      Token('', 'newline'),
      Token('', 'newline')
    ]

    for token in tokens:
      if token.group == 'line continuation':
        include = False
        prev_tokens.append(token)
        prev_tokens = prev_tokens[1:]

      if token.group == 'whitespace' and \
        prev_tokens[-1].group == 'newline' and \
        prev_tokens[-2].group == 'line continuation':
        if prev_tokens[-3].group != 'whitespace':
          unwrapped_tokens.append(Token(' ', 'whitespace'))
      elif include:
        unwrapped_tokens.append(token)
        prev_tokens.append(token)
        prev_tokens = prev_tokens[1:]

      if token.group == 'newline':
        if not include:
          prev_tokens.append(token)
          prev_tokens = prev_tokens[1:]
        include = True

    return unwrapped_tokens


  def source_tokens(self):
    # remove tokens we don't care about
    drop_types = ['whitespace', 'comment']
    tokens = self.drop_tokens(self.tokens, drop_types)

    tokens = Examiner.join_continued_lines(tokens)

    return tokens


  def combine_tokens_and_adjacent_types(self, tokens, type1, type2, set2):
    new_list = []

    new_token = None
    for token in tokens:
      if token.group == type2 and token.text in set2 and \
         new_token is not None and new_token.group == type1:
        new_token = Token(new_token.text + token.text, type1)
      else:
        if new_token is not None:
          new_list.append(new_token)
        new_token = token
    if new_token is not None:
      new_list.append(new_token)

    return new_list


  # two operands in a row decreases confidence
  def calc_operand_confidence(self, tokens, operand_types):
    two_operand_count = 0
    prev_token = Token('\n', 'newline')
    for token in tokens:
      if token.group in operand_types and prev_token.group in operand_types:
        two_operand_count += 1
        self.errors.append({
          'TYPE': 'OPERAND',
          'FIRST': prev_token.text,
          'SECOND': token.text
          })

      prev_token = token

    operand_confidence = 1.0
    if len(tokens) > 0:
      operand_confidence = 1.0 - (two_operand_count / len(tokens))

    self.confidences['operand'] = operand_confidence


  # two values in a row decreases confidence
  def calc_value_value_different_confidence(self, tokens):
    # remove tokens we don't care about
    drop_types = ['whitespace', 'comment', 'line continuation']
    tokens = Examiner.drop_tokens(self.tokens, drop_types)

    value_types = ['number', 'string', 'symbol']

    two_value_count = 0
    prev_token = Token('\n', 'newline')
    for token in tokens:
      if token.group in value_types and\
        prev_token.group in value_types and\
        not token.group == prev_token.group:
        two_value_count += 1
        self.errors.append({
          'TYPE': 'VALUE VALUE DIFFERENT',
          'FIRST': prev_token.text,
          'SECOND': token.text
          })

      prev_token = token

    value_value_confidence = 1.0
    if len(tokens) > 0:
      value_value_confidence = 1.0 - (two_value_count / len(tokens))

    self.confidences['value value'] = value_value_confidence


  def calc_picture_confidence(self):
    self.confidences['picture'] = 1.0


  def calc_keyword_confidence(self):
    group = ['keyword', 'type', 'function']
    num_keywords = self.count_tokens(group)

    if num_keywords > 0:
      self.confidences['keyword'] = 1.0
    else:
      # if no keywords, compute based on tokens
      self.errors.append({
        'TYPE': 'KEYWORDS',
        'MESSAGE': 'No keywords found'
        })

      num_tokens = len(self.tokens)
      num_invalid_tokens = Examiner.count_invalid_tokens(self.tokens)
      num_valid_tokens = num_tokens - num_invalid_tokens

      # fewer than 1000 tokens and no keyword? possible
      if num_valid_tokens > 0 and num_tokens < 1000:
        # the more invalid tokens, the less likely
        confidence_1 = 1.0 - num_invalid_tokens / num_tokens
        # the more tokens, the less likely
        confidence_2 = 1.0 - num_tokens / 1000
        self.confidences['keyword'] = confidence_1 * confidence_2
      else:
        # more than 1000 tokens and no keyword? assume it is not
        self.confidences['keyword'] = 0.0


  def count_source_lines(self):
    # count source lines
    # (lines with tokens other than space or comment or line number or line description)
    drop_types = ['whitespace', 'comment', 'line number', 'line identification']
    tokens = Examiner.drop_tokens(self.tokens, drop_types)
    line_count = 0
    token_count = 0

    for token in tokens:
      if token.group == 'newline':
        if token_count > 0:
          line_count += 1
        token_count = 0
      else:
        token_count += 1

    # count a line that has no trailing newline
    if token_count > 0:
      line_count += 1

    return line_count


  def calc_pair_balance(self, openers, closers):
    bias = 0
    for token in self.tokens:
      if token.group == 'group':
        if token.text in openers:
          bias += 1
        if token.text in closers:
          bias -= 1

    return bias


  def calc_statistics(self):
    self.statistics = {}

    # count token types
    for token in self.tokens:
      ctype = token.group
      if ctype in self.statistics:
        self.statistics[ctype] += 1
      else:
        self.statistics[ctype] = 1

    # count source lines
    self.statistics['source lines'] = self.count_source_lines()

    # check group items are balanced
    self.statistics['parentheses bias'] = self.calc_pair_balance('(', ')')
    self.statistics['bracket bias'] = self.calc_pair_balance('[', ']')
    self.statistics['brace bias'] = self.calc_pair_balance('{', '}')

    # calculate complexity based on decision points

    # calculate complexity based on boolean constants (boolexity)


  # join lines that are connected via line continuation
  @staticmethod
  def join_continued_lines(tokens):
    new_tokens = []

    prev_token = Token('\n', 'newline')

    for token in tokens:
      keep = True

      # don't append newlines after line continuations
      # never append line continations
      if token.group == 'newline' and prev_token.group == 'line continuation':
        keep = False

      if token.group == 'line continuation':
        keep = False

      if keep:
        new_tokens.append(token)

      # always remember the last token, even if we dropped it
      prev_token = token

    return new_tokens


  # join lines
  @staticmethod
  def join_all_lines(tokens):
    drop_types = ['newline']
    tokens = Examiner.drop_tokens(tokens, drop_types)

    return tokens


  # join lines that are connected via dangling operators
  @staticmethod
  def join_operator_continued_lines(tokens, postfix_operators):
    new_tokens = []

    prev_token = Token('\n', 'newline')

    for token in tokens:
      keep = True

      # don't append newlines after line continuations
      # never append line continations
      if token.group == 'newline' and \
        prev_token.group == 'operator' and \
        prev_token.text not in postfix_operators:
        keep = False

      if keep:
        new_tokens.append(token)
        prev_token = token

    return new_tokens


  # join lines that are connected with open parentheses
  @staticmethod
  def join_parens_continued_lines(tokens):
    new_tokens = []

    # keep track of open and close parentheses
    parens_count = 0

    for token in tokens:
      if token.group == 'newline' and parens_count > 0:
        continue

      if token.group == 'group' and token.text == '(':
        parens_count += 1

      if token.group == 'group' and token.text == ')':
        parens_count -= 1

      new_tokens.append(token)
    
    return new_tokens


  def unwrapped_code(self):
    tokens = Examiner.unwrap_code_lines(self.tokens)
    text = ''

    for token in tokens:
      if token.group == 'newline':
        text += '\n'
      else:
        text += token.text

    return text
