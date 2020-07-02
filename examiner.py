from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import CaseSensitiveListTokenBuilder

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
    self.unknown_operator_tb = CaseSensitiveListTokenBuilder(operators, 'invalid operator', False)
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


  @staticmethod
  def tabs_to_spaces(text, tab_size):
    if tab_size is None:
      tab_size = 8

    column = 0
    detabbed_text = ''

    for c in text:
      if c == '\n':
        detabbed_text += c
        column = 0
      elif c == '\r':
        detabbed_text += c
      elif c == '\t':
        next_tab_stop = int((column + tab_size) / tab_size) * tab_size
        while column < next_tab_stop:
          detabbed_text += ' '
          column += 1
      else:
        detabbed_text += c
        column += 1
    
    return detabbed_text


  @staticmethod
  def find_tokens(tokens, groups):
    found_tokens = []

    for token in tokens:
      if token.group in groups:
        found_tokens.append(token)

    return found_tokens


  @staticmethod
  def count_tokens(tokens, groups):
    count = 0

    for token in tokens:
      if token.group in groups:
        count += 1

    return count


  def count_my_tokens(self, groups):
    count = 0

    for token in self.tokens:
      if token.group in groups:
        count += 1

    return count


  def convert_identifiers_to_functions(self):
    prev_token = Token('\n', 'newline', False)

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
        new_token = Token(new_token.text + token.text, group, token.is_operand)
      else:
        if new_token is not None:
            new_list.append(new_token)
        new_token = token

    if new_token is not None:
      new_list.append(new_token)

    return new_list


  # convert identifiers followed by colons to labels
  # but only if they are the first printable tokens on the line
  @staticmethod
  def combine_identifier_colon(tokens, separator_groups, separator_texts, ignore_groups):
    new_list = []

    new_token = None
    first_printable_token = True

    for token in tokens:
      if token.text == ':' and \
        new_token is not None and new_token.group == 'identifier' and \
        first_printable_token:
        new_token = Token(new_token.text + token.text, 'label', False)
      else:
        if new_token is not None:
            new_list.append(new_token)
            if new_token.group in separator_groups or \
              new_token.text in separator_texts:
              first_printable_token = True
            else:
              if new_token.group not in ignore_groups:
                first_printable_token = False
        new_token = token

    if new_token is not None:
      new_list.append(new_token)

    return new_list


  # convert keywords after specified operators to identifiers
  def convert_keywords_to_identifiers(self, operators):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'keyword' and\
        prev_token.group == 'operator' and prev_token.text in operators:
        token.group = 'identifier'
        token.is_operand = True

      if token.group not in ['whitespace', 'comment', 'newline']:
        prev_token = token


  # convert identifiers after 'goto' to labels
  def convert_identifiers_to_labels(self):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'identifier' and \
        prev_token.group == 'keyword' and prev_token.text.lower() == 'goto':
        token.group = 'label'
        token.is_operand = False

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


    line_length_confidence = 0.0
    if len(lines) > 0:
      line_length_confidence = num_ok_lines / len(lines)

    self.confidences['line_length'] = line_length_confidence


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

    token_confidence = 0.0

    if len(self.tokens) > 0:
      num_known_tokens = len(self.tokens) - num_invalid_tokens
      token_confidence = num_known_tokens / len(self.tokens)

    self.confidences['token'] = token_confidence


  # repeated tokens reduce confidence
  def calc_token_2_confidence(self, allowed_tokens = []):
    num_repeated_tokens = 0
    prev_token = Token('\n', 'newline', False)

    allowed_groups = ['invalid', 'whitespace', 'newline', 'comment', 'group']
    for token in self.tokens:
      if token.group not in allowed_groups and token.text not in allowed_tokens:
        if token.group == prev_token.group and token.text == prev_token.text:
          num_repeated_tokens += 1

          self.errors.append({
            'TYPE': 'TOKEN',
            'REPEATED': token.text,
            'FIRST': '',
            'SECOND': ''
          })

      prev_token = token

    repeat_confidence = 1.0

    if len(self.tokens) > 0:
      repeat_unconfidence = num_repeated_tokens / len(self.tokens)
      repeat_confidence = 1.0 - repeat_unconfidence

    self.confidences['repeated token'] = repeat_confidence


  #  unknown operators reduce confidence
  def calc_operator_confidence(self):
    invalid_operators = Examiner.find_tokens(self.tokens, ['invalid operator'])
    num_invalid_operators = Examiner.count_tokens(self.tokens, ['invalid operator'])
    num_known_operators = Examiner.count_tokens(self.tokens, ['operator'])
    num_operators = num_known_operators + num_invalid_operators

    for operator in invalid_operators:
      self.errors.append({
        'TYPE': 'OPERATOR',
        'INVALID': operator.text,
        'FIRST': '',
        'SECOND': ''
      })

    operator_confidence = 0.0

    if num_operators > 0:
      operator_confidence = num_known_operators / num_operators

    self.confidences['operator'] = operator_confidence


  # binary operators that follow operators reduce confidence
  def calc_operator_2_confidence(self, tokens, allow_pairs):
    num_invalid_operators = Examiner.count_tokens(tokens, ['invalid operator'])
    num_known_operators = Examiner.count_tokens(tokens, ['operator'])
    num_operators = num_known_operators + num_invalid_operators

    operator_confidence_2 = 0.0

    if num_operators > 0:
      errors = 0
      prev_token = Token('\n', 'newline', False)

      lower_unary_operators = []
      for op in self.unary_operators:
        lower_unary_operators.append(op.lower())

      for token in tokens:
        if token.group == 'operator' and \
          prev_token.group == 'operator' and \
          prev_token.text not in self.adjective_operators and \
          prev_token.text not in self.postfix_operators and \
          token.text.lower() not in lower_unary_operators and \
          [prev_token.text, token.text] not in allow_pairs:
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
  def calc_operator_3_confidence(self, tokens, group_ends, allow_pairs):
    num_invalid_operators = Examiner.count_tokens(tokens, ['invalid operator'])
    num_known_operators = Examiner.count_tokens(tokens, ['operator'])
    num_operators = num_known_operators + num_invalid_operators

    operator_confidence_3 = 0.0

    if num_operators > 0:
      errors = 0
      prev_token = Token('\n', 'newline', False)

      lower_unary_operators = []
      for op in self.unary_operators:
        lower_unary_operators.append(op.lower())

      for token in tokens:
        prev_token_operand = prev_token.is_operand or \
          prev_token.text in group_ends or \
          prev_token.text.lower() == 'end'
  
        if token.group == 'operator' and \
          not prev_token_operand and \
          prev_token.text not in self.adjective_operators and \
          prev_token.text not in self.postfix_operators and \
          token.text not in self.keyword_postfix and \
          token.text.lower() not in lower_unary_operators and \
          [prev_token.text, token.text] not in allow_pairs:
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
  def calc_operator_4_confidence(self, tokens, group_starts, allow_pairs):
    num_invalid_operators = Examiner.count_tokens(tokens, ['invalid operator'])
    num_known_operators = Examiner.count_tokens(tokens, ['operator'])
    num_operators = num_known_operators + num_invalid_operators

    operator_confidence_4 = 0.0

    if num_operators > 0:
      errors = 0
      prev_token = Token('\n', 'newline', False)

      lower_unary_operators = []
      for op in self.unary_operators:
        lower_unary_operators.append(op.lower())

      for token in tokens:
        prev_token_postfix_operator = prev_token.text.lower() in (op.lower() for op in self.postfix_operators)
  
        if prev_token.group == 'operator' and \
          not prev_token_postfix_operator and \
          not token.is_operand and \
          token.text.lower() not in lower_unary_operators and \
          token.text not in group_starts and \
          [prev_token.text, token.text] not in allow_pairs:
          errors += 1
          self.errors.append({
            'TYPE': 'OPERATOR4',
            'FIRST': prev_token.text,
            'SECOND': token.text
            })

        prev_token = token

      operator_confidence_4 = 1.0 - errors / num_operators

    self.confidences['operator_4'] = operator_confidence_4


  # groups that follow groups reduce confidence
  def calc_group_confidence(self, tokens, groups):
    num_groups = self.count_my_tokens(['group'])

    group_confidence = 1.0

    if num_groups > 0:
      errors = 0
      prev_token = Token('\n', 'newline', False)

      for token in tokens:
        if token.group == 'group' and token.text in groups and \
          prev_token.group == 'group' and prev_token.text in groups:
          errors += 1
          self.errors.append({
            'TYPE': 'GROUP',
            'FIRST': prev_token.text,
            'SECOND': token.text
            })

        prev_token = token

      group_confidence = 1.0 - errors / num_groups

    self.confidences['group'] = group_confidence


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
      Token('', 'newline', False),
      Token('', 'newline', False),
      Token('', 'newline', False)
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
          unwrapped_tokens.append(Token(' ', 'whitespace', False))
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
        new_token = Token(new_token.text + token.text, type1, token.is_operand)
      else:
        if new_token is not None:
          new_list.append(new_token)
        new_token = token
    if new_token is not None:
      new_list.append(new_token)

    return new_list


  # operands in a row decreases confidence
  def calc_operand_n_confidence(self, tokens, operand_types, max_count):
    confidence_name_u = 'OPERAND_' + str(max_count)
    confidence_name_l = 'operand_' + str(max_count)
    n_operand_count = 0
    consec_count = 0
    prev_token = Token('\n', 'newline', False)
    for token in tokens:
      if token.group in operand_types:
        consec_count += 1
        if consec_count > max_count:
          n_operand_count += 1
          self.errors.append({
            'TYPE': confidence_name_u,
            'FIRST': prev_token.text,
            'SECOND': token.text
            })
      else:
        consec_count = 0

      prev_token = token

    operand_confidence = 1.0

    if len(tokens) > 0:
      operand_confidence = 1.0 - (n_operand_count / len(tokens))

    self.confidences[confidence_name_l] = operand_confidence


  # two values in a row decreases confidence
  def calc_value_value_different_confidence(self, tokens):
    # remove tokens we don't care about
    drop_types = ['whitespace', 'comment', 'line continuation']
    tokens = Examiner.drop_tokens(self.tokens, drop_types)

    value_types = ['number', 'string', 'symbol']

    two_value_count = 0
    prev_token = Token('\n', 'newline', False)
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
    groups = ['keyword', 'type', 'function']
    num_keywords = self.count_my_tokens(groups)

    if num_keywords > 0:
      self.confidences['keyword'] = 1.0
    else:
      # if no keywords, compute based on tokens
      self.errors.append({
        'TYPE': 'KEYWORDS',
        'MESSAGE': 'No keywords found'
        })

      num_tokens = len(self.tokens)
      invalid_groups = ['invalid', 'invalid operator']
      num_invalid_tokens = Examiner.count_tokens(self.tokens, invalid_groups)
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

    prev_token = Token('\n', 'newline', False)

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

    prev_token = Token('\n', 'newline', False)

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


  def tokenize_asm_code(self, code, tab_size, tokenizer, opcode_extras, label_leads, label_mids, label_ends, comment_leads):
    lines = code.split('\n')

    tokens = []
    indents = []

    for line in lines:
      newline = '\n'
      if len(line) > 0 and line[-1] == '\r':
        newline = '\r\n'
      line = line.rstrip('\r')
      line = line.rstrip()
      line = Tokenizer.tabs_to_spaces(line, tab_size)

      # get tokens and indents
      line_tokens, line_indents = self.tokenize_asm_line(line, tokenizer, opcode_extras, label_leads, label_mids, label_ends, comment_leads)
      tokens += line_tokens
      indents.append(line_indents)

      tokens.append(Token(newline, 'newline', False))

    # return tokens and indents
    return tokens, indents


  def tokenize_asm_line(self, line, tokenizer, opcode_extras, label_leads, label_mids, label_ends, comment_leads):
    # break apart the line based on format
    # The line format is:
    # label
    # opcode
    # arguments
    # comment

    START_LABEL_OR_WHITESPACE = 1
    AFTER_LABEL = 2
    IN_LINE_COMMENT = 3
    IN_LABEL = 4
    LABEL_OPCODE_WHITESPACE = 5
    IN_OPCODE = 6
    OPCDE_ARGS_WHITESPACE = 7
    IN_ARGS = 8
    ARGS_COMMENT_WHITESPACE = 9
    IN_COMMENT = 10

    label = ''
    label_indent = None
    lo_space = ''
    opcode = ''
    opcode_indent = None
    oa_space = ''
    args = ''
    args_indent = None
    ac_space = ''
    comment = ''
    comment_indent = None
    line_comment = ''
    line_comment_indent = None
    quotes = ['"', "'", '`']
    in_quote = False
    quote_char = None
    parens_level = 0
    state = START_LABEL_OR_WHITESPACE
    column = 0
    for c in line:
      # start (label or label-opcode whitespace)
      if state == START_LABEL_OR_WHITESPACE:
        if c in comment_leads:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace():
          # state change to label-opcode whitespace
          lo_space = c
          state = LABEL_OPCODE_WHITESPACE
        else:
          # state change to in label
          label = c
          label_indent = column
          state = IN_LABEL
      # in line_comment
      elif state == IN_LINE_COMMENT:
        line_comment += c
      # in label
      elif state == IN_LABEL:
        if c in comment_leads:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace():
          # state change to in label-opcode whitespace
          lo_space = c
          state = LABEL_OPCODE_WHITESPACE
        elif c in label_ends:
          label += c
          state = AFTER_LABEL
        else:
          label += c
      # after label and first character of whitespace or opcode
      elif state == AFTER_LABEL:
        if c.isspace():
          lo_space = c
          state = LABEL_OPCODE_WHITESPACE
        else:
          opcode = c
          state = IN_OPCODE
      # in label-opcode whitespace
      elif state == LABEL_OPCODE_WHITESPACE:
        if c in comment_leads:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace():
          lo_space += c
        else:
          # state change to in opcode
          opcode = c
          opcode_indent = column
          state = IN_OPCODE
      # in opcode
      elif state == IN_OPCODE:
        if c in comment_leads:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace():
          # state change to in op-args whitespace
          oa_space = c
          state = OPCDE_ARGS_WHITESPACE
        else:
          opcode += c
      # in op-args whitespace
      elif state == OPCDE_ARGS_WHITESPACE:
        if c in comment_leads:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace():
          oa_space += c
        else:
          # state change to in args
          args = c
          args_indent = column
          state = IN_ARGS
          if c in quotes:
            in_quote = True
            quote_char = c
          if c == '(':
            parens_level = 1
      # in args
      elif state == IN_ARGS:
        if c in comment_leads and not in_quote and parens_level <= 0:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace() and not in_quote and parens_level <= 0:
          # state change to in args-comment whitespace
          ac_space = c
          state = ARGS_COMMENT_WHITESPACE
        else:
          args += c
          if in_quote:
            if c == quote_char:
              in_quote = False
              quote_char = None
          else:
            if c in quotes:
              in_quote = True
              quote_char = c
            if c == '(':
              parens_level += 1
            if c == ')':
              parens_level -= 1
      # in args-comment whitespace
      elif state == ARGS_COMMENT_WHITESPACE:
        if c in comment_leads:
          line_comment = c
          line_comment_indent = column
          state = IN_LINE_COMMENT
        elif c.isspace():
          ac_space += c
        else:
          # state change to in comment
          comment = c
          comment_indent = column
          state = IN_COMMENT
      # in comment
      elif state == IN_COMMENT:
        comment += c

      column += 1

    indents = {
      'label': label_indent,
      'opcode': opcode_indent,
      'args': args_indent,
      'comment': comment_indent,
      'line_comment': line_comment_indent
    }

    # tokenize the code
    tokens = []

    if len(label) > 0:
      if Tokenizer.check_label_format(label, label_leads, label_mids, label_ends):
        tokens.append(Token(label, 'label', False))
      else:
        tokens.append(Token(label, 'invalid', False))

    if len(lo_space) > 0:
      tokens.append(Token(lo_space, 'whitespace', False))

    if len(opcode) > 0:
      if Tokenizer.check_opcode_format(opcode, opcode_extras):
        tokens.append(Token(opcode, 'opcode', False))
      else:
        tokens.append(Token(opcode, 'invalid', False))

    if len(oa_space) > 0:
      tokens.append(Token(oa_space, 'whitespace', False))

    if len(args) > 0:
      tokens += tokenizer.tokenize(args)

    if len(ac_space) > 0:
      tokens.append(Token(ac_space, 'whitespace', False))

    if len(comment) > 0:
      tokens.append(Token(comment, 'comment', False))

    if len(line_comment) > 0:
      tokens.append(Token(line_comment, 'comment', False))

    return tokens, indents
