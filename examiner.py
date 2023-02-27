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
    self.max_expected_line = 132
    self.no_keyword_confidence  = 0.7


# Convert some UNICODE characters to ASCII equivalents
  @staticmethod
  def convert_to_ascii(code):
    convertables = {
      ' ': ' ',
      ' ': ' ',
      '­': ' ',
      '＝': '=',
      '＜': '<',
      '＞': '>',
      '꞉': ':',
      '．': '.',
      '，': ',',
      ';': ';',
      '；': ';',
      '': ';',
      '’': "'"
    }

    ascii_code = ""

    for c in code:
      if c in convertables:
        ascii_code += convertables[c]
      else:
        ascii_code += c

    return ascii_code


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


  # assume CTRL-Z is a EOF marker
  # remove CTRL-Z and all text after
  @staticmethod
  def TrimCtrlZText(code):
    length = len(code)

    ctrlz_char = ''
    ctrlz_position = 0
    for index in range(-1, -length, -1):
      if code[index] == ctrlz_char:
        ctrlz_position = index

    if ctrlz_position != 0:
      code = code[:ctrlz_position]

    return code


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
      if token.group == 'group' and token.text == '(' and \
        prev_token.group == 'identifier':
        prev_token.group = 'function'

      if token.group not in ['whitespace', 'comment', 'newline', 'line identification']:
        prev_token = token


  def convert_functions_to_common_functions(self, common_names):
    for token in self.tokens:
      if token.group == 'function' and token.text.lower() in common_names:
        token.group = 'common function'


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

      if token.group not in ['whitespace', 'comment', 'newline', 'line identification']:
        prev_token = token


  # convert keywords before parens to functions
  def convert_keywords_to_functions(self):
    prev_prev_token = Token('\n', 'newline', False)
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'group' and token.text == '(' and \
        prev_token.group == 'keyword' and \
        prev_prev_token.group != 'newline':
        prev_token.group = 'function'
        prev_token.is_operand = True

      if token.group not in ['whitespace', 'comment']:
        prev_prev_token = prev_token
        prev_token = token


  # convert identifiers to labels
  def convert_identifiers_before_colon_to_labels(self, starters):
    prev_prev_token = Token('\n', 'newline', False)
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if starters == '' or prev_prev_token.group == 'newline':
        in_starters = True
      else:
        in_starters = prev_prev_token.text in starters

      if token.group == 'group' and token.text == ':' and \
        prev_token.group == 'identifier' and \
        in_starters:
        prev_token.group = 'label'
        prev_token.is_operand = False

      if token.group not in ['whitespace', 'comment', 'line identification']:
        prev_prev_token = prev_token
        prev_token = token


  # convert identifiers after 'goto' to labels
  def convert_identifiers_after_goto_to_labels(self):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'identifier' and \
        prev_token.group == 'keyword' and prev_token.text.lower() == 'goto':
        token.group = 'label'
        token.is_operand = False

      if token.group not in ['whitespace', 'comment', 'newline', 'line identification']:
        prev_token = token


  # convert identifiers at start of line to labels
  def convert_asm_identifiers_to_labels(self):
    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if token.group == 'identifier' and prev_token.group == 'newline':
        token.group = 'label'

      prev_token = token


  # convert keywords after 'EQU' or '=' to operators
  def convert_asm_keywords_to_operators(self):
    seen_equ = False

    for token in self.tokens:
      if token.group == 'newline':
        seen_equ = False

      if token.group == 'keyword' and seen_equ:
        token.group = 'operator'
        token.is_operand = False

      if token.text in ['EQU', '=']:
        seen_equ = True


  # convert keywords after other tkeywords on line to identifiers
  def convert_asm_keywords_to_identifiers(self):
    seen_identifier = False

    for token in self.tokens:
      if token.group == 'newline':
        seen_identifier = False

      if token.group == 'keyword' and seen_identifier:
        token.group = 'identifier'
        token.is_operand = True

      if token.group in ['keyword', 'identifier']:
        seen_identifier = True


  # convert asterisks in expressions to operators
  @staticmethod
  def convert_values_to_operators(tokens, operators):
    prev_token = Token('\n', 'newline', False)

    for token in tokens:
      if token.group == 'value' and \
        prev_token.group in ['number', 'identifier', 'value'] and \
        token.text in operators:
        token.group = 'operator'

      if token.group not in ['whitespace']:
        prev_token = token

    return tokens


  def calc_line_length_confidence(self, code, width):
    num_ok_lines = 0
    lines = code.split('\n')

    if len(lines) == 1 and self.newlines_important == 'never':
      return

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
      if token.group == 'invalid':
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

    allowed_groups = ['invalid', 'whitespace', 'newline', 'comment', 'line identification', 'group']
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
  def calc_operator_confidence(self, num_operators):
    invalid_operators = Examiner.find_tokens(self.tokens, ['invalid operator'])
    num_known_operators = num_operators - len(invalid_operators)

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
  def calc_operator_2_confidence(self, tokens, num_operators, allow_pairs):
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
  def calc_operator_3_confidence(self, tokens, num_operators, group_ends, allow_pairs):
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
  def calc_operator_4_confidence(self, tokens, num_operators, group_starts, allow_pairs):
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


  # tokens after line  continuation reduce confidence
  def calc_line_continuation_confidence(self, tokens):
    num_line_cont = self.count_my_tokens(['line continuation'])

    if num_line_cont == 0:
      return

    line_continuation_confidence = 1.0
    errors = 0
    prev_token = Token('\n', 'newline', False)

    for token in tokens:
      if prev_token.group == 'line continuation' and \
        token.group != 'newline':
        errors += 1
        self.errors.append({
          'TYPE': 'LINE CONTINUATION',
          'FIRST': prev_token.text,
          'SECOND': token.text
          })

      prev_token = token

    reduction = errors * 0.05

    if reduction > 1.0:
      reduction = 1.0

    line_continuation_confidence = 1.0 - reduction

    self.confidences['line continuation'] = line_continuation_confidence


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

    if 'paired_blockers_match' in self.confidences:
      self.confidences['paired_blockers_match'] *= paired_blocker_confidence
    else:
      self.confidences['paired_blockers_match'] = paired_blocker_confidence


  def calc_line_format_confidence(self):
    self.confidences['line format'] = 1.0


  # groups that follow groups reduce confidence
  def calc_line_description_confidence(self):
    if 'line identification' not in self.statistics:
      return

    num_line_descrips = self.statistics['line identification']

    if 'valid line identification' not in self.statistics:
      return

    num_valid_line_descrips = self.statistics['valid line identification']

    if num_line_descrips > 0:
      line_des_confidence = num_valid_line_descrips / num_line_descrips

      # add this confidence only when there are some line identifications
      self.confidences['line_description'] = line_des_confidence


  def calc_confidences(self, operand_types, group_starts, group_mids, group_ends, indents):
    tokens = self.source_tokens()
    tokens = Examiner.join_all_lines(tokens)

    self.calc_token_confidence()
    self.calc_token_2_confidence()

    num_operators = self.count_my_tokens(['operator', 'invalid operator'])
    if num_operators > 0:
      self.calc_operator_confidence(num_operators)
      allow_pairs = []
      self.calc_operator_2_confidence(tokens, num_operators, allow_pairs)
      self.calc_operator_3_confidence(tokens, num_operators, group_ends, allow_pairs)
      self.calc_operator_4_confidence(tokens, num_operators, group_starts, allow_pairs)

    self.calc_group_confidence(tokens, group_mids)

    operand_types_2 = ['number', 'value']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    if indents is not None:
      self.calc_indent_confidence(indents)


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
    drop_types = ['whitespace', 'comment', 'line identification']
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
    drop_types = ['whitespace', 'comment', 'line identification', 'line continuation']
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
    groups = ['keyword', 'type', 'common function']
    num_keywords = self.count_my_tokens(groups)

    if num_keywords > 0:
      self.confidences['keyword'] = 1.0
    else:
      # if no keywords, compute based on tokens
      self.errors.append({
        'TYPE': 'KEYWORDS',
        'MESSAGE': 'No keywords found'
        })

      self.confidences['keyword'] = self.no_keyword_confidence


  # expecting some percentage of lines with separators
  def calc_statement_separator_confidence(self, tokens, groups, threshold):
    num_separators = self.count_tokens(tokens, groups)
    num_lines = self.count_source_lines()
    confidence = num_separators / num_lines

    if confidence > threshold:
      confidence = 1.0

    self.confidences['separator'] = confidence


  # preprocessor tokens followed by non-whitespace reduce confidence
  def calc_preprocessor_confidence(self):
    confidence = 1.0

    prev_token = Token('\n', 'newline', False)

    for token in self.tokens:
      if prev_token.group == 'preprocessor' and \
        token.group not in ['whitespace', 'newline']:
        confidence -= 0.01

      prev_token = token

    if confidence < 0.50:
      confidence = 0.50

    self.confidences['preprocessor'] = confidence


  def calc_indent_confidence(self, indents):
    opcode_indents = {}
    for indent_dict in indents:
      opcode = indent_dict['opcode']
      if opcode is not None:
        if opcode in opcode_indents:
          opcode_indents[opcode] += 1
        else:
          opcode_indents[opcode] = 1

    total = 0
    highest = 0

    for key in opcode_indents:
      indent = opcode_indents[key]
      total += indent
      if indent > highest:
        highest = indent

    if total > 0:
      confidence = highest / total
    else:
      confidence = 0.0
    
    self.confidences['opcode_indent'] = confidence


  def calc_line_ident_confidence(self):
    if 'line identification' in self.statistics:
      num_line_idents = self.statistics['line identification']
      num_valid_line_idents = self.statistics['valid line identification']
      confidence = num_valid_line_idents / num_line_idents
      self.confidences['line identification'] = confidence


  def count_source_lines(self):
    # count source lines
    # (lines with tokens other than space or comment or line number or line identification)
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

    # count line identifications
    if 'line identification' in self.statistics:
      num_valid_line_identifications = 0

      for token in self.tokens:
        if token.group == 'line identification':
          if ' ' in token.text.strip():
            self.errors.append({
              'TYPE': 'LINE DESCRIPTION',
              'FIRST': token.text,
              'SECOND': ''
              })
          else:
            num_valid_line_identifications += 1
    
      self.statistics['valid line identification'] = num_valid_line_identifications

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
