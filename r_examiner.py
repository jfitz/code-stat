from codestat_token import Token
from codestat_tokenizer import Tokenizer
from token_builders import (
  InvalidTokenBuilder,
  WhitespaceTokenBuilder,
  NewlineTokenBuilder,
  EscapedStringTokenBuilder,
  PrefixedRawStringTokenBuilder,
  IntegerTokenBuilder,
  IntegerExponentTokenBuilder,
  RealTokenBuilder,
  RealExponentTokenBuilder,
  IdentifierTokenBuilder,
  CaseInsensitiveListTokenBuilder,
  CaseSensitiveListTokenBuilder,
  SingleCharacterTokenBuilder,
  LeadToEndOfLineTokenBuilder
)
from r_token_builders import (
  ROperatorTokenBuilder
)
from examiner import Examiner

class RExaminer(Examiner):
  @staticmethod
  def __escape_z__():
    InvalidTokenBuilder.__escape_z__()
    WhitespaceTokenBuilder.__escape_z__()
    NewlineTokenBuilder.__escape_z__()
    EscapedStringTokenBuilder.__escape_z__()
    PrefixedRawStringTokenBuilder.__escape_z__()
    IntegerTokenBuilder.__escape_z__()
    IntegerExponentTokenBuilder.__escape_z__()
    RealTokenBuilder.__escape_z__()
    RealExponentTokenBuilder.__escape_z__()
    IdentifierTokenBuilder.__escape_z__()
    CaseInsensitiveListTokenBuilder.__escape_z__()
    CaseSensitiveListTokenBuilder.__escape_z__()
    SingleCharacterTokenBuilder.__escape_z__()
    LeadToEndOfLineTokenBuilder.__escape_z__()
    ROperatorTokenBuilder.__escape_z__()
    return 'Escape ?Z'


  def __init__(self, code):
    super().__init__()
    self.newlines_important = 'parens'

    operand_types = []

    whitespace_tb = WhitespaceTokenBuilder()
    newline_tb = NewlineTokenBuilder()

    integer_tb = IntegerTokenBuilder('_')
    integer_exponent_tb = IntegerExponentTokenBuilder('_')
    real_tb = RealTokenBuilder(False, False, '_')
    real_exponent_tb = RealExponentTokenBuilder(False, False, 'E', '_')
    operand_types.append('number')

    leads = '_'
    extras = '_'
    identifier_tb = IdentifierTokenBuilder(leads, extras)
    operand_types.append('identifier')

    quotes = ['"', "'", '`']
    string_tb = EscapedStringTokenBuilder(quotes, 10)
    raw_string_tb = PrefixedRawStringTokenBuilder('r', True, quotes)
    operand_types.append('string')

    hash_comment_tb = LeadToEndOfLineTokenBuilder('#', True, 'comment')

    known_operators = [
        '+', '-', '*', '/', '**', '^',
        '%%', '%/%', '%*%', '%in%',
        '<', '<=', '>', '>=',
        '==', '!=', '!', '|', '&', '||', '&&',
        '.', ':', '::', '[[', ']]', '@', '$',
        '=', '<-', '<<-', '->', '->>'
      ]

    self.unary_operators = [
      '+', '-',
      '!', '@', '.'
    ]

    known_operator_tb = CaseSensitiveListTokenBuilder(known_operators, 'operator', False)
    stmt_separator_tb = SingleCharacterTokenBuilder(';', 'statement separator', False)
    user_operator_tb = ROperatorTokenBuilder()

    groupers = ['(', ')', ',', '[', ']', '{', '}']
    group_starts = ['(', '[', ',', '{']
    group_mids = [',']
    group_ends = [')', ']', '}']

    groupers_tb = CaseInsensitiveListTokenBuilder(groupers, 'group', False)

    keywords = [
      'if', 'else', 'repeat', 'while',
      'function', 'for', 'in', 'next', 'break'
    ]

    keyword_tb = CaseSensitiveListTokenBuilder(keywords, 'keyword', False)

    values = [
      'TRUE', 'FALSE', 'NULL', 'Inf', 'NaN', 'NA',
      'NA_integer_', 'NA_real_', 'NA_complex_', 'NA_character_',
      '...'
    ]

    values_tb = CaseSensitiveListTokenBuilder(values, 'value', True)
    operand_types.append('value')

    known_functions = [
      'add_variable', 'add_constraint',
      'abline', 'abs', 'addmargins', 'addNA', 'aggregate', 'alist',
      'alt.equal', 'all', 'anti_join', 'any', 'apply', 'approx', 'approxfun',
      'apropos', 'arrange', 'as.data_frame', 'as.Date', 'as.double', 'as.factor',
      'as.function', 'as.name', 'as.ordered', 'as.single', 'attach', 'attr',
      'ave', 'axis',
      'barplot', 'beep', 'between', 'bind_cols', 'bind_rows',
      'body', 'box', 'boxplot',
      'call', 'c', 'case_when', 'casefold',
      'cat', 'cbind', 'ceiling', 'charmatch', 'chartr', 'chol', 'choose_files',
      'clip', 'coalesce', 'colMeans', 'colMedians', 'colnames', 'colSums',
      'colnames',
      'combine', 'combo', 'comment', 'cor', 'crossprod', 'cumall', 'cumany',
      'cum_dist', 'cummax', 'cummean', 'cumsum', 'cut',
      'cbind',
      'dim',
      'data.frame',
      'date', 'dbern', 'dbeta', 'dbinom', 'dcast', 'dcauchy', 'dchisq',
      'dense_rank', 'density', 'deparse', 'deriv', 'det', 'determinant',
      'dexp', 'df', 'dgamma', 'dgeom', 'dhyper', 'diag', 'diff', 'dim',
      'dimnames', 'dist', 'distinct', 'dlnorm', 'dlogis', 'dnbinom', 'dpois',
      'drop', 'droplevels', 'dsignrank', 'dt', 'dunif', 'duplicated',
      'dweibull', 'dwilcox',
      'eval', 'everything', 'exists', 'exp',
      'expression', 'extract',
      'filter', 'find', 'first', 'fivenum',
      'floor', 'format', 'formatC', 'full_join',
      'gather', 'gc', 'geom_bar',
      'get', 'gl', 'gregexp', 'grid',
      'get_row_duals', 'get_col_duals',
      'get_solution',
      'hasName', 'heatmap', 'hist',
      'identical', 'inner_join', 'integrate', 'intersect', 'is.nan', 'is.null',
      'isprime',
      'is.na',
      'list',
      'lag', 'lapply', 'last', 'last_col', 'layout', 'left_join',
      'library',
      'lapply',
      'list',
      'log', 'log10', 'log2',
      'mapply', 'match', 'merge',
      'matrix',
      'min', 'max',
      'mean',
      'median',
      'names', 'norm',
      'nrows', 'ncols'
      'objective_value',
      'order',
      'paste',
      'print',
      'range', 'rank', 'replace', 'replicate', 'rev',
      'rep',
      'rownames',
      'sample', 'sapply', 'scale', 'sd', 'solve', 'sort', 'split', 'sqrt',
      'sum_expr',
      'set_objective',
      'solver_status', 'solve_model',
      'sd',
      'sum',
      't'
    ]

    function_tb = CaseSensitiveListTokenBuilder(known_functions, 'common function', True)
    operand_types.append('common function')
    operand_types.append('function')

    invalid_token_builder = InvalidTokenBuilder()

    tokenbuilders = [
      newline_tb,
      whitespace_tb,
      stmt_separator_tb,
      integer_tb,
      integer_exponent_tb,
      real_tb,
      real_exponent_tb,
      keyword_tb,
      values_tb,
      user_operator_tb,
      known_operator_tb,
      groupers_tb,
      function_tb,
      identifier_tb,
      string_tb,
      raw_string_tb,
      hash_comment_tb,
      self.unknown_operator_tb,
      invalid_token_builder
    ]

    tokenizer = Tokenizer(tokenbuilders)

    code = self.TrimCtrlZText(code)
    ascii_code = self.convert_to_ascii(code)
    tokens = tokenizer.tokenize(ascii_code)

    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid operator')
    tokens = Examiner.combine_adjacent_identical_tokens(tokens, 'invalid')

    self.tokens = tokens
    self.convert_keywords_to_identifiers(['<-', '.', '='])
    self.convert_identifiers_to_functions()

    self.calc_statistics()

    tokens = self.source_tokens()
    tokens = Examiner.join_parens_continued_lines(tokens)
    tokens = Examiner.join_operator_continued_lines(tokens, self.postfix_operators)

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

    operand_types_2 = ['number', 'string', 'identifier', 'variable', 'symbol']
    self.calc_operand_n_confidence(tokens, operand_types_2, 2)
    self.calc_operand_n_confidence(tokens, operand_types, 4)

    self.calc_keyword_confidence()

    self.calc_line_length_confidence(code, self.max_expected_line)
