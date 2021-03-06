# token class
class AbstractElement
  attr_reader :precedence

  def initialize
    @operator = false
    @function = false
    @variable = false
    @operand = false
    @terminal = false
    @group_start = false
    @group_end = false
    @param_start = false
    @separator = false
    @file_handle = false
    @precedence = 10
    @scalar = false
    @array = false
    @matrix = false
  end

  def operator?
    @operator
  end

  def function?
    @function
  end

  def variable?
    @variable
  end

  def function_variable?
    function? || variable?
  end

  def operand?
    @operand
  end

  def terminal?
    @terminal
  end

  def group_start?
    @group_start
  end

  def group_end?
    @group_end
  end

  def param_start?
    @param_start
  end

  def starter?
    @group_start || @param_start
  end

  def separator?
    @separator
  end

  def group_separator?
    group_start? || group_end? || separator?
  end

  def previous_is_array(stack)
    !stack.empty? && stack[-1].class.to_s == 'Array'
  end

  def file_handle?
    @file_handle
  end

  def scalar?
    @scalar
  end

  def array?
    @array
  end

  def matrix?
    @matrix
  end

  protected

  def make_coord(c)
    [NumericConstant.new(c)]
  end

  def make_coords(r, c)
    [NumericConstant.new(r), NumericConstant.new(c)]
  end
end

# beginning of a group
class GroupStart < AbstractElement
  def self.accept?(token)
    classes = %w(GroupStartToken)
    classes.include?(token.class.to_s)
  end

  attr_reader :text

  def initialize(element)
    super()
    @text = element.to_s
    @group_start = true
  end

  def to_s
    @text
  end
end

# end of a group
class GroupEnd < AbstractElement
  def self.accept?(token)
    classes = %w(GroupEndToken)
    classes.include?(token.class.to_s)
  end

  def initialize(element)
    super()
    @text = element.to_s
    @group_end = true
    @operand = true
  end

  def match?(start_element)
    (start_element.text == '(' && @text == ')') ||
      (start_element.text == '[' && @text == ']')
  end

  def to_s
    @text
  end
end

# beginning of a set of parameters
class ParamStart < AbstractElement
  attr_reader :text

  def initialize(element)
    super()
    @text = element.to_s
    @param_start = true
  end

  def to_s
    @text
  end
end

# separator for group or params
class ParamSeparator < AbstractElement
  def self.accept?(token)
    classes = %w(ParamSeparatorToken)
    classes.include?(token.class.to_s)
  end

  def initialize(token)
    super()
    @text = token.to_s
    @separator = true
  end

  def to_s
    @text
  end
end

public

# class that holds a value
class AbstractValueElement < AbstractElement
  def initialize
    super
    @numeric_constant = false
    @text_constant = false
    @boolean_constant = false
    @scalar = true
    @value = nil
  end

  def numeric_constant?
    @numeric_constant
  end

  def text_constant?
    @text_constant
  end

  def boolean_constant?
    @boolean_constant
  end

  def eql?(other)
    @value == other.to_v
  end

  def hash
    @value.hash
  end

  def ==(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    @value == other.to_v
  end

  def >(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    @value > other.to_v
  end

  def >=(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    @value >= other.to_v
  end

  def <(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    @value < other.to_v
  end

  def <=(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    @value <= other.to_v
  end

  def b_eq(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    BooleanConstant.new(@value == other.to_v)
  end

  def b_ne(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    BooleanConstant.new(@value != other.to_v)
  end

  def b_gt(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    BooleanConstant.new(@value > other.to_v)
  end

  def b_ge(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    BooleanConstant.new(@value >= other.to_v)
  end

  def b_lt(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    BooleanConstant.new(@value < other.to_v)
  end

  def b_le(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    BooleanConstant.new(@value <= other.to_v)
  end

  def b_and(_)
    raise(BASICException, 'Invalid operator AND')
  end

  def b_or(_)
    raise(BASICException, 'Invalid operator OR')
  end

  def +(_)
    raise(BASICException, 'Invalid operator +')
  end

  def -(_)
    raise(BASICException, 'Invalid operator -')
  end

  def add(_)
    raise(BASICException, 'Invalid operator add')
  end

  def subtract(_)
    raise(BASICException, 'Invalid operator subtract')
  end

  def multiply(_)
    raise(BASICException, 'Invalid operator multiply')
  end

  def divide(_)
    raise(BASICException, 'Invalid operator divide')
  end

  def power(_)
    raise(BASICException, 'Invalid operator power')
  end

  def max(_)
    raise(BASICException, 'Invalid operator MAX')
  end

  def min(_)
    raise(BASICException, 'Invalid operator MIN')
  end

  def printable?
    true
  end

  def evaluate(_, _, _)
    self
  end

  def to_v
    @value
  end

  private

  def compatible?(other)
    self.class.to_s == other.class.to_s
  end
end

# Numeric constants
class NumericConstant < AbstractValueElement
  def self.accept?(token)
    classes = %w(Fixnum Bignum Float NumericConstantToken)
    classes.include?(token.class.to_s)
  end

  def self.init?(text)
    numeric_classes = %w(Fixnum Bignum Float)
    numeric_classes.include?(text.class.to_s) ||
      (text.class.to_s == 'String' && text == 'PI') ||
      (text.size == 2 && text[0]) == '#' ||
      !numeric(text).nil?
  end

  def self.numeric(text)
    # PI
    if text == 'PI'
      3.1415926
    # #c constant
    elsif text.size == 2 && text[0] == '#'
      text[1].ord
    # nnn(E+nn)
    elsif /\A\s*[+-]?\d+(E+?\d+)?\z/ =~ text
      text.to_f.to_i
    # nnn(E-nn)
    elsif /\A\s*[+-]?\d+(E-\d+)?\z/ =~ text
      text.to_f
    # nnn.(nnn)(E+-nn)
    elsif /\A\s*[+-]?\d+\.(\d*)?(E[+-]?\d+)?\z/ =~ text
      text.to_f
    # (nnn).nnn(E+-nn)
    elsif /\A\s*[+-]?(\d+)?\.\d*(E[+-]?\d+)?\z/ =~ text
      text.to_f
    end
  end

  private

  def float_to_possible_int(f)
    i = f.to_i
    frac = f - i
    frac.zero? || (!i.zero? && frac.abs < 1e-7) ? i : f
  end

  public

  attr_reader :token_chars

  def initialize(text)
    super()
    numeric_classes = %w(Fixnum Bignum Float)
    f = text if numeric_classes.include?(text.class.to_s)
    if text.class.to_s == 'NumericConstantToken'
      t = text.to_s
      if !t.empty? && t[0] == '#'
        f = t[1].ord
      else
        f = text.to_f
      end
    end
    @token_chars = text.to_s
    @value = float_to_possible_int(f)
    @value = 3.1415926 if text == 'PI'
    @operand = true
    @precedence = 0
    @numeric_constant = true
    raise BASICException, "'#{text}' is not a number" if @value.nil?
  end

  def +(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    NumericConstant.new(@value + other.to_v)
  end

  def -(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    NumericConstant.new(@value - other.to_v)
  end

  def *(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    NumericConstant.new(@value * other.to_v)
  end

  def add(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    NumericConstant.new(@value + other.to_v)
  end

  def subtract(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    NumericConstant.new(@value - other.to_v)
  end

  def multiply(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    NumericConstant.new(@value * other.to_v)
  end

  def divide(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    raise(BASICException, 'Divide by zero') if other == NumericConstant.new(0)
    NumericConstant.new(@value.to_f / other.to_v.to_f)
  end

  def power(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    NumericConstant.new(@value**other.to_v)
  end

  def maximum(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    NumericConstant.new([@value, other.to_v].max)
  end

  def minimum(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    NumericConstant.new([@value, other.to_v].min)
  end

  def truncate
    NumericConstant.new(@value.to_i)
  end

  def floor
    NumericConstant.new(@value.floor)
  end

  def exp
    NumericConstant.new(Math.exp(@value))
  end

  def log
    NumericConstant.new(@value > 0 ? Math.log(@value) : 0)
  end

  def abs
    NumericConstant.new(@value >= 0 ? @value : -@value)
  end

  def sqrt
    NumericConstant.new(@value > 0 ? Math.sqrt(@value) : 0)
  end

  def sin
    NumericConstant.new(Math.sin(@value))
  end

  def cos
    NumericConstant.new(Math.cos(@value))
  end

  def tan
    NumericConstant.new(@value >= 0 ? Math.tan(@value) : 0)
  end

  def atn
    NumericConstant.new(Math.atan(@value))
  end

  def max(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    @value = [to_v, other.to_v].max
  end

  def min(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    @value = [to_v, other.to_v].min
  end

  def sign
    result = 0
    result = 1 if @value > 0
    result = -1 if @value < 0
    NumericConstant.new(result)
  end

  def to_i
    @value.to_i
  end

  def to_f
    @value.to_f
  end

  def to_s
    @value.to_s
  end

  def to_b
    !@value.to_f.zero?
  end

  def six_digits(value)
    # ensure only 6 digits of precision
    decimals = 5 - (value != 0 ? Math.log(value.abs, 10).to_i : 0)
    rounded = value.round(decimals)
    rounded.to_f
  end

  def print(printer)
    s = to_formatted_s
    s = s.upcase
    printer.print_item s
    printer.last_was_numeric
  end

  def write(printer)
    s = to_formatted_s
    s = s.upcase
    printer.print_item s
  end

  private

  def to_formatted_s
    lead_space = @value >= 0 ? ' ' : ''
    digits = six_digits(@value).to_s
    # remove trailing zeros and decimal point
    digits = digits.sub(/0+\z/, '').sub(/\.\z/, '') if
      digits.include?('.') && !digits.include?('e')
    lead_space + digits
  end

  def compatible?(other)
    other.numeric_constant?
  end
end

# Integer constants
class IntegerConstant < AbstractValueElement
  def self.accept?(token)
    classes = %w(Fixnum Bignum Float IntegerConstantToken)
    classes.include?(token.class.to_s)
  end

  def self.init?(text)
    numeric_classes = %w(Fixnum Bignum Float)
    numeric_classes.include?(text.class.to_s) || !numeric(text).nil?
  end

  def self.numeric(text)
    # nnn%
    if /\A\s*[+-]?\d+%\z/ =~ text
      text.to_f.to_i
    end
  end

  attr_reader :token_chars

  def initialize(text)
    super()
    numeric_classes = %w(Fixnum Bignum Float)
    f = text.to_i if numeric_classes.include?(text.class.to_s)
    f = text.to_i if text.class.to_s == 'NumericConstantToken'
    f = text.to_f.to_i if text.class.to_s == 'IntegerConstantToken'
    @token_chars = text.to_s
    @value = f
    @operand = true
    @precedence = 0
    @numeric_constant = true
    raise BASICException, "'#{text}' is not a number" if @value.nil?
  end

  def +(other)
    IntegerConstant.new(@value + other.to_v)
  end

  def -(other)
    IntegerConstant.new(@value - other.to_v)
  end

  def *(other)
    IntegerConstant.new(@value * other.to_v)
  end

  def add(other)
    IntegerConstant.new(@value + other.to_v)
  end

  def subtract(other)
    IntegerConstant.new(@value - other.to_v)
  end

  def multiply(other)
    IntegerConstant.new(@value * other.to_v)
  end

  def divide(other)
    raise(BASICException, 'Divide by zero') if other == IntegerConstant.new(0)
    IntegerConstant.new(@value.to_f / other.to_v.to_f)
  end

  def power(other)
    IntegerConstant.new(@value**other.to_v)
  end

  def maximum(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    IntegerConstant.new([@value, other.to_v].max)
  end

  def minimum(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    IntegerConstant.new([@value, other.to_v].min)
  end

  def truncate
    IntegerConstant.new(@value.to_i)
  end

  def floor
    IntegerConstant.new(@value.floor)
  end

  def exp
    IntegerConstant.new(Math.exp(@value))
  end

  def log
    IntegerConstant.new(@value > 0 ? Math.log(@value) : 0)
  end

  def abs
    IntegerConstant.new(@value >= 0 ? @value : -@value)
  end

  def sqrt
    IntegerConstant.new(@value > 0 ? Math.sqrt(@value) : 0)
  end

  def sin
    IntegerConstant.new(Math.sin(@value))
  end

  def cos
    IntegerConstant.new(Math.cos(@value))
  end

  def tan
    IntegerConstant.new(@value >= 0 ? Math.tan(@value) : 0)
  end

  def atn
    IntegerConstant.new(Math.atan(@value))
  end

  def max(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    @value = [to_v, other.to_v].max
  end

  def min(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    @value = [to_v, other.to_v].min
  end

  def sign
    result = 0
    result = 1 if @value > 0
    result = -1 if @value < 0
    IntegerConstant.new(result)
  end

  def to_i
    @value.to_i
  end

  def to_f
    @value.to_i
  end

  def to_s
    @value.to_s
  end

  def to_b
    !@value.to_i.zero?
  end

  def print(printer)
    s = to_formatted_s
    s = s.upcase
    printer.print_item s
    printer.last_was_numeric
  end

  def write(printer)
    s = to_formatted_s
    s = s.upcase
    printer.print_item s
  end

  private

  def to_formatted_s
    lead_space = @value >= 0 ? ' ' : ''
    digits = @value.to_s
    lead_space + digits
  end

  def compatible?(other)
    other.numeric_constant?
  end
end

# Text constants
class TextConstant < AbstractValueElement
  def self.accept?(token)
    classes = %w(TextConstantToken)
    classes.include?(token.class.to_s)
  end

  def self.init?(text)
    /\A".*"\z/.match(text)
  end

  attr_reader :value

  def initialize(text)
    super()
    @value = nil
    @value = text.value if text.class.to_s == 'TextConstantToken'
    raise(BASICException, "'#{text}' is not a text constant") if @value.nil?
    @operand = true
    @precedence = 0
    @text_constant = true
  end

  def +(other)
    unquoted = @value + other.to_v
    quoted = '"' + unquoted + '"'
    token = TextConstantToken.new(quoted)
    TextConstant.new(token)
  end

  def add(other)
    unquoted = @value + other.to_v
    quoted = '"' + unquoted + '"'
    token = TextConstantToken.new(quoted)
    TextConstant.new(token)
  end

  def maximum(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    TextConstant.new([@value, other.to_v].max)
  end

  def minimum(other)
    raise(BASICException, 'Type mismatch') unless compatible?(other)
    TextConstant.new([@value, other.to_v].min)
  end

  def to_s
    "\"#{@value}\""
  end

  def to_b
    !@value.size.zero?
  end

  def print(printer)
    printer.print_item @value
  end

  def write(printer)
    v = to_s
    printer.print_item v
  end

  def unpack
    length = NumericConstant.new(@value.size)
    dims = [length]
    values = {}
    values[[NumericConstant.new(0)]] = length
    index = 1
    @value.each_char do |char|
      key = [NumericConstant.new(index)]
      values[key] = NumericConstant.new(char.ord)
      index += 1
    end
    BASICArray.new(dims, values)
  end
end

# Boolean constants
class BooleanConstant < AbstractValueElement
  def self.accept?(token)
    classes = %w(BooleanConstantToken)
    classes.include?(token.class.to_s)
  end

  def self.init?(text)
    %w(TRUE FALSE).include?(text)
  end

  attr_reader :value

  def initialize(obj)
    super()
    obj_class = obj.class.to_s
    @value =
      (obj_class == 'BooleanConstantToken' && obj.to_s == 'TRUE') ||
      (obj_class == 'String' && obj.casecmp('TRUE').zero?) ||
      (obj_class == 'NumericConstant' && !obj.to_f.zero?) ||
      (obj_class == 'IntegerConstant' && !obj.to_i.zero?) ||
      (obj_class == 'TextConstant' && !obj.value.strip.size.zero?) ||
      obj_class == 'TrueClass'
    @operand = true
    @precedence = 0
    @boolean_constant = true
  end

  def b_and(other)
    BooleanConstant.new(@value && other.to_v)
  end

  def b_or(other)
    BooleanConstant.new(@value || other.to_v)
  end

  def to_s
    @value ? 'true' : 'false'
  end

  def to_b
    @value
  end
end

# File handle class
class FileHandle < AbstractElement
  attr_reader :number

  def initialize(num)
    super()
    raise(BASICException, 'Invalid file reference') unless
      num.class.to_s == 'Fixnum'
    raise(BASICException, 'Invalid file number') if num < 0
    @number = num
    @file_handle = true
  end

  def hash
    number.hash
  end

  def eql?(other)
    number == other.number
  end

  def to_s
    '#' + @number.to_s
  end
end

# Carriage control for PRINT and MAT PRINT statements
class CarriageControl
  def self.accept?(token)
    classes = %w(ParamSeparatorToken)
    classes.include?(token.class.to_s)
  end

  def self.init?(token)
    text = token.to_s
    ['NL', ',', ';', ''].include?(text)
  end

  def initialize(token)
    raise(BASICException, "'#{token}' is not a valid separator") unless
      CarriageControl.init?(token)
    text = token.to_s
    @operator = text
  end

  def printable?
    false
  end

  def to_s
    case @operator
    when ';'
      '; '
    when ','
      ', '
    when 'NL'
      ''
    when ''
      ' '
    end
  end

  def print(printer, _)
    case @operator
    when ','
      printer.tab
    when ';'
      printer.semicolon
    when 'NL'
      printer.newline
    when ''
      printer.implied
    end
  end

  def write(printer, _)
    case @operator
    when ','
      printer.print_item(@operator)
    when ';'
      printer.print_item(@operator)
    when 'NL'
      printer.newline
    when ''
      printer.print_item(',')
    end
  end
end

# Hold a variable name (not a reference or value)
class VariableName < AbstractElement
  def self.accept?(token)
    classes = %w(VariableToken)
    classes.include?(token.class.to_s)
  end

  def self.init?(text)
    /\A[A-Z]\d?\$?\z/.match(text)
  end

  attr_reader :name
  attr_reader :content_type

  def initialize(token)
    super()
    raise(BASICException, "'#{token}' is not a variable name") unless
      token.class.to_s == 'VariableToken'
    @name = token
    @variable = true
    @operand = true
    @precedence = 7
    @content_type = @name.content_type
  end

  def eql?(other)
    to_s == other.to_s
  end

  def ==(other)
    to_s == other.to_s
  end

  def hash
    @name.hash
  end

  def compatible?(value)
    compatible = false

    numerics = %w(NumericConstant IntegerConstant)
    strings = %w(TextConstant)

    if @content_type == 'NumericConstant'
      compatible = numerics.include?(value.class.to_s)
    end
    if @content_type == 'IntegerConstant'
      compatible = numerics.include?(value.class.to_s)
    end
    if @content_type == 'TextConstant'
      compatible = strings.include?(value.class.to_s)
    end

    compatible
  end

  def to_s
    @name.to_s
  end
end

# Hold a variable (name with possible subscripts and value)
class Variable < AbstractElement
  attr_reader :subscripts

  def initialize(variable_name, subscripts = [])
    super()
    raise(BASICException, "'#{variable_name}' is not a variable name") if
      variable_name.class.to_s != 'VariableName'
    @variable_name = variable_name
    @subscripts = normalize_subscripts(subscripts)
    @variable = true
    @operand = true
    @precedence = 7
  end

  def name
    @variable_name
  end

  def content_type
    @variable_name.content_type
  end

  def compatible?(value)
    content_type = self.content_type
    compatible = false

    numerics = %w(NumericConstant IntegerConstant)
    strings = %w(TextConstant)

    if content_type == 'NumericConstant'
      compatible = numerics.include?(value.class.to_s)
    end
    if content_type == 'IntegerConstant'
      compatible = numerics.include?(value.class.to_s)
    end
    if content_type == 'TextConstant'
      compatible = strings.include?(value.class.to_s)
    end

    compatible
  end

  def to_s
    if subscripts.empty?
      @variable_name.to_s
    else
      @variable_name.to_s + '(' + @subscripts.join(',') + ')'
    end
  end

  private

  def normalize_subscripts(subscripts)
    raise(Exception, 'Invalid subscripts container') unless
      subscripts.class.to_s == 'Array'
    int_subscripts = []
    subscripts.each do |subscript|
      raise(Excaption, "Invalid subscript #{subscript}") unless
        subscript.numeric_constant?
      int_subscripts << subscript.truncate
    end
    int_subscripts
  end
end

# A list (needed because it has precedence value)
class List < AbstractElement
  def initialize(parsed_expressions)
    super()
    @parsed_expressions = parsed_expressions
    @variable = true
    @precedence = 7
  end

  def list
    @parsed_expressions
  end

  def evaluate(interpreter, _, trace)
    interpreter.evaluate(@parsed_expressions, trace)
  end

  def to_s
    "[#{@parsed_expressions.join('] [')}]"
  end
end
