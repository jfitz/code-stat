# Common routines for readers
module Reader
  def ascii_printables(text)
    ascii_text = ''
    text.each_char do |c|
      ascii_text += c if c >= ' ' && c <= '~'
    end
    ascii_text
  end

  def make_tokenizers
    tokenizers = []
    tokenizers << InputTextTokenBuilder.new
    tokenizers << InputNumberTokenBuilder.new
    tokenizers << InputBareTextTokenBuilder.new
    tokenizers << ListTokenBuilder.new([',', ';'], ParamSeparatorToken)
    tokenizers << WhitespaceTokenBuilder.new
  end

  def verify_tokens(tokens)
    evens = tokens.values_at(* tokens.each_index.select(&:even?))
    evens.each do |token|
      raise(BASICException, 'Invalid input') unless
        token.numeric_constant? || token.text_constant?
    end

    odds = tokens.values_at(* tokens.each_index.select(&:odd?))
    odds.each do |token|
      raise(BASICException, 'Invalid input') unless token.separator?
    end
  end
end

# Common routines for input
module Inputter
  def input(interpreter)
    input_text = read_line

    tokenizers = make_tokenizers
    tokenizer = Tokenizer.new(tokenizers, nil)
    tokens = tokenizer.tokenize(input_text)

    # drop whitespace
    tokens.delete_if(&:whitespace?)

    # verify all even-index tokens are numeric or text
    # verify all odd-index tokens are separators
    verify_tokens(tokens)

    # convert from tokens to values
    expressions = ValueScalarExpression.new(tokens)
    expressions.evaluate(interpreter, false)
  end

  def line_input(interpreter)
    input_text = read_line
    quoted = '"' + input_text + '"'
    token = TextConstantToken.new(quoted)
    tokens = [token]
    # convert from tokens to values
    expressions = ValueScalarExpression.new(tokens)
    expressions.evaluate(interpreter, false)
  end
end

# Handle tab stops and carriage control
class ConsoleIo
  def initialize(max_width, zone_width, back_tab, print_rate, newline_rate,
                 implied_semicolon, default_prompt, qmark_after_prompt,
                 echo_input)
    @column = 0
    @max_width = max_width
    @zone_width = zone_width
    @back_tab = back_tab
    @print_rate = print_rate
    @newline_rate = newline_rate
    @implied_semicolon = implied_semicolon
    @default_prompt = default_prompt
    @echo_input = echo_input
    @qmark_after_prompt = qmark_after_prompt
    @last_was_numeric = false
  end

  include Reader
  include Inputter

  def read_char
    input_text = STDIN.getch
    ascii_text = ascii_printables(input_text)
    print(ascii_text) if @echo_input
    ascii_text
  end

  def read_line
    input_text = gets
    raise(BASICException, 'End of file') if input_text.nil?
    ascii_text = ascii_printables(input_text)
    puts(ascii_text) if @echo_input
    ascii_text
  end

  def prompt(text)
    if text.nil?
      print @default_prompt.value
    else
      print text.value
      print @default_prompt.value if @qmark_after_prompt
    end
  end

  def print_item(text)
    text.each_char do |c|
      print_out(c)
      incr = c == "\b" ? -1 : 1
      @column += incr
      @column = 0 if @column < 0
      newline if @max_width > 0 && @column >= @max_width
    end
    @last_was_numeric = false
  end

  def print_line(text)
    print_item(text)
    newline
  end

  def last_was_numeric
    @last_was_numeric = true
  end

  def tab
    space_after_numeric if @last_was_numeric
    if @zone_width > 0
      print_item(' ') while @column > 0 && @column % @zone_width != 0
    end
    @last_was_numeric = false
  end

  def semicolon
    if @last_was_numeric
      space_after_numeric
      print_item(' ') while @column % 3 != 0
    end
    @last_was_numeric = false
  end

  def implied
    semicolon if @implied_semicolon
    # nothing else otherwise
  end

  def columns_to_advance(new_column)
    @back_tab ? new_column - @column : [new_column - @column, 0].max
  end

  def trace_output(s)
    newline_when_needed
    print_out(s)
    newline
  end

  private

  def space_after_numeric
    count = 3
    while @column > 0 && count > 0
      print_item(' ')
      count -= 1
    end
  end

  public

  def newline
    puts
    newline_delay
    @column = 0
    @last_was_numeric = false
  end

  def newline_when_needed
    newline if @column > 0
  end

  def implied_newline
    @column = 0
  end

  def print_out(text)
    return if text.nil?

    text.each_char do |c|
      print(c)
      delay
    end
  end

  def delay
    sleep(1.0 / @print_rate) if @print_rate > 0
  end

  def newline_delay
    sleep(1.0 / @print_rate) if @print_rate > 0 && @newline_rate.zero?
    sleep(1.0 / @newline_rate) if @newline_rate > 0
  end
end

# Null output used when we are not tracing
class NullOut
  def print_item(_) end

  def print_line(_) end

  def last_was_numeric
    false
  end

  def tab; end

  def semicolon; end

  def implied; end

  def trace_output(_) end

  def newline; end

  def newline_when_needed; end

  def implied_newline; end

  def print_out(_) end

  def delay; end

  def newline_delay; end
end

# stores values from DATA statement
class DataStore
  def initialize
    @data_store = []
    @data_index = 0
  end

  def store(values)
    @data_store += values
  end

  def read
    raise BASICException, 'Out of data' if @data_index >= @data_store.size
    @data_index += 1
    @data_store[@data_index - 1]
  end

  def reset
    @data_index = 0
  end
end

# reads values from file and writes values to file
class FileHandler
  def initialize(file_name)
    @file_name = file_name
    @mode = nil
    @file = nil
    @data_store = []
  end

  include Reader
  include Inputter

  def set_mode(mode)
    if @mode.nil?
      case mode
      when :print
        @file = File.new(@file_name, 'wt')
      when :read
        @file = File.new(@file_name, 'rt')
      else
        raise(Exception, 'Invalid file mode')
      end
      @mode = mode
    else
      raise(BASICException, 'Inconsistent file operation') unless @mode == mode
    end
  end

  def close
    @file.close unless @file.nil?
  end

  def read_line
    input_text = @file.gets
    raise(BASICException, 'End of file') if input_text.nil?
    input_text = input_text.chomp
    ascii_printables(input_text)
  end

  def print_item(text)
    set_mode(:print)
    @file.print text
  end

  def last_was_numeric
    # for a file, this function does nothing
    set_mode(:print)
  end

  def newline
    set_mode(:print)
    @file.puts
  end

  def implied_newline
    # for a file, this function does nothing
    set_mode(:print)
  end

  def tab
    set_mode(:print)
    @file.putc ' '
  end

  def semicolon
    set_mode(:print)
    @file.putc ' '
  end

  def implied
    set_mode(:print)
    @file.putc ' '
  end

  def read
    set_mode(:read)

    tokenizers = make_tokenizers
    tokenizer = Tokenizer.new(tokenizers, InvalidTokenBuilder.new)
    @data_store = refill(@data_store, @file, tokenizer)
    @data_store.shift
  end

  private

  def refill(data_store, file, tokenizer)
    while data_store.empty?
      line = file.gets
      raise(BASICException, 'End of file') if line.nil?
      line = line.chomp

      tokens = tokenizer.tokenize(line)
      tokens.delete_if { |token| token.separator? || token.whitespace? }

      converted = read_convert(tokens)
      data_store += converted
    end

    data_store
  end

  def read_convert(tokens)
    converted = []

    tokens.each do |token|
      t = NumericConstant.new(token) if token.numeric_constant?
      t = TextConstant.new(token) if token.text_constant?
      converted << t
    end

    converted
  end
end
