#!/usr/bin/ruby
require 'benchmark'
require 'optparse'
require 'singleton'
require 'io/console'

require_relative 'exceptions'
require_relative 'tokens'
require_relative 'tokenbuilders'
require_relative 'tokenizers'
require_relative 'constants'
require_relative 'operators'
require_relative 'functions'
require_relative 'expressions'
require_relative 'io'
require_relative 'statements'
require_relative 'modifiers'

# Contain line numbers
class LineNumber
  attr_reader :line_number

  def self.init?(text)
    /\A\d+\z/.match(text)
  end

  def initialize(line_number)
    raise BASICException, "Invalid line number '#{line_number}'" unless
      line_number.class.to_s == 'NumericConstantToken'

    @line_number = line_number.to_i
  end

  def eql?(other)
    @line_number == other.line_number
  end

  def ==(other)
    @line_number == other.line_number
  end

  def hash
    @line_number.hash
  end

  def succ
    LineNumber.new(@line_number + 1)
  end

  def <=>(other)
    @line_number <=> other.line_number
  end

  def >(other)
    @line_number > other.line_number
  end

  def >=(other)
    @line_number >= other.line_number
  end

  def <(other)
    @line_number < other.line_number
  end

  def <=(other)
    @line_number <= other.line_number
  end

  def to_s
    @line_number.to_s
  end
end

# line number range, in form start-end
class LineNumberRange
  attr_reader :list

  def self.init?(text)
    /\A\d+-\d+\z/.match(text)
  end

  def initialize(spec, program_line_numbers)
    raise(BASICException, 'Invalid list specification') unless
      LineNumberRange.init?(spec)
    parts = spec.split('-')
    start_val = LineNumber.new(NumericConstantToken.new(parts[0]))
    end_val = LineNumber.new(NumericConstantToken.new(parts[1]))
    @list = []
    program_line_numbers.each do |line_number|
      @list << line_number if line_number >= start_val && line_number <= end_val
    end
  end
end

# line number range, in form start-count (count default is 20)
class LineNumberCountRange
  attr_reader :list

  def self.init?(text)
    /\A\d+\+\d+\z/.match(text) || /\A\d+\+\z/.match(text)
  end

  def initialize(spec, program_line_numbers)
    raise(BASICException, 'Invalid list specification') unless
      LineNumberCountRange.init?(spec)

    parts = spec.split('+')
    start_val = LineNumber.new(NumericConstantToken.new(parts[0]))
    count = parts.size > 1 ? parts[1].to_i : 20
    make_list(program_line_numbers, start_val, count)
  end

  private

  def make_list(program_line_numbers, start_val, count)
    @list = []
    program_line_numbers.each do |line_number|
      if line_number >= start_val && count >= 0
        @list << line_number
        count -= 1
      end
    end
  end
end

# Contain line number ranges
# used in LIST and DELETE commands
class LineListSpec
  attr_reader :line_numbers
  attr_reader :range_type

  def initialize(text, program_line_numbers)
    @line_numbers = []
    @range_type = :empty
    if text == ''
      @line_numbers = program_line_numbers
      @range_type = :all
    elsif LineNumber.init?(text)
      make_single(text, program_line_numbers)
    elsif LineNumberRange.init?(text)
      make_range(text, program_line_numbers)
    elsif LineNumberCountRange.init?(text)
      make_count_range(text, program_line_numbers)
    else
      raise(BASICException, 'Invalid list specification')
    end
  end

  private

  def make_single(text, program_line_numbers)
    line_number = LineNumber.new(NumericConstantToken.new(text))
    @line_numbers << line_number if
      program_line_numbers.include?(line_number)
    @range_type = :single
  end

  def make_range(text, program_line_numbers)
    range = LineNumberRange.new(text, program_line_numbers)
    @line_numbers = range.list
    @range_type = :range
  end

  def make_count_range(text, program_line_numbers)
    range = LineNumberCountRange.new(text, program_line_numbers)
    @line_numbers = range.list
    @range_type = :range
  end
end

# LineNumberIndex class to hold line number and index within line
class LineNumberIndex
  attr_reader :number
  attr_reader :statement
  attr_reader :index

  def initialize(number, statement, index)
    @number = number
    @statement = statement
    @index = index
  end

  def to_s
    return @number.to_s if @statement.zero? && @index.zero?
    return @number.to_s + '.' + @statement.to_s if @index.zero?
    @number.to_s + '.' + @statement.to_s + '.' + @index.to_s
  end
end

# Line class to hold a line of code
class Line
  attr_reader :statements
  attr_reader :tokens

  def initialize(text, statements, tokens, comment)
    @text = text
    @statements = statements
    @tokens = tokens
    @comment = comment
  end

  def list
    @text
  end

  def pretty
    text = AbstractToken.pretty_tokens([], @tokens)
    unless @comment.nil?
      space = @text.size - (text.size + @comment.to_s.size)
      space = 5 if space < 5
      text += ' ' * space
      text += @comment.to_s
    end
    text
  end

  def profile
    texts = []
    @statements.each { |statement| texts << statement.profile }
  end

  def renumber(renumber_map)
    tokens = []
    separator = StatementSeparatorToken.new('\\')
    @statements.each do |statement|
      statement.renumber(renumber_map)
      tokens << statement.keywords
      tokens << statement.tokens
      tokens << separator
    end
    tokens.pop
    text = AbstractToken.pretty_tokens([], tokens.flatten)
    Line.new(text, @statements, tokens.flatten, @comment)
  end

  def check(program, console_io, line_number)
    retval = true
    index = 0
    @statements.each do |statement|
      line_number_index = LineNumberIndex.new(line_number, index, 0)
      r = statement.program_check(program, console_io, line_number_index)
      retval &&= r
      index += 1
    end
    retval
  end
end

# the interpreter
class Interpreter
  attr_reader :current_line_index
  attr_accessor :next_line_index
  attr_reader :console_io
  attr_reader :trace_out
  attr_reader :if_false_next_line

  def initialize(console_io, int_floor, ignore_rnd_arg, randomize,
                 respect_randomize, if_false_next_line)
    @running = false
    @randomizer = Random.new(1)
    @randomizer = Random.new if randomize && respect_randomize
    @respect_randomize = respect_randomize
    @int_floor = int_floor
    @ignore_rnd_arg = ignore_rnd_arg
    @console_io = console_io
    @data_store = DataStore.new
    @file_handlers = {}
    @return_stack = []
    @fornexts = {}
    @dimensions = {}
    @user_functions = {}
    @user_var_names = {}
    @user_var_values = []
    @program_lines = {}
    @variables = {}
    @get_value_seen = []
    @if_false_next_line = if_false_next_line
  end

  private

  def verify_next_line_index
    raise BASICException, 'Program terminated without END' if
      @next_line_index.nil?
    line_numbers = @program_lines.keys
    unless line_numbers.include?(@next_line_index.number)
      raise(BASICException, "Line number #{@next_line_index.number} not found")
    end
  end

  public

  def run(program, trace_flag, show_timing)
    @program = program
    @program_lines = program.lines
    @trace_flag = trace_flag

    # reset profile metrics
    @program_lines.keys.sort.each do |line_number|
      line = @program_lines[line_number]
      statements = line.statements
      statements.each do |statement|
        statement.profile_count = 0
        statement.profile_time = 0
      end
    end

    @null_out = NullOut.new

    timing = Benchmark.measure { run_and_time }
    print_timing(timing) if show_timing
  end

  def run_and_time
    # run the program
    @variables = {}
    @trace_out = @trace_flag ? @console_io : @null_out
    @running = true
    run_phase_1
    run_phase_2 if @running
  end

  def print_timing(timing)
    user_time = timing.utime + timing.cutime
    sys_time = timing.stime + timing.cstime
    @console_io.newline
    @console_io.print_line('CPU time:')
    @console_io.print_line(" user: #{user_time.round(2)}")
    @console_io.print_line(" system: #{sys_time.round(2)}")
    @console_io.newline
  end

  def run_phase_1
    # phase 1: do all initialization (store values in DATA lines)
    line_number = @program_lines.min[0]
    @current_line_index = LineNumberIndex.new(line_number, 0, 0)
    preexecute_loop
  end

  def run_phase_2
    # phase 2: run each command
    # start with the first line number
    line_number = @program_lines.min[0]
    @current_line_index = LineNumberIndex.new(line_number, 0, 0)
    begin
      program_loop while @running
    rescue Interrupt
      stop_running
    end
    @file_handlers.each { |_, fh| fh.close }
  end

  def print_trace_info(statement)
    @trace_out.newline_when_needed
    @trace_out.print_out @current_line_index.to_s + ':' + statement.pretty
    @trace_out.newline
  end

  def print_errors(line_number, statement)
    @console_io.print_line("Errors in line #{line_number}:")
    statement.errors.each { |error| puts error }
  end

  def preexecute_a_statement
    line_number = @current_line_index.number
    line = @program_lines[line_number]
    statements = line.statements
    statement_index = @current_line_index.statement
    statement = statements[statement_index]

    if statement.errors.empty?
      statement.pre_execute(self)
    else
      stop_running
      print_errors(line_number, statement)
    end
  end

  def preexecute_loop
    while !@current_line_index.nil? && @running
      preexecute_a_statement
      @current_line_index = @program.find_next_line_index(@current_line_index)
    end
  rescue BASICException => e
    line_number = @current_line_index.number
    message = "#{e.message} in line #{line_number}"
    @console_io.print_line(message)
    stop_running
  end

  def execute_a_statement
    line_number = @current_line_index.number
    line = @program_lines[line_number]
    statements = line.statements
    statement_index = @current_line_index.statement
    statement = statements[statement_index]
    print_trace_info(statement)
    if statement.errors.empty?
      timing = Benchmark.measure { statement.execute(self) }
      user_time = timing.utime + timing.cutime
      sys_time = timing.stime + timing.cstime
      time = user_time + sys_time
      statement.profile_time += time
      statement.profile_count += 1
    else
      stop_running
      print_errors(line_number, statement)
    end
    @get_value_seen = []
  end

  def program_loop
    # pick the next line number
    @next_line_index = @program.find_next_line_index(@current_line_index)
    begin
      execute_a_statement
      # set the next line number
      @current_line_index = nil
      if @running
        verify_next_line_index
        @current_line_index = @next_line_index
      end
    rescue BASICException => e
      if @current_line_index.nil?
        @console_io.print_line(e.message)
      else
        line_number = @current_line_index.number
        @console_io.print_line("#{e.message} in line #{line_number}")
      end
      stop_running
    end
  end

  def line_number?(line_number)
    @program_lines.key?(line_number)
  end

  def find_next_line
    @program.find_next_line(@current_line_index)
  end

  def statement_start_index(line_number, _statement_index)
    line = @program_lines[line_number]
    return if line.nil?

    statements = line.statements
    statement = statements[0]
    statement.start_index unless statement.nil?
  end

  def trace(tron_flag)
    @trace_out = (@trace_flag || tron_flag) ? @console_io : @null_out
  end

  def clear_variables
    @variables = {}
  end

  # returns an Array of values
  def evaluate(parsed_expressions, trace)
    old_trace_flag = @trace_flag
    @trace_flag = trace
    result_values = []
    parsed_expressions.each do |parsed_expression|
      stack = []
      exp = parsed_expression.empty? ? 0 : 1
      parsed_expression.each do |element|
        value = element.evaluate(self, stack, trace)
        stack.push value
      end
      act = stack.length
      raise(BASICException, 'Bad expression') if act != exp

      next if act.zero?

      # verify each item is of correct type
      item = stack[0]
      result_values << item
    end
    @trace_flag = old_trace_flag
    result_values
  end

  def dump_vars
    @variables.each do |key, value|
      @console_io.print_line("#{key}: #{value}")
    end
    puts
  end

  def dump_user_functions
    @user_functions.each do |name, expression|
      @console_io.print_line("#{name}: #{expression}")
    end
    puts
  end

  def dump_dims
    @dimensions.each do |key, value|
      dims = []
      value.each { |nc| dims << nc.to_v }
      @console_io.print_line("#{key.class}:#{key} (#{dims.join(', ')})")
    end
  end

  def stop
    stop_running
    @console_io.print_line("STOP in line #{@current_line_index.number}")
  end

  def stop_running
    @running = false
  end

  def rand(upper_bound)
    upper_bound = upper_bound.to_v
    upper_bound = upper_bound.truncate
    upper_bound = 1 if upper_bound <= 0
    upper_bound = 1 if @ignore_rnd_arg
    upper_bound = upper_bound.to_f
    NumericConstant.new(@randomizer.rand(upper_bound))
  end

  def new_random
    @randomizer = Random.new if @respect_randomize
  end

  def find_closing_next(control_variable)
    # move to the next statement
    line_number = @current_line_index.number
    line = @program_lines[line_number]
    statements = line.statements
    statement_index = @current_line_index.statement + 1
    line_numbers = @program_lines.keys.sort
    if statement_index < statements.size
      forward_line_numbers =
        line_numbers.select { |ln| ln >= @current_line_index.number }
    else
      forward_line_numbers =
        line_numbers.select { |ln| ln > @current_line_index.number }
    end

    # search for a NEXT with the same control variable
    until forward_line_numbers.empty?
      line_number = forward_line_numbers[0]
      line = @program_lines[line_number]
      statements = line.statements
      statement_index = 0
      statements.each do |statement|
        # consider only core statements, not modifiers
        return LineNumberIndex.new(line_number, statement_index, 0) if
          statement.class.to_s == 'NextStatement' &&
          statement.control == control_variable
        statement_index += 1
      end

      forward_line_numbers.shift
    end

    raise(BASICException, 'FOR without NEXT') # if none found, error
  end

  def set_dimensions(variable, subscripts)
    name = variable.name
    int_subscripts = normalize_subscripts(subscripts)
    @dimensions[name] = int_subscripts
  end

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

  def get_dimensions(variable)
    @dimensions[variable]
  end

  def set_user_function(name, variable_names, expressions)
    @user_var_names[name] = variable_names
    @user_functions[name] = expressions
  end

  def get_user_function(name)
    @user_functions[name]
  end

  def set_user_var_values(name, user_var_values)
    param_names = @user_var_names[name]
    param_names_values = param_names.zip(user_var_values)
    names_and_values = Hash[param_names_values]
    @user_var_values.push(names_and_values)
  end

  def clear_user_var_values
    @user_var_values.pop
  end

  private

  def make_dimensions(variable, count)
    if @dimensions.key?(variable)
      @dimensions[variable]
    else
      Array.new(count, NumericConstant.new(10))
    end
  end

  public

  def check_subscripts(variable, subscripts)
    int_subscripts = normalize_subscripts(subscripts)
    dimensions = make_dimensions(variable, int_subscripts.size)
    raise(BASICException, 'Incorrect number of subscripts') if
      int_subscripts.size != dimensions.size
    int_subscripts.zip(dimensions).each do |pair|
      raise(BASICException, "Subscript #{pair[0]} out of range #{pair[1]}") if
        pair[0] > pair[1]
    end
  end

  def get_value(variable, trace)
    value = nil
    # first look in user function values stack
    length = @user_var_values.length
    if length > 0
      names_and_values = @user_var_values[-1]
      value = names_and_values[variable]
    end
    # then look in general table
    if value.nil?
      v = variable.to_s
      default_type = variable.content_type
      default_value = NumericConstant.new(0)
      default_value = TextConstant.new(TextConstantToken.new('""')) if
        default_type == 'TextConstant'
      @variables[v] = default_value unless @variables.key?(v)
      value = @variables[v]
    end
    seen = @get_value_seen.include?(variable)
    if @trace_flag && trace && !seen
      @trace_out.print_line(' ' + variable.to_s + ' = ' + value.to_s)
      @get_value_seen << variable
    end
    value
  end

  def set_value(variable, value)
    # convert a numeric to a string when a string is expected
    if value.numeric_constant? &&
       variable.content_type == 'TextConstant'
      val = value.token_chars
      quoted_val = '"' + val + '"'
      token = TextConstantToken.new(quoted_val)
      value = TextConstant.new(token)
    end

    # convert an integer to a numeric
    if value.class.to_s == 'NumericConstant' &&
       variable.content_type == 'IntegerConstant'
      token = IntegerConstantToken.new(value.to_s)
      value = IntegerConstant.new(token)
    end

    # convert a numeric to an integer
    if value.class.to_s == 'IntegerConstant' &&
       variable.content_type == 'NumericConstant'
      token = NumericConstantToken.new(value.to_s)
      value = NumericConstant.new(token)
    end

    # check that value type matches variable type
    unless variable.compatible?(value)
      raise(BASICException,
            "Type mismatch '#{value}' is not #{variable.content_type}")
    end

    var = variable.to_s
    @variables[var] = value
    @trace_out.print_line(' ' + variable.to_s + ' = ' + value.to_s)
  end

  def set_values(name, values)
    values.each do |coords, value|
      variable = Variable.new(name, coords)
      set_value(variable, value)
    end
  end

  def push_return(destination)
    @return_stack.push(destination)
  end

  def pop_return
    raise(BASICException, 'RETURN without GOSUB') if @return_stack.empty?
    @return_stack.pop
  end

  def assign_fornext(fornext_control)
    control_variable = fornext_control.control
    @fornexts[control_variable] = fornext_control
  end

  def retrieve_fornext(control_variable)
    fornext = @fornexts[control_variable]
    raise(BASICException, 'NEXT without FOR') if fornext.nil?
    fornext
  end

  def add_file_names(file_names)
    file_names.each do |name|
      raise(BASICException, 'Invalid file name') unless
        name.class.to_s == 'TextConstant'
      raise(BASICException, "File '#{name.to_v}' not found") unless
        File.file?(name.to_v)
      file_handle = FileHandle.new(@file_handlers.size + 1)
      @file_handlers[file_handle] = FileHandler.new(name.to_v)
    end
  end

  def open_file(filename, fh, mode)
    raise(BASICException, 'Invalid file name') unless
      filename.class.to_s == 'TextConstant'
    ### todo: check for already open handle
    fhr = FileHandler.new(filename.to_v)
    fhr.set_mode(mode)
    @file_handlers[fh] = fhr
  end

  def close_file(fh)
    raise(BASICException, 'Unknown file handle') unless
      @file_handlers.key?(fh)
    fhr = @file_handlers[fh]
    fhr.close
    ### todo: remove file handle
  end

  def get_file_handler(file_handle)
    return @console_io if file_handle.nil?

    raise(BASICException, 'Unknown file handle') unless
      @file_handlers.key?(file_handle)
    fh = @file_handlers[file_handle]
    fh.set_mode(:print)
    fh
  end

  def get_input(file_handle)
    raise(BASICException, 'Unknown file handle') unless
      @file_handlers.key?(file_handle)
    fh = @file_handlers[file_handle]
    fh.set_mode(:read)
    fh
  end

  def get_data_store(file_handle)
    return @data_store if file_handle.nil?
    raise(BASICException, 'Unknown file handle') unless
      @file_handlers.key?(file_handle)
    fh = @file_handlers[file_handle]
    fh.set_mode(:read)
    fh
  end

  def int_floor?
    @int_floor
  end
end

# program container
class Program
  def initialize(console_io, tokenizers)
    @console_io = console_io
    @program_lines = {}
    @statement_factory = StatementFactory.instance
    @statement_factory.tokenizers = tokenizers
  end

  def empty?
    @program_lines.empty?
  end

  def line_number?(line_number)
    @program_lines.key?(line_number)
  end

  def lines
    @program_lines
  end

  def cmd_new
    @program_lines = {}
  end

  def list(linespec, list_tokens)
    linespec = linespec.strip
    if !@program_lines.empty?
      begin
        line_numbers = @program_lines.keys.sort
        line_number_range = LineListSpec.new(linespec, line_numbers)
        line_numbers = line_number_range.line_numbers
        list_lines_errors(line_numbers, list_tokens)
      rescue BASICException => e
        @console_io.print_line(e)
      end
    else
      @console_io.print_line('No program loaded')
    end
  end

  def pretty(linespec)
    linespec = linespec.strip
    if !@program_lines.empty?
      begin
        line_numbers = @program_lines.keys.sort
        line_number_range = LineListSpec.new(linespec, line_numbers)
        line_numbers = line_number_range.line_numbers
        pretty_lines_errors(line_numbers)
      rescue BASICException => e
        @console_io.print_line(e)
      end
    else
      @console_io.print_line('No program loaded')
    end
  end

  def profile(linespec)
    linespec = linespec.strip
    if !@program_lines.empty?
      begin
        line_numbers = @program_lines.keys.sort
        line_number_range = LineListSpec.new(linespec, line_numbers)
        line_numbers = line_number_range.line_numbers
        profile_lines_errors(line_numbers)
      rescue BASICException => e
        @console_io.print_line(e)
      end
    else
      @console_io.print_line('No program loaded')
    end
  end

  def load(filename)
    filename = filename.strip
    if !filename.empty?
      begin
        File.open(filename, 'r') do |file|
          @program_lines = {}
          file.each_line do |line|
            line = @console_io.ascii_printables(line)
            store_program_line(line, false)
          end
        end
        true
      rescue Errno::ENOENT
        @console_io.print_line("File '#{filename}' not found")
        false
      end
    else
      @console_io.print_line('Filename not specified')
      false
    end
  end

  def save(filename)
    if @program_lines.empty?
      @console_io.print_line('No program loaded')
    else
      filename = filename.strip
      if filename.empty?
        @console_io.print_line('Filename not specified')
      else
        save_file(filename)
      end
    end
  end

  def save_file(filename)
    line_numbers = @program_lines.keys.sort
    begin
      File.open(filename, 'w') do |file|
        line_numbers.each do |line_num|
          line = @program_lines[line_num]
          statements = line.statements
          text = statements.join('\\')
          file.puts line_num + ' ' + text
        end
        file.close
      end
    rescue Errno::ENOENT
      @console_io.print_line("File '#{filename}' not opened")
    end
  end

  def print_profile
    @program_lines.keys.sort.each do |line_number|
      line = @program_lines[line_number]
      number = line_number.to_s
      statements = line.statements
      statement_index = 0
      statements.each do |statement|
        profile = statement.profile
        text = number.to_s + '.' + statement_index.to_s + profile
        puts text
        statement_index += 1
      end
    end
  end

  def delete(linespec)
    if @program_lines.empty?
      @console_io.print_line('No program loaded')
    else
      delete_lines(linespec.strip)
    end
  end

  def delete_lines(linespec)
    line_numbers = @program_lines.keys.sort
    line_number_range = LineListSpec.new(linespec.strip, line_numbers)
    line_numbers = line_number_range.line_numbers
    case line_number_range.range_type
    when :single
      delete_specific_lines(line_numbers)
    when :range
      list_and_delete_lines(line_numbers)
    when :all
      @console_io.print_line('Type NEW to delete an entire program')
    end
  rescue BASICException => e
    @console_io.print_line(e)
  end

  def renumber
    # generate new line numbers
    renumber_map = {}
    new_number = 10
    @program_lines.keys.sort.each do |line_number|
      number_token = NumericConstantToken.new(new_number)
      new_line_number = LineNumber.new(number_token)
      renumber_map[line_number] = new_line_number
      new_number += 10
    end
    # assign new line numbers
    new_program_lines = {}
    @program_lines.keys.sort.each do |line_number|
      line = @program_lines[line_number]
      new_line_number = renumber_map[line_number]
      new_program_lines[new_line_number] = line.renumber(renumber_map)
    end

    @program_lines = new_program_lines
  end

  def store_program_line(cmd, print_errors)
    line_num, line = parse_line(cmd)
    if !line_num.nil? && !line.nil?
      check_line_duplicate(line_num, print_errors)
      check_line_sequence(line_num, print_errors)
      @program_lines[line_num] = line
      statements = line.statements
      any_errors = false
      statements.each do |statement|
        statement.errors.each { |error| puts error } if print_errors
        any_errors |= !statement.errors.empty?
      end
      any_errors
    else
      true
    end
  end

  def parse_line(line)
    @statement_factory.parse(line)
  rescue BASICException => e
    @console_io.print_line("Syntax error: #{e.message}")
  end

  def check_line_duplicate(line_num, print_errors)
    # warn about duplicate lines when loading
    # but not when typing
    @console_io.print_line("Duplicate line number #{line_num}") if
      @program_lines.key?(line_num) && !print_errors
  end

  def check_line_sequence(line_num, print_errors)
    # warn about lines out of sequence
    # but not when typing
    @console_io.print_line("Line #{line_num} is out of sequence") if
      !@program_lines.empty? &&
      line_num < @program_lines.max[0] &&
      !print_errors
  end

  def list_lines_errors(line_numbers, list_tokens)
    line_numbers.each do |line_number|
      line = @program_lines[line_number]
      @console_io.print_line(line_number.to_s + line.list)
      statements = line.statements
      statements.each do |statement|
        statement.errors.each { |error| puts ' ' + error }
      end
      tokens = line.tokens
      text_tokens = tokens.map(&:to_s)
      @console_io.print_line('TOKENS: ' + text_tokens.to_s) if list_tokens
    end
  end

  def pretty_lines_errors(line_numbers)
    line_numbers.each do |line_number|
      line = @program_lines[line_number]
      number = line_number.to_s
      pretty = line.pretty
      @console_io.print_line(number + pretty)
      statements = line.statements
      statements.each do |statement|
        statement.errors.each { |error| puts ' ' + error }
      end
    end
  end

  def profile_lines_errors(line_numbers)
    line_numbers.each do |line_number|
      line = @program_lines[line_number]
      number = line_number.to_s
      statements = line.statements
      statement_index = 0
      statements.each do |statement|
        profile = statement.profile
        @console_io.print_line(number + '.' + statement_index.to_s + profile)
        statement_index += 1
      end
    end
  end

  def list_lines(line_numbers)
    line_numbers.each do |line_number|
      line = @program_lines[line_number]
      text = line.list
      @console_io.print_line(line_number.to_s + text)
    end
  end

  def delete_specific_lines(line_numbers)
    line_numbers.each { |line_number| @program_lines.delete(line_number) }
  end

  def list_and_delete_lines(line_numbers)
    list_lines(line_numbers)
    print 'DELETE THESE LINES? '
    answer = @console_io.read_line
    delete_specific_lines(line_numbers) if answer == 'YES'
  end

  def check
    result = true
    @program_lines.keys.sort.each do |line_number|
      r = @program_lines[line_number].check(self, @console_io, line_number)
      result &&= r
    end
    result
  end

  def find_next_line_index(current_line_index)
    # find next index with current statement
    line_number = current_line_index.number
    line = @program_lines[line_number]

    statements = line.statements
    statement_index = current_line_index.statement
    statement = statements[statement_index]

    index = current_line_index.index
    if index < statement.last_index
      index += 1
      return LineNumberIndex.new(line_number, statement_index, index)
    end

    # find next statement within the current line
    if statement_index < statements.size - 1
      statement_index += 1
      statement = statements[statement_index]
      index = statement.start_index
      return LineNumberIndex.new(line_number, statement_index, index)
    end

    # find the next line
    line_numbers = @program_lines.keys.sort
    line_number = current_line_index.number
    index = line_numbers.index(line_number)
    line_number = line_numbers[index + 1]
    unless line_number.nil?
      line = @program_lines[line_number]
      statements = line.statements
      statement = statements[0]
      index = statement.start_index
      return LineNumberIndex.new(line_number, 0, index)
    end

    # nothing left to execute
    nil
  end

  def find_next_line(current_line_index)
    # find next numbered statement
    line_numbers = @program_lines.keys.sort
    line_number = current_line_index.number
    index = line_numbers.index(line_number)
    line_number = line_numbers[index + 1]
    unless line_number.nil?
      line = @program_lines[line_number]
      statements = line.statements
      statement = statements[0]
      index = statement.start_index
      next_line_index = LineNumberIndex.new(line_number, 0, index)
      return next_line_index
    end

    # nothing left to execute
    nil
  end
end

# interactive shell
class Shell
  def initialize(console_io, interpreter, program)
    @console_io = console_io
    @interpreter = interpreter
    @program = program
  end

  def run
    need_prompt = true
    done = false
    until done
      @console_io.print_line('READY') if need_prompt
      cmd = @console_io.read_line
      if /\A\d/ =~ cmd
        # starts with a number, so maybe it is a program line
        need_prompt = @program.store_program_line(cmd, true)
      else
        # immediate command -- execute
        done = execute_command(cmd)
        need_prompt = true
      end
    end
  end

  private

  def execute_command(cmd)
    return false if cmd.empty?
    return true if cmd == 'EXIT'
    cmd4 = cmd[0..3]
    cmd6 = cmd[0..5]
    cmd7 = cmd[0..6]
    cmd8 = cmd[0..7]
    if simple_command?(cmd)
      execute_simple_command(cmd)
    elsif command_4?(cmd4)
      execute_4_command(cmd4, cmd[4..-1])
    elsif command_6?(cmd6)
      execute_6_command(cmd6, cmd[6..-1])
    elsif command_7?(cmd7)
      execute_7_command(cmd7, cmd[7..-1])
    elsif command_8?(cmd8)
      execute_8_command(cmd8, cmd[8..-1])
    else
      print "Unknown command #{cmd}\n"
    end
    false
  end

  def simple_command?(text)
    %w(NEW RUN TRACE .VARS .UDFS .DIMS).include?(text)
  end

  def execute_simple_command(text)
    case text
    when 'NEW'
      @program.cmd_new
      @interpreter.clear_variables
    when 'RUN'
      cmd_run(false, true)
    when 'TRACE'
      cmd_run(true, false)
    when '.VARS'
      dump_vars
    when '.UDFS'
      dump_user_functions
    when '.DIMS'
      dump_dims
    end
  end

  def command_4?(text)
    %w(LIST LOAD SAVE).include?(text)
  end

  def execute_4_command(cmd, rest)
    case cmd
    when 'LIST'
      @program.list(rest, false)
    when 'LOAD'
      @program.load(rest)
    when 'SAVE'
      @program.save(rest)
    end
  end

  def command_6?(text)
    %w(TOKENS PRETTY DELETE).include?(text)
  end

  def execute_6_command(cmd, rest)
    case cmd
    when 'TOKENS'
      @program.list(rest, true)
    when 'PRETTY'
      @program.pretty(rest)
    when 'DELETE'
      @program.delete(rest)
    end
  end

  def command_7?(text)
    %w(PROFILE).include?(text)
  end

  def execute_7_command(cmd, rest)
    case cmd
    when 'PROFILE'
      @program.profile(rest)
    end
  end

  def command_8?(text)
    %w(RENUMBER).include?(text)
  end

  def execute_8_command(cmd, _)
    case cmd
    when 'RENUMBER'
      @program.renumber if @program.check
    end
  end

  def cmd_run(trace_flag, show_timing)
    if @program.empty?
      @console_io.print_line('No program loaded')
    else
      @interpreter.run(@program, trace_flag, show_timing) if @program.check
    end
  end

  def dump_vars
    @interpreter.dump_vars
  end

  def dump_user_functions
    @interpreter.dump_user_functions
  end

  def dump_dims
    @interpreter.dump_dims
  end
end

def make_tokenizers(statement_separators, comment_leads, allow_hash_constant,
                    min_max_op, colon_file)
  tokenizers = []

  tokenizers << CommentTokenBuilder.new(comment_leads)
  tokenizers << RemarkTokenBuilder.new

  unless statement_separators.empty?
    tokenizers <<
      ListTokenBuilder.new(statement_separators, StatementSeparatorToken)
  end

  statement_factory = StatementFactory.instance
  keywords = statement_factory.keywords_definitions

  tokenizers << ListTokenBuilder.new(keywords, KeywordToken)

  un_ops = UnaryOperator.operators(colon_file)
  bi_ops = BinaryOperator.operators(min_max_op)
  operators = (un_ops + bi_ops).uniq
  tokenizers << ListTokenBuilder.new(operators, OperatorToken)

  tokenizers << BreakTokenBuilder.new

  tokenizers << ListTokenBuilder.new(['(', '['], GroupStartToken)
  tokenizers << ListTokenBuilder.new([')', ']'], GroupEndToken)
  tokenizers << ListTokenBuilder.new([',', ';'], ParamSeparatorToken)

  tokenizers <<
    ListTokenBuilder.new(FunctionFactory.function_names, FunctionToken)

  function_names = ('FNA'..'FNZ').to_a
  tokenizers << ListTokenBuilder.new(function_names, UserFunctionToken)

  tokenizers << TextTokenBuilder.new
  tokenizers << NumberTokenBuilder.new(allow_hash_constant)
  tokenizers << IntegerTokenBuilder.new
  tokenizers << VariableTokenBuilder.new

  tokenizers <<
    ListTokenBuilder.new(%w(TRUE FALSE), BooleanConstantToken)

  tokenizers << WhitespaceTokenBuilder.new
end

options = {}
OptionParser.new do |opt|
  opt.on('-l', '--list SOURCE') { |o| options[:list_name] = o }
  opt.on('--tokens') { |o| options[:tokens] = o }
  opt.on('-p', '--pretty SOURCE') { |o| options[:pretty_name] = o }
  opt.on('-r', '--run SOURCE') { |o| options[:run_name] = o }
  opt.on('--profile') { |o| options[:profile] = o }
  opt.on('--no-heading') { |o| options[:no_heading] = o }
  opt.on('--echo-input') { |o| options[:echo_input] = o }
  opt.on('--trace') { |o| options[:trace] = o }
  opt.on('--no-timing') { |o| options[:no_timing] = o }
  opt.on('--tty') { |o| options[:tty] = o }
  opt.on('--tty-lf') { |o| options[:tty_lf] = o }
  opt.on('--no-colon-separator') { |o| options[:no_colon_sep] = o }
  opt.on('--colon-file') { |o| options[:colon_file] = o }
  opt.on('--bang-comment') { |o| options[:bang_comment] = o }
  opt.on('--print-width WIDTH') { |o| options[:print_width] = o }
  opt.on('--zone-width WIDTH') { |o| options[:zone_width] = o }
  opt.on('--back-tab') { |o| options[:back_tab] = o }
  opt.on('--int-floor') { |o| options[:int_floor] = o }
  opt.on('--ignore-rnd-arg') { |o| options[:ignore_rnd_arg] = o }
  opt.on('--implied-semicolon') { |o| options[:implied_semicolon] = o }
  opt.on('--qmark-after-prompt') { |o| options[:qmark_after_prompt] = o }
  opt.on('--randomize') { |o| options[:randomize] = o }
  opt.on('--ignore-randomize') { |o| options[:ignore_randomize] = o }
  opt.on('--if-false-next-line') { |o| options[:if_false_next_line] = o }
  opt.on('--hash-constant') { |o| options[:hash_constant] = o }
  opt.on('--min-max-op') { |o| options[:min_max_op] = o }
end.parse!

list_filename = options[:list_name]
list_tokens = options.key?(:tokens)
pretty_filename = options[:pretty_name]
run_filename = options[:run_name]
show_profile = options.key?(:profile)
show_heading = !options.key?(:no_heading)
echo_input = options.key?(:echo_input)
trace_flag = options.key?(:trace)
show_timing = !options.key?(:no_timing)
output_speed = 0
output_speed = 10 if options.key?(:tty)
newline_speed = 0
newline_speed = 10 if options.key?(:tty_lf)
colon_separator = !options.key?(:no_colon_sep)
colon_file = options.key?(:colon_file)
colon_separator = false if colon_file
backslash_separator = true
apostrophe_comment = true
bang_comment = options.key?(:bang_comment)
print_width = 72
print_width = options[:print_width].to_i if options.key?(:print_width)
zone_width = 16
zone_width = options[:zone_width].to_i if options.key?(:zone_width)
back_tab = options.key?(:back_tab)
int_floor = options.key?(:int_floor)
ignore_rnd_arg = options.key?(:ignore_rnd_arg)
implied_semicolon = options.key?(:implied_semicolon)
qmark_after_prompt = options.key?(:qmark_after_prompt)
randomize = options.key?(:randomize)
respect_randomize = true
respect_randomize = !options[:ignore_randomize] if
  options.key?(:ignore_randomize)
if_false_next_line = options.key?(:if_false_next_line)
statement_seps = []
statement_seps << '\\' if backslash_separator
statement_seps << ':' if colon_separator
comment_leads = []
comment_leads << "'" if apostrophe_comment
comment_leads << '!' if bang_comment
allow_hash_constant = options.key?(:hash_constant)
min_max_op = options.key?(:min_max_op)

default_prompt = TextConstantToken.new('"? "')
console_io =
  ConsoleIo.new(print_width, zone_width, back_tab, output_speed,
                newline_speed, implied_semicolon, default_prompt,
                qmark_after_prompt, echo_input)

tokenizers = make_tokenizers(statement_seps, comment_leads, allow_hash_constant,
                             min_max_op, colon_file)

if show_heading
  console_io.print_line('BASIC-1973 interpreter version -1')
  console_io.newline
end

program = Program.new(console_io, tokenizers)
if !run_filename.nil?
  if program.load(run_filename) && program.check
    interpreter =
      Interpreter.new(console_io, int_floor, ignore_rnd_arg, randomize,
                      respect_randomize, if_false_next_line)
    interpreter.run(program, trace_flag, show_timing)
  end
elsif !list_filename.nil?
  program.list('', list_tokens) if program.load(list_filename)
elsif !pretty_filename.nil?
  program.pretty('') if program.load(pretty_filename)
else
  interpreter =
    Interpreter.new(console_io, int_floor, ignore_rnd_arg, randomize,
                    respect_randomize, if_false_next_line)
  shell = Shell.new(console_io, interpreter, program)
  shell.run
end

if show_heading
  console_io.newline
  console_io.print_line('BASIC-1973 ended')
end
