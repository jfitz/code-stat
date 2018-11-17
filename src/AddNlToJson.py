import fileinput

for line in fileinput.input():
    line = line.rstrip('\r\n')

    start_of_file = True
    start_of_line = True
    in_quotes = False
    prev_was_escape = False
    prev_was_group = False
    prev_was_comma = False

    for char in line:
        if in_quotes:
            if char == '"' and not prev_was_escape:
                in_quotes = False

            print(char, end = '')
            prev_was_escape = char == '\\'
            
        else:
            if char == '"':
                in_quotes = True

            if char in ['}', ']']:
                print()

            if prev_was_group and char not in ['}', ']', ',']:
                print()

            if prev_was_comma:
                print()

            print(char, end = '')

            prev_was_group = char in ['{', '}', '[', ']']
            prev_was_comma = char == ','

        start_of_file = False

