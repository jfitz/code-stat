import fileinput

for line in fileinput.input():
    line = line.rstrip('\r\n')

    in_quotes = False
    prev_was_escape = False 

    for char in line:
        if in_quotes:
            if char == '"' and not prev_was_escape:
                in_quotes = False
            print(char, end = '')
            prev_was_escape = char == '\\'
            
        else:
            if char == '"':
                in_quotes = True
            if char in ['{', '}'] and not in_quotes:
                print()
            print(char, end = '')
            if char in ['{', '}', ','] and not in_quotes:
                print()
