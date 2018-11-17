import fileinput

indent_level = 0

for line in fileinput.input():
    line = line.strip()

    if len(line) > 0 and line[0] in [']', '}']:
        indent_level -= 1

    spaces = ' ' * indent_level * 4
    print(spaces + line)

    if len(line) > 0 and line[0] in ['[', '{']:
        indent_level += 1
