import argparse

parser = argparse.ArgumentParser(description='Check all bytes are ASCII.')
parser.add_argument('infile')

args = parser.parse_args()

in_file = open(args.infile, "rb")
bytes = in_file.read()
in_file.close()

errors = 0
line = 0
pos = 0
for b in bytes:
    if b > 127:
        print('byte ' + str(b) + ' line ' + str(line) + ' position ' + str(pos))
        errors += 1

    if b == 0x0a:
        line += 1
        pos = 0
    else:
        pos += 1

    if errors > 10:
        break

