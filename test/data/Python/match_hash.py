collisions = {}
for line in iter(input, ""):
  hash, *_ = line.split() # really should be re.split but that would be too mean to Python
  if hash not in collisions:
    collisions[hash] = []
  collisions[hash].append(line)

for hash, lines in collisions.items():
  if len(lines) > 1:
    print(hash)
    for line in lines:
      print(line)