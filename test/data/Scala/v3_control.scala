if x < 0 then
  "negative"
else if x == 0 then
  "zero"
else
  "positive"

if x < 0 then -x else x

while x >= 0 do x = f(x)

for x <- xs if x > 0
yield x * x

for
  x <- xs
  y <- ys
do
  println(x + y)

try body
catch case ex: IOException => handle