 procedure FindFirstAnswer;
   var X, Y, Z, Count: Integer;
 label FoundAnAnswer;
 begin
   Count := SomeConstant;
   for X := 1 to Count do
     for Y := 1 to Count do
       for Z := 1 to Count do
         if { some condition holds on X, Y, and Z } then
           goto FoundAnAnswer;
 
     { Code to execute if no answer is found }
   Exit;
 
   FoundAnAnswer:
     { Code to execute when an answer is found }
 end;