/
/ Code Section
/
*0200          
Main,     cla cll   / clear AC and Link
          tad N     / load N
          cia       / negate it
          dca Count / deposit it to Count and clear AC
          dca Index / zero out Index
          dca Sum   / zero out sum
Loop,     isz Index / add 1 to Index
          tad Sum   / load Sum
          tad Index / add in Index
          dca Sum   / store result in Sum
          isz Count / increment Count and skip on zero
          jmp Loop  / otherwise loop
          hlt       / done
          jmp Main  / allows easy restart
/
/ Data Section 
/
*0250
N,   10
Count, 0
Index, 0
Sum, 0
$Main               / end of pgm - entry point
