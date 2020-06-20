/
/ Basic I/O Routine Vectors  - Page 0
/
*0050
GetChar,  XGetChar
Type,     XType
/
/ Code Segment - Page 1
/
*0200
Main,     cla cll   / clear AC and link
          kcc       / reset keyboard
          tls       / rest printer
...
          jms i GetChar  / read a character
          dca Hold       / store it
          tad Hold       / get character
          jms i Type     / echo to screen
...

/
/ Basic I/O routines - page 30
/
*7400
XGetChar, 0              / return address here
          ksf            / is keyboard flag raised?
          jmp .-1        / no  - loop
          krb            / yes - read character to AC
          jmp i XGetChar / return
XType,    0              / return address here
          tsk            / is printer flag raised?
          jmp .-1        / no  - loop
          tls            / yes - print character
          cla cll        / clear AC and link
          jmp i XType    / return
$Main
