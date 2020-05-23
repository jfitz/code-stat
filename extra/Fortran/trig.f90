c c c c
      program trig
c
c       John Mahaffy,  Penn State University, CmpSc 201 Example
c       1/26/96
c
C
C    Program to Calculate SIN and COS of ANGLE
C    Given in Degrees
c
C    angdeg - angle in degrees  (INPUT)
*    angrad - angle in radians
c    pi     -  3.14159....
c    sinang - sin ( angrad )  OUTPUT
c    cosang - cos ( angrad )  OUTPUT
c  
      implicit none
c 
      real angrad, angdeg, pi, sinang, cosang
      print *, 'Angle in Degrees?'
      read *, angdeg
c
c       Convert the Angle to Radians
c      
          pi=asin(1.0)*2.0
c         
          angrad=angdeg*pi/180.0
c
c       Calculate Trig Functions
c     
      sinang=sin(angrad)
c     
      cosang=cos(angrad)
c
      print *, 'ANGLE =',angdeg,' degrees =',angrad,' radians'
      print *, 'SIN(ANGLE) = ', sinang
      print *, 'COS(ANGLE) = ', cosang
c
      stop
      end
c
c c 