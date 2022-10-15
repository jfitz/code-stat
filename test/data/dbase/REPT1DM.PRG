* REPT1DM: This program displays or prints average salary for each job
* title.
erase
@ 2,30 say 'AVERAGE SALARIES'
@ 4,10 say 'Job Title'
@ 4,35 say 'Job Code'
@ 4,50 say '# Emp.'
@ 4,62 say 'Average Salary'
* line counter
store 7 to accum
select primary
use employee
select secondary
use
select secondary
use jobdet
* executes once/job title
do while .not. eof
   select primary
   sum salary for job:code = s.job:code to sumsal
   count for job:code = s.job:code to jobcount
   if jobcount = 0
      * to prevent zero divide
      store 0.0 to average
   else
      store sumsal/jobcount to average
   endif
   @ accum,10 say job:title
   @ accum,39 say s.job:code
   @ accum,43 say jobcount
   @ accum,63 say average
   select secondary
   skip
   store accum + 1 to accum
enddo
select secondary
use
select primary
return



 than midpoint of salary range'
   @ 12,10 say '3) Salary increase (by Department)'
   @ 14,10 say '4) Return to MASTER MENU'
   @ 17,5 say 'Please 