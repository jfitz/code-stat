* REPT2DM: This program prints or displays information about employees
* earning more than the midpoint of their salary range.
erase
select secondary
use
select secondary
use jobdet
select primary
use employee
* create new file with fields from employee (last name, first name, salary)
* and jobdet (job title, low salary, high salary)
join to midpoint for job:code = s.job:code field first:name, last:name,;
   job:title, low:sal, hi:sal,salary
use midpoint
* second line of heading is within midform
set heading to    Employees Earning More Than
report form midform for salary > (low:sal + hi:sal)/2
set heading to
select secondary
use
select primary
return










LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL