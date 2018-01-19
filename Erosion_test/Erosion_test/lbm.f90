program lbm

use Cdata
use Avg
use Evolve
use Force
use Messages
use Sub
use BoundCond
use InitCond
implicit none
integer :: unit,iostat=0
integer :: it
open(unit=10,file='input',status='old')
call rparam_cdata(10, iostat)
close(10)
call allocate_cdata()
call allocate_avg()
call initff()
do it=1,iTMAX
  call set_boundary_before()
  call stream() 
  call calc_avg()
  call comp_equilibrium_BGK()
  call collision()
  call set_boundary_after()
enddo
call free_avg()
call free_cdata()
endprogram
