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
call initialize_bc()
do it=1,iTMAX
  call set_boundary_before()
  call calc_avg()
  call stream() 
!  call calc_avg()
  call rwrite_density_uu()
  call comp_equilibrium_BGK()
  call collision()
  call set_boundary_after()
enddo
write(*,*) 'ok so far'
call free_avg()
call free_cdata()
endprogram lbm
