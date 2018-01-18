program lbm

use Cdata
use Avg
use Evolve
use Force
use Messages
use Sub
implicit none
integer :: unit,iostat=0

open(unit=10,file='input',status='old')
call rparam_cdata(10, iostat)
close(10)
call allocate_cdata()
call allocate_avg()



call free_avg()
call free_cdata()
endprogram
