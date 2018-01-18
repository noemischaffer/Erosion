module InitCond

use Cdata
use Avg

implicit none
private
public::initff

character (len=labellen) :: force_type='static'

namelist /init_cond/& 
    force_type

contains
!***************************************************************
subroutine initff()
integer :: i,j,q

select case (force_type)
case('static')
  do q=1,qmom
     do j=2,Ny+1
        do i=2,Nx+1
           if(is_solid(i,j).ne.1) then
              if(q.eq.5) then
                ff(i,j,q) = 1.0d0
              endif
              ff(i,j,q) = 0.0d0
           endif
        enddo
     enddo
  enddo
case default
        call fatal_error('initialize_force',&
            'error in force initialization! ')
endselect
endsubroutine initff
!***************************************************************

endmodule
