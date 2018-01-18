module BoundCond
use Cdata
use Avg

implicit none
private
character (len=labellen) :: bctype='periodic'
public :: initialize

namelist /bc_pars/ &
    bctype

!***************************************************************
contains
!***************************************************************
subroutine read_bc(unit,iostat)
! read the namelist bc_pars
  integer, intent(in) :: unit
  integer, intent(out) :: iostat
  read(unit, NML=bc_pars, IOSTAT=iostat)
endsubroutine read_bc
!***************************************************************
subroutine initialize()
! Set dx and dy depending on type of BC
! PBC: dx=Lx/Nx and dy=Ly/Ny
! No-slip: dx=Lx/(Nx-1) and dy=Ly/(Ny-1)
  if(bctype.eq.'periodic') then
    dx=Lx/Nx
    dy=Ly/Ny
  endif
  if(bctype.eq.'noslip') then
    dx=Lx/(Nx-1)
    dy=Ly/(Ny-1)
  endif
! Boundary types: rb, lb, tb, bb
! This is true for both directions

endsubroutine
!***************************************************************
endmodule 
