! LBM2D
!
!  calculates evolution of ff
!
!The distribution function ff can have the following structures:
!  Array of Structures (AoS) ff(9,Nx+2,Ny+2)
!  Structure of Arrays (SoA) ff(Nx+2,Ny+2,9)
!  Bundle                    ff(3,Nx+2,Ny+2,3)
! Accordingly to Mattila et al 2007 the last one is the fastest
! in terms of cache misses ! But as the SoA is closest to the pencil
! structures we use that now. 
!***************************************************************
module Force
  use Messages
  use Cdata
  use Sub
  implicit none
  private
  public :: get_ueq
  character (len=labellen) :: iforce='none'
  double precision :: famp=1.
  namelist /force_pars/ &
     iforce,famp
!***************************************************************
contains
!***************************************************************
subroutine read_fpars(unit,iostat)
! read the namelist force_pars
  integer, intent(in) :: unit
  integer, intent(out) :: iostat
  read(unit, NML=force_pars, IOSTAT=iostat)
endsubroutine read_fpars
  !***************************************************************
subroutine get_ueq(uin,rhoin,ueq)
  double precision,dimension(2),intent(in) :: uin
  double precision, intent(in) :: rhoin
  double precision,dimension(2), intent(out) :: ueq
  select case (iforce)
  case('none')
     ueq=uin
  case default
     call fatal_error("force","iforce does not match")
  endselect
endsubroutine get_ueq
!***************************************************************
endmodule Force
