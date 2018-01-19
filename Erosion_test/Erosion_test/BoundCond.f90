module BoundCond
use Cdata
use Avg

implicit none
private
public :: initialize, set_boundary_before,set_boundary_after
character (len=labellen) :: bc_type='xperiodic'

namelist /bc_pars/ &
    bc_type

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
! Boundary types: rb, lb, tb, bb
  select case (bc_type)
    case('xperiodic')
      dx=Lx/Nx
    case('yperiodic')
      dy=Ly/Ny
    case('noslip_xl', 'noslip_xr')
      dx=Lx/(Nx-1)
    case('noslip_yt', 'noslip_yb')
      dy=Ly/(Ny-1)
  endselect
endsubroutine initialize
!***************************************************************
subroutine set_boundary_before()
  integer :: i,q,m,n,qnext
  integer,dimension(3) :: qq 
  select case (bc_type)
  case('xperiodic')
    ff(Nx+2,:,:)=ff(2,:,:)
    ff(1,:,:)=ff(Nx+1,:,:)
  case('yperiodic')
    ff(:,Ny+2,:)=ff(:,2,:)
    ff(:,1,:)=ff(:,Ny+1,:)
  case('noslip_ytop')
    qq=[7,8,9]
    call noslipy_before(qq,Ny+2)
  case('noslip_ybot')
    qq=[1,2,3]
    call noslipy_before(qq,1)
  case default
  endselect
endsubroutine set_boundary_before
!***************************************************************
subroutine noslipy_before(qarray,jy)
  integer,dimension(3),intent(in) :: qarray
  integer, intent(in) :: jy 
  integer :: i,q,m,n,qnext,iq
  i=1
  do iq=2,3
    q=qarray(iq)
    m=i-ee_int(1,q)
    n=jy-ee_int(2,q)
    ff(i,jy,q)=ff(m,n,q)
  enddo
  do i=2,Nx+1
    do iq=1,3
      q=qarray(iq)
      m=i-ee_int(1,q)
      n=jy-ee_int(2,q)
      ff(i,jy,q)=ff(m,n,q)
    enddo
  enddo
  i=Nx+1
  do iq=1,2
    q=qarray(iq)
    m=i-ee_int(1,q)
    n=jy-ee_int(2,q)
    ff(i,jy,q)=ff(m,n,q)
  enddo

endsubroutine noslipy_before
!***************************************************************
subroutine set_boundary_after()
  integer,dimension(3) :: qq
  select case (bc_type)
  case('xperiodic')
  case('yperiodic')
  case('noslip_ytop')
    qq=[7,8,9]
    call noslipy_after(qq,Ny+2)
  case('noslip_ybot')
    qq=[1,2,3]
    call noslipy_after(qq,1)
  case default
  endselect
endsubroutine set_boundary_after
!***************************************************************
subroutine noslipy_after(qarray,jy)
  integer,dimension(3),intent(in) :: qarray
  integer, intent(in) :: jy
  integer :: i,q,p,iq
  i=1
  do iq=2,3
    q=qarray(iq)
    p=mirrorq(q)
    ff(i,jy,p)=ff(i,jy,q)
  enddo
  do i=2,Nx+1
    do iq=1,3
      q=qarray(iq)
      p=mirrorq(q)
      ff(i,jy,p)=ff(i,jy,q)
    enddo
  enddo
  i=Nx+2
  do iq=1,2
    q=qarray(iq)
    p=mirrorq(q)
    ff(i,jy,p)=ff(i,jy,q)
  enddo
endsubroutine noslipy_after
!***************************************************************
endmodule BoundCond
