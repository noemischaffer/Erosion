module BoundCond
use Cdata
use Avg
use messages
implicit none
private
public :: initialize_bc, set_boundary_before,set_boundary_after
character (len=labellen) :: bc_left='periodic',bc_right='periodic'
character (len=labellen) :: bc_bot='noslip',bc_top='noslip'
! It is left, right, bottom, top
namelist /bc_pars/ &
   bc_left,bc_right,bc_bot,bc_top

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
subroutine initialize_bc()
! Boundary types: rb, lb, tb, bb
  integer :: ibc
  if (bc_left.eq.'periodic') then
    if (bc_right.eq.'periodic') then
      dx=Lx/Nx
    else
    call fatal_error('initialize_bc','bc left and right dont match')
   endif 
  endif
  if (bc_bot.eq.'periodic') then
    if (bc_top.eq.'periodic') then
      dy=Ly/Ny
    else
    call fatal_error('initialize_bc','bc bot and top dont match')
   endif 
  endif
endsubroutine initialize_bc
!***************************************************************
subroutine set_boundary_before()
  integer :: i,q,m,n,qnext
  integer,dimension(3) :: qq 
  integer :: ibc

!
! left boundary
!
  select case(bc_left)
     case('periodic')
     ! write(*,*) 'periodic x'
      ff(Nx+2,:,:)=ff(2,:,:)
      ff(1,:,:)=ff(Nx+1,:,:)
     ! write(*,*) ff(Nx+2,4,4),ff(2,4,4)
     ! write(*,*) ff(1,4,4),ff(Nx+1,4,4)
     ! write(*,*) '***********'
     case default
       call fatal_error('boundary before','nothing other than pbc coded') 
  endselect

!
! bc_right should go here once we have something
! other than pbc.
!
!
!
! bottom boundary
!
  select case(bc_bot)
  case('periodic')
    ff(:,Ny+2,:)=ff(:,2,:)
    ff(:,1,:)=ff(:,Ny+1,:)
  case('noslip')
    qq=[1,2,3]
    call noslipy_before(qq,1)
  case default
      call fatal_error('boundary before','bc not found')
  endselect
!
! top boundary
!
  select case(bc_top)
    case('noslip')
     qq=[7,8,9]
     call noslipy_before(qq,Ny+2)
  case default
      call fatal_error('boundary before','bc not found')
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

!
! bottom boundary
!
  select case(bc_bot)
  case('noslip')
    qq=[1,2,3]
    call noslipy_after(qq,1)
  case default
      call fatal_error('boundary after','bc not found')
  endselect
!
! top boundary
!
  select case(bc_top)
    case('noslip')
     qq=[7,8,9]
     call noslipy_after(qq,Ny+2)
    case default
      call fatal_error('boundary after','bc not found')
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
