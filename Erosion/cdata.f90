module Cdata
  use Sub
  implicit none
  public
!***********************!
  real,parameter :: d_pi = 3.14159265358979323846264338327950288419716939937510D+00
  integer,parameter :: labellen=25
  integer,parameter :: qmom = 9 ! we are solving the D2Q9 model
  double precision, dimension(qmom) :: weight
  double precision, parameter :: wrest=4.0d0/9.0d0, wudlr=1.0d0/9.0d0,wcorner=1.0d0/36.0d0
  integer :: Nx=16
  integer :: Ny=16
  double precision, dimension(2,qmom) :: ee ! unit vectors of the lattice
  integer, dimension(2,qmom) :: ee_int ! integer unit vectors of the lattice  
  double precision,dimension(2) :: xhat=(1.,0.),yhat=(0.,1.)
  double precision :: vunit=1.,tau=1.
  double precision :: Lx=2*d_pi,Ly=2*d_pi
  double precision :: dx=1.,dy=1.0
  double precision,allocatable, dimension(:,:,:) :: ff,fftemp,ffEq
  integer, allocatable, dimension(:,:) :: is_solid
  logical::lffaloc=.false.
!
namelist /cdata_pars/ &
     Nx,Ny,Lx,Ly,vunit,tau
!
contains
!***********************!
subroutine rparam_cdata(unit,iostat)
! read the namelist grid_pars
  integer, intent(in) :: unit
  integer, intent(out) :: iostat
  read(unit, NML=cdata_pars, IOSTAT=iostat)
  dx=Lx/Nx
  dy=Ly/Ny
! the two lines above may change depending on
! whether we are using PBC or not along a particular
! direction
endsubroutine rparam_cdata
!***********************!
subroutine allocate_cdata()
allocate(ff(Nx+2,Ny+2,qmom))
ff=1.0d0
allocate(fftemp(Nx+2,Ny+2,qmom))
fftemp=1.0d0
allocate(ffEq(Nx+2,Ny+2,qmom))
ffEq=1.0d0
lffaloc=.true.
endsubroutine allocate_cdata
!***************************************************************  
subroutine free_cdata()
  if (lffaloc .eqv. .true.) then
     deallocate(ff)
     deallocate(fftemp)
     deallocate(ffEq)
  endif
endsubroutine free_cdata
!***********************!
subroutine set_model()
  integer :: q,i,j
  q=1
     do j=-1,1; do i=-1,1
        ee_int(1,q)=i
        ee_int(2,q)=j
        ee(1,q) = real(i) 
        ee(2,q) = real(j)
        q=q+1
     enddo; enddo
  weight(1)=wcorner; weight(3)=wcorner; weight(7)=wcorner; weight(9)=wcorner
  weight(2)=wudlr ;  weight(4)=wudlr;   weight(6)=wudlr; weight(8)=wudlr
  weight(5)=wrest
endsubroutine set_model
!***********************!
endmodule Cdata
