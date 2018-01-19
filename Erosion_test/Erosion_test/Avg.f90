! LBM2D
!
!  calculates averages, i.e., velocity and density
!
!The distribution function ff can have the following structures:
!  Array of Structures (AoS) ff(9,Nx+2,Ny+2)
!  Structure of Arrays (SoA) ff(Nx+2,Ny+2,9)
!  Bundle                    ff(3,Nx+2,Ny+2,3)
! Accordingly to Mattila et al 2007 the last one is the fastest
! in terms of cache misses ! But as the SoA is closest to the pencil
! structures we use that now. 
!***************************************************************
module Avg
  use Cdata
  use Sub
  implicit none
  private
!------the following are public ------------
  public :: allocate_avg,calc_avg,free_avg
  public :: uu,rho
!-------------------------------------------
  double precision, allocatable, dimension(:,:,:) :: uu
  double precision, allocatable, dimension(:,:) :: rho
  logical :: lavg=.false.
contains
!***************************************************************
subroutine allocate_avg()
  allocate(uu(Nx,Ny,2))
  uu=0.
  allocate(rho(Nx,Ny))
  rho=0.
  lavg=.true.
endsubroutine allocate_avg
!***************************************************************
subroutine calc_avg()
  integer :: q
  uu=0.;rho=0.
  do q=1,qmom
     uu(:,:,1) = uu(:,:,1)+vunit*ff(2:Nx+1,2:Ny+1,q)*dot2d(ee(:,q),xhat)
     uu(:,:,2) = uu(:,:,2)+vunit*ff(2:Nx+1,2:Ny+1,q)*dot2d(ee(:,q),yhat)
     rho(:,:) = rho(:,:) + ff(2:Nx+1,2:Ny+1,q)
  enddo
  uu(:,:,1)=uu(:,:,1)/rho(:,:)
  uu(:,:,2)=uu(:,:,2)/rho(:,:)
endsubroutine calc_avg
!***************************************************************  
subroutine free_avg()
  if (lavg .eqv. .true.) then
     deallocate(uu); deallocate(rho)
  endif
endsubroutine free_avg
!***************************************************************  
endmodule Avg
  
