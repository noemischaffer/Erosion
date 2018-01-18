module calc_test
implicit none
public
  integer,parameter :: qmom = 9 ! we are solving the D2Q9 model
  double precision, dimension(qmom) :: weight
  double precision, parameter :: wrest=4.0d0/9.0d0, wudlr=1.0d0/9.0d0,wcorner=1.0d0/36.0d0
  double precision, dimension(2,qmom) :: ee ! unit vectors of the lattice
  integer, dimension(2,qmom) :: ee_int ! integer unit vectors of the lattice  
contains

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
  weight(1)=wcorner; weight(3)=wcorner; weight(7)=wcorner;weight(9)=wcorner
  weight(2)=wudlr ;  weight(4)=wudlr;   weight(6)=wudlr; weight(8)=wudlr
  weight(5)=wrest
endsubroutine set_model
 
endmodule


program test
use calc_test
integer::q
call set_model()

do q=1,qmom
write(*,*), ee(:,q), weight(q)
enddo
endprogram
