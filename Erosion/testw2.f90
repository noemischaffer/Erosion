program test
use cdata
integer::q
call set_model()

do q=1,qmom
write(*,*), ee(:,q), weight(q)
enddo
endprogram
