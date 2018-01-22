program try 

    integer*8 :: x(100)
    real*8 :: y(100)
    
    ! ... code that calculates something in x and y ...

    open(unit=9, file="data.bin", form="unformatted")
    write(9) x
    write(9) y
    close(9)

end program try



