program main
implicit none

integer, parameter :: y = 10

integer, dimension(y) :: x 
integer z, p, q, w


write(*,*),"输入4个数"
read(*,*)x

do q = 1, y
    do p = 1, y - 1
        if(x(p) > x(p + 1))then
            w = x(p)
            x(p) = x(p + 1)
            x(p + 1) = w 
        end if 
    end do 
end do

if( mod(y, 2) == 0 )then
    write(*,"(' ', f5.1)")real(( x(y / 2) + x(y / 2 + 1) ))/2
else  
    write(*,*)x((y + 1)/2)
end if

read(*,*)
end