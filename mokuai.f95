module test
implicit none
contains

    subroutine zdgsys(a, b, e)
    integer, intent(in):: a, b
    integer, intent(out):: e
    integer c, d, i

    do  i=1, min(a,b)

        c = mod(a, i)
        d = mod(b, i)
        if(c == 0 .and. d == 0)e = i 
    end do 

return
end subroutine
end module


program main
use test
implicit none

    integer x, y, z
    write(*,*),"请输入两个整数："
    read(*,*)x, y

    call zdgsys(x, y, z)

    write(*,*),"最大公约数是："
    write(*,*)z

read(*,*)
end    