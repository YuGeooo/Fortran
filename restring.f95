subroutine restring(string1)
implicit none

character(len = 80)string1, string2
integer i, a

a = len_trim(string1)
!string1 = "hello world"
!write(*,*)string1

string2 = string1

do i=1,a
string1(i : i) = string2(a + 1 - i : a + 1 - i)
end do

!write(*,*)string2

!return
read(*,*)
end subroutine
