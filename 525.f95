program main
implicit none

character(len = 80)string

write(*,*),"请输入字符"

read(*,*)string
call restring(string)
write(*,*)string

end