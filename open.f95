program main
implicit none

character(len = 20) :: filename = "new-file.txt"
integer :: stat = 0                             !stat = 0表示文件操作成功
character(len = 80) msg                         !返回错误信息
character(len = 50)po1, po2, po3, po4, po

!       文档内容        !
po1 = "To see a world in a grain of sand"
po2 = "And a heaven in a wild flower"
po3 = "Hold infinity in the plam of your hand"
po4 = "And eternity in an hour"

!       写入文档        !
open(unit = 100, file = filename, &
        status = 'replace', action = 'write', &
        iostat = stat, iomsg = msg)
    
    write(unit = 100, fmt = *, &
            iostat = stat, iomsg = msg)po1
    write(unit = 100, fmt = *, &
            iostat = stat, iomsg = msg)po2
    write(unit = 100, fmt = *, &
            iostat = stat, iomsg = msg)po3
    write(unit = 100, fmt = *, &
            iostat = stat, iomsg = msg)po4
close(100)

!       读取文档        !
open(unit = 100, file = filename, &
       status = 'old', action = 'read', &
       iostat = stat, iomsg = msg)
    
    do          !循环读取并输出文档内容。特别注意格式fmt = '(a50)'，若fmt = * 则只读出第一个单词
    read(unit = 100, fmt = '(a50)', iostat = stat, iomsg = msg)po
    if (stat /= 0) exit
    write(*,'(a50)') po
    end do 
close(100) 
!write(*,*)msg

read(*,*)
end
