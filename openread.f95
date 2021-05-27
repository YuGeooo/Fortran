program main
implicit none

integer :: stat = 0                             !stat = 0表示文件操作成功
character(len = 80) :: msg = "ok"                        !msg返回错误信息
integer :: p, h
real, dimension(12):: po

real, dimension(13):: ho

open(100, file = 'data-asci-seq.dat', &
       status = 'old', action = 'read', &
       form = 'formatted', access = 'sequential', &
       iostat = stat, iomsg = msg, recl = 78)
    write(*,*)msg
open(101, file = 'data-binary.dat', &
        status = 'replace', action = 'write', position = 'append', &
        form = 'unformatted', access = 'sequential', &
        iostat = stat, iomsg = msg)    
    
    do      
        write(*,*)"---------------"
        if (stat /= 0) exit   
        read(100, '(i6, 12f6.2)', iostat = stat, iomsg = msg)p, po
        write(*,*)msg
        write(*,'(i6, 12f6.2)')p, po
        write(101, iomsg = msg)p, po    
        write(*,*)msg
    end do 
    write(*,*)msg

close(101)
close(100) 

write(*,*)"--------"

open(101, file = 'data-binary.dat', &
        status = 'old', action = 'read', &
        form = 'unformatted', access = 'direct', &
        iostat = stat, iomsg = msg, recl = 52)

    read(101, iostat = stat, iomsg = msg, rec = 1)ho
    write(*,*)msg
    write(*,'(13f6.2)', iomsg = msg)ho
    write(*,*)msg

close(101)

read(*,*)
end