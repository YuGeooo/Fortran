program main
implicit none

integer :: stat = 0                             
character(len = 80) msg                         
integer num, wei, jin, hight, year, month, a, b, c
integer :: i = 0                                
integer, dimension(12):: average = (/0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/)
integer, dimension(12):: most = (/0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/)
integer, dimension(12):: mini = (/9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999/)
integer, dimension(12):: mostyear, miniyear

open(100, file = 'SURF_CLI_CHN_MUL_MON-PRE-57494.txt', status = 'old', action = 'read', iostat = stat, iomsg = msg)
do
    i = i + 1                                   ! i代表年份，从1到63
    read(100, fmt = *, iostat = stat, iomsg = msg)num, wei, jin, hight, year, month, a, b, c
    if (stat /= 0) exit

    average(month) = average(month) + c

    !   计算最大降水量    !
    if(most(month) < c)then 
        most(month) = c
        mostyear(month) = year
    end if

    !   计算最小降水量    !
    if(mini(month) > c)then 
        mini(month) = c
        miniyear(month) = year
    end if

end do
close(100)

do i = 1, 12
    average(i) = average(i) / 63
end do

open(101, file = '57494-wuhan-mon-cli-pre.txt', status = 'replace', action = 'write', iostat = stat, iomsg = msg)

    !   输出平均降水量    !
    write(101, fmt = *, iostat = stat, iomsg = msg),"各月平均"
    write(101, fmt = *, iostat = stat, iomsg = msg),"           月份   降水量"
    do i = 1, 12
        write(101, fmt = *, iostat = stat, iomsg = msg)i, average(i)
    end do

    !   输出最大降水量    !
    write(101, fmt = *, iostat = stat, iomsg = msg),""
    write(101, fmt = *, iostat = stat, iomsg = msg),"最大降水"
    write(101, fmt = *, iostat = stat, iomsg = msg),"           月份   降水量       年"
    do i = 1, 12
        write(101, fmt = *, iostat = stat, iomsg = msg)i, most(i), mostyear(i)
    end do

    !   输出最小降水量    !
    write(101, fmt = *, iostat = stat, iomsg = msg),""
    write(101, fmt = *, iostat = stat, iomsg = msg),"最小降水"
    write(101, fmt = *, iostat = stat, iomsg = msg),"           月份   降水量       年"
    do i = 1, 12
        write(101, fmt = *, iostat = stat, iomsg = msg)i, mini(i), miniyear(i)
    end do

close(101)

end