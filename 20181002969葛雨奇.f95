program main
implicit none

type dat
integer num, latitude, longitude, hight, year, month, average, maximum, maximumdir, maximal, maximaldir
end type 

integer :: stat = 0                             
character(len = 80) msg
type(dat), dimension(63,12):: information
integer, dimension(63):: abc
integer :: i, k, m      !   i是年份循环, 1~63; k是月份循环, 1~12; m是风向循环, 1~16.
integer :: n
real :: med1, med2
real, dimension(16) :: direction, frequency

open(100, file = 'SURF_CLI_CHN_MUL_MON-WIN-59287.txt', status = 'old', action = 'read', iostat = stat, iomsg = msg)
    
    !   information(i,k)代表 (i + 1950) 年 k 月的数据   !
    do i = 1, 63
        do k = 1, 12
            read(100, fmt = *, iostat = stat, iomsg = msg)&
                information(i,k)%num, information(i,k)%latitude, information(i,k)%longitude,&
                information(i,k)%hight, information(i,k)%year, information(i,k)%month, information(i,k)%average,&
                information(i,k)%maximum, information(i,k)%maximumdir, information(i,k)%maximal, information(i,k)%maximaldir   
        end do
    end do

close(100)

open(101, file = '20181002969葛雨奇.txt', status = 'replace', action = 'write', iostat = stat, iomsg = msg)

1003    format(a7, 2a20, 16a22)
1004    format(i3, 2f19.1, 16f22.3)

write(101,1003),"月份", "平均风速中值", "最大风速中值", "1风向方位频率",&
             "2风向方位频率", "3风向方位频率",&
            "4风向方位频率", "5风向方位频率", "6风向方位频率", "7风向方位频率",&
             "8风向方位频率", "9风向方位频率",&
            "10风向方位频率", "11风向方位频率", "12风向方位频率", "13风向方位频率",&
             "14风向方位频率", "15风向方位频率", "16风向方位频率"
write(101,*),""

do k = 1, 12

    !   计算平均风速的中位数. abc储存平均风速数据, n统计缺失年数.   !
    n = 0
    do i = 1, 63        
        abc(i) = information(i,k)%average
        if(abc(i) == 32766)then
            n = n + 1 
        end if
    end do 
    call median(abc, 63, n, med1)

    !   计算最大风速的中位数. abc储存最大风速数据, n统计缺失年数.   !
    n = 0
    do i = 1, 63        
        abc(i) = information(i,k)%maximum
        if(abc(i) == 32766)then
            n = n + 1 
        end if
    end do 
    call median(abc, 63, n, med2)

    !   计算最大风速的16方位风向出现的频率   !
    do m = 1, 16
    direction(m) = 0
    end do

    do i = 1, 63
        do m = 1, 16
            if(information(i,k)%maximumdir == m)then
            direction(m) =direction(m) + 1
            end if 
        end do
    end do

    do m = 1, 16
        frequency(m) = direction(m)/sum(direction)
    end do 

    write(101,1004)k, med1, med2, frequency

end do
close(101)
end


!   用于求数组中位数的子程序. s是数组名, y是数组长度, t是缺失年数   !
subroutine median(s, y, t, z)
    
    implicit none 
    integer, intent(in) :: y, t
    integer, intent(in) :: s(y)
    real, intent(out) :: z
    integer p, q, w
    integer, dimension(y):: x
    x = s

    !   对数组从小到大重新排序   !
    do q = 1, y
        do p = 1, y - 1
            if(x(p) > x(p + 1))then
                w = x(p)
                x(p) = x(p + 1)
                x(p + 1) = w 
            end if 
        end do 
    end do

    !   排序后缺失数据 "32766" 排在数组末尾, 用 (y - t) 排除影响选出中值   !
    if( mod(y, 2) == 0 )then
        z = real(( x((y - t) / 2) + x((y - t) / 2 + 1) ))/2
    else  
        z = real(x(((y - t) + 1)/2))
    end if

end subroutine