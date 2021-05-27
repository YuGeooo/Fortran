program main
implicit none

real:: x, n, y, m, k, a
n = 1
y = 0                               ! y = sin x
m = 1
a = 2                               ! a-2为展开项数

write(*,*), "请输入角度（单位：度）"
read(*,*)x
x = x * atan(1.) * 4 / 180          ! 角度转换为弧度
write(*,*),"  展开项数         展开式结果      sin(x)函数计算结果"
write(*,*),""

do while(a < 12)                    ! 这一层用于显示展开项的多少对精度的影响
        do while(n < a)             ! 这一层计算累加
            k = 2 * n - 1
                do while(k > 1)     ! 这一层计算(2n-1)的阶乘
                    m = m * k
                    k = k - 1    
                end do
            y = y + ((-1) ** (n - 1)) * (x ** (2 * n - 1)) / m
            n = n + 1
            m = 1
        end do
    a = a + 1
    write(*,*)a - 2, y, sin(x)
    write(*,*),""
end do

read(*,*)
end
