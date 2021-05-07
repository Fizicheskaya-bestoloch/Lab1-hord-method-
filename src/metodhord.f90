module dlyahord

contains
subroutine DD(f, x1, xk, i, eps, xa, xb)
implicit none
real(8) :: f, x1, x0, xa, xb, xk, eps, x2, f1, f0
integer, parameter :: imax = 100
integer :: i
i = 1
x1 = xa
x0 = xb

f0 = f(x0)
x2 = x1 - (f(x1) * (x1 - x0))/(f(x1) - f0)
write (*,*) 'x2 = ', x2, 'f(x2) = ', f(x2)
do while ((abs(x2 - x1) .GT. eps) .and. (i .LE. imax)) 
	x1 = x2
	f1 = f(x1)
	x2 = x1 - (f1 * (x1 - x0))/(f1 - f0)
	i = i + 1
	write (*,*) 'x2 = ', x2, 'f(x2) = ', f(x2)
end do

xk = (x2+x1)/2.0
end subroutine DD
end module dlyahord
program hord
use dlyahord
implicit none
real(8) xa, xb, eps, xk, x0, x1, m1, fp1, h
integer :: i, imax

interface

	real(8) function f(x)
	real (8) :: x
	end function f
	
	real(8) function f1p(u)
	real (8) :: u
	end function f1p
	
	real(8) function f2p(z)
	real (8) :: z
	end function f2p
end interface


write (*,*) 'enter xa' 
read (*,*) xa

write (*,*) 'enter xb'
read (*,*) xb

write (*,*) 'enter eps' 	
read (*,*) eps

imax = 100
x0=xa
h=abs(xb-xa)/(imax-1)
m1=f1p(x0)
do i=1,imax !проверка на применимость метода хорд (производная не должна менять знак на промежутке)
	x1=x0+h*i
	if (f2p(x1)*f2p(x0) .LT. 0) then
		print*, 'wrong promejutok, vtoraya proizvodnaya menyaet znak'
		stop
	end if
	x0=x1
	fp1=f1p(x1)
	if (abs(fp1) .LT. m1) then !вычисление m1=inf|f'(x)| для оценки точности
		m1=fp1
	end if
end do


call DD(f, x1, xk, i, eps, xa, xb)


write (*,*) 'f(xk) = ', f(xk)
write (*,*) 'i =', i
write(*,*) 'Ocenka tochnosti:', abs(f(xk))/m1 !|xk-xotvet|<=|f(xk)|/m1
write (*,*) 'xk = ', xk
pause 
end program				

real(8) function f(x) !сама функция
implicit none
real(8) :: x
f = x**4-3*x**2+75*x-9999
end function f

real(8) function f1p(u) !первая производная функции
implicit none
real(8) :: u
f1p = 4*u**3-6*u+75
end function f1p

real(8) function f2p(z) !вторая производная функции
implicit none
real(8) :: z
f2p = 12*z**2-6
end function f2p
