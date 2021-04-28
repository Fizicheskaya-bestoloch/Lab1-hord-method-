module dlyahord

contains
subroutine DD(f, x1, xk, i, eps, xa, xb)
implicit none
real(8) :: f, x1, x0, xa, xb, xk, eps, x2  
integer, parameter :: imax = 100
integer :: i
i = 1
x1 = xa
x0 = xb


x2 = x1 - (f(x1) * (x1 - x0))/(f(x1) - f(x0))
write (*,*) 'x2 = ', x2, 'f(x2) = ', f(x2)
do while (( abs(x2 - x1) .GT. eps) .and. (i .LT. imax))  
x1 = x2
x2 = x1 - (f(x1) * (x1 - x0))/(f(x1) - f(x0))
i = i + 1

write (*,*) 'x2 = ', x2, 'f(x2) = ', f(x2) 

end do

xk = (x2+x1)/2.0
end subroutine DD
end module dlyahord

program hord
use dlyahord
implicit none
real(8) xa, xb, eps, xk, x0, x1
integer :: i  

interface

	real(8) function f(x)
	real (8) :: x
	end function f

end interface


write (*,*) 'enter xa' 
read (*,*) xa

write (*,*) 'enter xb'
read (*,*) xb

write (*,*) 'enter eps' 	
read (*,*) eps



call DD(f, x1, xk, i, eps, xa, xb)


write (*,*) 'xk = ', xk
write (*,*) 'f(xk) = ', f(xk)
write (*,*) 'i =', i
pause 
end program				

real(8) function f(x)
implicit none
real(8) :: x
f = x**4-3*x**2+75*x-9999
end function f
