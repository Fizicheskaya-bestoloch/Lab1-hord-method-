module modu
	contains
	subroutine prog(A,B,C,D,X,Imax)
	implicit none
	integer :: Imax,i
	real(8) :: A(Imax), B(Imax), C(Imax), D(Imax), X(Imax),alf(Imax), bet(Imax)
	!____________________________________________________________Прямой ход
	alf(1)=-C(1)/B(1)
	bet(1)=D(1)/B(1)
	i=2
	do while (i .LE. (Imax-1))!цикл вычислеия коэффициентов альфа и бета
		alf(i)=-(C(i)/(B(i)+A(i)*alf(i-1)))
		bet(i)=(D(i)-A(i)*bet(i-1))/(B(i)+A(i)*alf(i-1))
		i = i + 1
	end do
	!____________________________________________________________Обратный ход
	X(Imax)=(D(Imax)-A(Imax)*bet(Imax-1))/(B(Imax)+A(Imax)*alf(Imax-1))
	i=Imax-1
	do while (i .GE. 1) ! цикл вычисления X(i)
		X(i)=alf(i)*X(i+1)+bet(i)
		i = i - 1
	end do
	end subroutine
end module
program metod_progonki
use modu
implicit none
integer, parameter :: Imax=5
integer :: i
real(8) :: A(Imax), B(Imax), C(Imax), D(Imax), X(Imax)

call random_number(A) !заполнение матриц псевдослучайными числами
call random_number(B)
call random_number(C)
call random_number(D)
i=1
A(1)=0
C(Imax)=0
i = 1

call prog(A,B,C,D,X,Imax)

do i =1,Imax
	write(*,'(a3,i2,a5,f8.5)')'X(',i,') = ',X(i)
end do

end program
