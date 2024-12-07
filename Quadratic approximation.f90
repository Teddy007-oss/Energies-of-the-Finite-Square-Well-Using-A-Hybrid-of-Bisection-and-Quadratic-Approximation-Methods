! Author  Emmanuel Osei Boakye
! Date    Friday 26 April 2024
! This program finds the root of a given function by implementing the quadratic approximation methods........

program quadraticapproximation
implicit none


!defining variables
real(kind = 8) :: x0,x1,x2,x3, delta, x, f, a, b, c
real(kind = 8), parameter :: tol = 3.0

!making three guesses 
x0 = 0.5
x1 = 4
x2 = 5

do

! lets evaluate a,b and c (this will be explained in a readme file)
a = ((x1 - x2) * (f(x0) - f(x2)) - (x0 - x2)*(f(x1) - f(x2))) / ((x0 - x1)*(x0 - x2)*(x1 - x2))
!print*, a

b = (((x0 - x2)**2.0)*(f(x1) - f(x2)) - ((x1 - x2)**2.0)*(f(x0) - f(x2))) / ((x0 - x1)*(x0 - x2)*(x1 - x2))
!print*, b

c = f(x2)
!print*, c


if (b .ge. 0.0) then

delta = (- 2.0*c / (b + sqrt(b**2.0 - 4*a*c)))
!print*, delta

x3 = x2 + delta

elseif (b .le. 0.0) then

delta = (2.0*c / (-b + sqrt(b**2.0 - 4*a*c)))
!print*, delta

x3 = x2 + delta

end if

x0 = x1
x1 = x2
x2 = x3

if (abs(delta) .le. tol) then

print*, x3

end if
    exit
end do

end program quadraticapproximation



function f(x)
implicit none

real(kind = 8):: f, x
f = 2*x - 6

end function
