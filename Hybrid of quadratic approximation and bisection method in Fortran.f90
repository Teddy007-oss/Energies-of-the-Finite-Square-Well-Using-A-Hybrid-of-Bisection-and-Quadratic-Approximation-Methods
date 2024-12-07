
! Author Emmanuel Osei Boakye
! Date 3rd May 2024 Friday
! A script the execute the hybrid of the bisection and the quadratic approximation method in finding the root f an equation.

program hybrid
	implicit none
	
	!declaring variables
	real(kind = 8):: xl,xr,a,b,c,x0,x1,x2,x3,f,x, delta, root_tester,error,error1
	real(kind = 8), parameter :: tol = 1e-12	
		
	!selecting an the interval for the program
	xl =-0.1
	xr =-2.8
	
	!selecting three values of x that lies between the interval already selected(thus xl and xr)
	x0=-0.2
	x1=-1.6
	x2=-2.7

!Attemping the root by method of quadratic approximation 	
	Do	
		! let evaluate a,b and c (this will be explained in a readme file)
		a = ( (x1 - x2) * (f(x0) - f(x2)) - (x0 - x2)*(f(x1) - f(x2)) ) / ( (x0 - x1)*(x0 - x2)*(x1 - x2) )
		b = ( ((x0 - x2)**2.0 )*(f(x1) - f(x2)) - ( (x1 - x2)**2.0 )*(f(x0) - f(x2)) ) / ( (x0 - x1)*(x0 - x2)*(x1 - x2) )
		c = f(x2) 
					
		if (b .ge. 0) then	
			delta = (- 2.0*c / (b + sqrt(b**2.0 - 4*a*c)))
			root_tester = ((x2 - xl)*(b+sqrt(b**2 - 4*a*c) -2*c)) * ((x2 - xr)*(b + sqrt(b**2 - 4*a*C) - 2*c) )
			
			if (root_tester .gt. 0) exit
			!the idea here is that if the root_tester is less than or equal to zero then x3(our calculated root) lies between x1 and x2 as required from the manual.
			!otherwise, the code should exit and begin bisection.
			
		else 
			delta = (2.0*c / (-b + sqrt(b**2.0 - 4*a*c)))
			root_tester = ((xl - x2)*(-b + sqrt(b**2 - 4*a*c) -2*c)) * ((xr - x2)*(-b + sqrt(b**2 - 4*a*C) - 2*c))
				
			if (root_tester .gt. 0) exit
			!the idea here is that if the root_tester is less than or equal to zero then x3(our calculated root) lies between x1 and x2 as required from the manual.
			!otherwise, the code should exit and begin bisection.
			
		end if
		
		x3 = x2 + delta
		print*, x3
		
		x0 = x1
		x1 = x2
		x2 = x3
		
		if (abs(delta) .le. tol) exit 
	end do	
	
!Finding the root by method of bisection	
	Do while (error .gt. tol)
		x3 = (xl + xr)/ 2.0
		print*, x3
			
		if (f(xl)*f(x3) .le. 0)  then
			xr = x3
				
		else 
			xl = x3	
				
		end if	
			error1 = x3
			x3 = (xl + xr)/2.0
			
			error = abs(error1 - x3)		
	end do




end program hybrid 


   function f(x)
		implicit none
		
		real(kind = 8):: f, x
		f = 5*x**2 + 12*x + 7 
		
   end function

