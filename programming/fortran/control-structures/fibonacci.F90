program fibonacci

	! Fibonacci numbers are a sequence of integers defined 
	! by the recurrence relation Fn = Fn-1 + Fn-2 with the 
	! initial values F0=0, F1=1. Print out Fibonacci numbers 
	! Fn < 100 using a do while loop.

	integer :: Fn_minus2, Fn_minus1, Fn
	Fn_minus2 = 0
	Fn_minus1 = 1
	Fn = Fn_minus1 + Fn_minus2
	
	write (*,*) 'Fibonacci numbers:'
	do while (Fn < 100)
		write (*,*) Fn
		Fn_minus2 = Fn_minus1
		Fn_minus1 = Fn
		Fn = Fn_minus1 + Fn_minus2
	end do

end program fibonacci