program check
	implicit none

	! Write a control structure which checks whether an integer is 
	! negative, zero, or larger than 100 and performs corresponding 
	! write. Investigate the behavior with different integer values.
	
	integer :: a
	write (*,*) 'Give integer:'
	read (*,*) a
	
	if (a < 0) then
		write (*,*) 'Given integer is negative'
	else if (a == 0) then
		write (*,*) 'Given integer is zero'
	else if (a > 100) then
		write (*,*) 'Given integer is greater than 100'
	else
		write (*,*) 'Integer does not fit criteria'
	end if
	
end program check
