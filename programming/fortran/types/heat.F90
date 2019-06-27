module heat
	use iso_fortran_env, only : REAL64
	implicit none	
	
	integer, parameter :: dp = REAL64
  
	type field
		integer :: nx, ny
		real(kind=dp) :: dx, dy
		real(kind=dp), dimension(:,:), allocatable :: temperature
	end type field
	
contains
	
	subroutine initialize(nx_points, ny_points, variable)
		implicit none
		
		type(field), intent(out) :: variable
		integer, intent(in) :: nx_points, ny_points
		
		variable % nx = nx_points
		variable % ny = ny_points
		variable % dx = 0.01
		variable % dy = 0.01
		allocate(variable % temperature(nx_points, ny_points))
		
	end subroutine initialize

end module heat
		