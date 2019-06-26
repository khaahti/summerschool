program loops
  implicit none
  
  ! define parameters nx and ny
  integer, parameter :: nx = 10, ny = 10

  ! define real-valued array A
  real :: A(nx, ny)
  
  integer :: i, j

  ! initialize array A here
  real :: x, y
  x = 0.0
  do i = 1, nx
    y = 0.0
	do j = 1, ny
		A(i,j) = x**2 + y**2
		y = y + 1.0 / (ny - 1)
	end do
	x = x + 1.0 / (nx - 1)
  end do

  !--------------------------------------------------
  ! Print out the array
  ! the ':' syntax means the whole row, see the Fortran arrays lecture
  do i = 1, nx
     write(*, '(12F6.1)') A(i,:)
  end do

end program loops
