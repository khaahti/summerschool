program arrays
  implicit none
  ! TODO: Define the array A
  real :: x, y, dx, dy
  integer :: nx, ny, i, j, alloc_stat
  real, allocatable :: A(:,:)

  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny
  dx = 1.0/real(nx-1)
  dy = 1.0/real(ny-1)

  ! TODO: allocate the array A
  allocate (A(nx, ny))

  ! TODO: initalize the array A
  x = 0.0
  do i = 1, nx
    y = 0.0
	do j = 1, ny
		A(i,j) = x**2 + y**2
		y = y + dy
	end do
	x = x + dx
  end do

  ! TODO: Print out the array
  do i = 1, nx
     write(*, '(12F6.1)') A(i,:)
  end do

end program arrays
