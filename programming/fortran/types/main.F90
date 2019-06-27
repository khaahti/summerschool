program main
  use heat
  implicit none
  type(field) :: field0
  integer :: nx, ny, i

  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny

  call initialize(nx, ny, field0)
  
  write(*,*) "Array:"
  do i = 1, size(field0 % temperature, dim=1)
	 write(*, '(10F6.3)') field0 % temperature(i,:)
  end do
  
  write(*,*) "nx: ", field0 % nx
  write(*,*) "ny: ", field0 % ny
  write(*,*) "dx: ", field0 % dx
  write(*,*) "dy: ", field0 % dy
 
end program main
