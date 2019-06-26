module laplacian_mod
  implicit none
  real, parameter :: dx = 0.1, dy = 0.1

contains

  subroutine initialize(field0)
  	  implicit none
	  ! TODO: implement a subroutine that initializes the input array
	  real, dimension(:,:), intent(out) :: field0
	  integer :: nx, ny, i, j
	  real :: x, y
	  
	  nx = size(field0, dim=1)
	  ny = size(field0, dim=2)
	  
	  y = 0.0
	  do j = 1, ny
		 x = 0.0
		 do i = 1, nx
			field0(i,j) =  x**2 + y**2
			x = x + dx
		 end do
		 y = y + dy
	  end do	  
	  
  end subroutine initialize

  subroutine laplacian(curr, prev)
      ! TODO: insert a subroutine that computes a laplacian of the
      ! array "prev" and returns it as an array "curr"
	  implicit none
	  
	  real, dimension(:,:), intent(in) :: prev
	  real, dimension(:,:), intent(out) :: curr
	  integer :: nx, ny, i, j
	  
	  nx = size(prev, dim=1)
	  ny = size(prev, dim=2)

	  ! boundaries get value zero
	  curr = 0.0
	  
	  ! mid section
	  do j = 2, ny-1
		 do i = 2, nx-1
			curr(i,j) = (prev(i-1,j) - 2*prev(i,j) + prev(i+1,j)) / (dx)**2 &
				+ (prev(i,j-1) - 2*prev(i,j) + prev(i,j+1)) / (dy)**2
		 end do
	  end do

  end subroutine laplacian

  subroutine write_field(array)
      implicit none
	  
	  real, dimension(:,:), intent(in) :: array
	  integer :: i
	 
      ! TODO: write a subroutine that prints "array" on screen
	  write(*,*) "Array:"
	  do i = 1, size(array, dim=1)
		 write(*, '(12F6.1)') array(i,:)
	  end do
  end subroutine write_field

end module laplacian_mod
