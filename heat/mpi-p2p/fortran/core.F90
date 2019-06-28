! Main solver routines for heat equation solver
module core
  use heat

contains

  ! Exchange the boundary data between MPI tasks
  subroutine exchange(field0, parallel)
    use mpi_f08

    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(in) :: parallel
    type(mpi_status) :: status

    integer :: ierr

    ! TODO start: implement halo exchange
    ! Send to left, receive from righta
    call mpi_sendrecv(field0%data(1,1:field0%ny), field0%ny, MPI_REAL, &
	parallel%nleft, parallel%nleft, &
	field0%data(field0%nx+1,1:field0%ny),field0%ny, MPI_REAL, &
	parallel%nright, parallel%rank, MPI_COMM_WORLD, status) 

    ! Send to right, receive from left
    call mpi_sendrecv(field0%data(field0%nx,1:field0%ny), field0%ny, MPI_REAL, &
	parallel%nleft, parallel%nright, field0%data(0,1:field0%ny), &
	field0%ny, MPI_REAL, parallel%nleft, parallel%rank, &	
	MPI_COMM_WORLD, status)
    ! TODO end

  end subroutine exchange

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  subroutine evolve(curr, prev, a, dt)

    implicit none

    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    do j = 1, ny
       do i = 1, nx
          curr%data(i, j) = prev%data(i, j) + a * dt * &
               & ((prev%data(i-1, j) - 2.0 * prev%data(i, j) + &
               &   prev%data(i+1, j)) / curr%dx**2 + &
               &  (prev%data(i, j-1) - 2.0 * prev%data(i, j) + &
               &   prev%data(i, j+1)) / curr%dy**2)
       end do
    end do
  end subroutine evolve

end module core
