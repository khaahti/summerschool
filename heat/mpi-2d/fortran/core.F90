! Main solver routines for heat equation solver
module core
  use heat

contains

  ! Exchange the boundary data between MPI tasks
  subroutine exchange_init(field0, parallel)
    use mpi_f08

    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(inout) :: parallel
    type(mpi_status) :: status(8) 
    
    integer :: ierr

    ! TODO start: implement 2D halo exchange using MPI datatypes
    ! Send to left, receive from right
    call mpi_irecv(field0%data(0,field0%ny+1), 1, parallel%columntype, &
	parallel%nright, 999, MPI_COMM_WORLD, parallel%requests(1)) 

    call mpi_isend(field0%data(0,1), 1, parallel%columntype, &
	parallel%nleft, 999, MPI_COMM_WORLD, parallel%requests(2)) 

    ! Send to right, receive from left
    call mpi_irecv(field0%data(0,0), 1, parallel%columntype, &
	parallel%nleft, 999, MPI_COMM_WORLD, parallel%requests(3))

    call mpi_isend(field0%data(0,field0%ny), 1, &
	parallel%columntype, parallel%nright, 999, MPI_COMM_WORLD, &
	parallel%requests(4))

    ! Send to up, receive from down
    call mpi_irecv(field0%data(field0%nx+1,0), 1, parallel%rowtype, &
	parallel%ndown, 999, MPI_COMM_WORLD, parallel%requests(5)) 

    call mpi_isend(field0%data(1,0), 1, parallel%rowtype, &
	parallel%nup, 999, MPI_COMM_WORLD, parallel%requests(6)) 

    ! Send to down, receive from up
    call mpi_irecv(field0%data(0,0), 1, parallel%rowtype, &
	parallel%nup, 999, MPI_COMM_WORLD, parallel%requests(7))

    call mpi_isend(field0%data(field0%nx,0), 1, &
	parallel%rowtype, parallel%ndown, 999, MPI_COMM_WORLD, &
	parallel%requests(8))

  end subroutine exchange_init

  ! Finalize the non-blocking communication
  subroutine exchange_finalize(parallel)
    use mpi_f08
    implicit none
    type(parallel_data), intent(inout) :: parallel
    integer :: ierr
    type(mpi_status) :: status(8) 

    ! TODO
    call mpi_waitall(8, parallel%requests, status)

  end subroutine exchange_finalize 

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  ! Update only the border-independent part of the field
  subroutine evolve_interior(curr, prev, a, dt)
    implicit none
    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    !$omp parallel do private(i,j)
    ! TODO
    do j = 2, ny-1
       do i = 2, nx-1
          curr%data(i, j) = prev%data(i, j) + a * dt * &
               & ((prev%data(i-1, j) - 2.0 * prev%data(i, j) + &
               &   prev%data(i+1, j)) / curr%dx**2 + &
               &  (prev%data(i, j-1) - 2.0 * prev%data(i, j) + &
               &   prev%data(i, j+1)) / curr%dy**2)
       end do
    end do
    !$omp end parallel do

  end subroutine evolve_interior

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  ! Update only the border-dependent part
  subroutine evolve_edges(curr, prev, a, dt)

    implicit none

    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    ! TODO
    !$omp parallel do private(i,j)
    ! left and right boundary
    do j = 1, ny, ny - 1
       do i = 1, nx
          curr%data(i, j) = prev%data(i, j) + a * dt * &
               & ((prev%data(i-1, j) - 2.0 * prev%data(i, j) + &
               &   prev%data(i+1, j)) / curr%dx**2 + &
               &  (prev%data(i, j-1) - 2.0 * prev%data(i, j) + &
               &   prev%data(i, j+1)) / curr%dy**2)
       end do
    end do
    !$omp end parallel do
    
    !$omp parallel do private(i,j)    
    ! upper and lower boundary
    do j = 1, ny
       do i = 1, nx, nx - 1
          curr%data(i, j) = prev%data(i, j) + a * dt * &
               & ((prev%data(i-1, j) - 2.0 * prev%data(i, j) + &
               &   prev%data(i+1, j)) / curr%dx**2 + &
               &  (prev%data(i, j-1) - 2.0 * prev%data(i, j) + &
               &   prev%data(i, j+1)) / curr%dy**2)
       end do
    end do
    !$omp end parallel do

  end subroutine evolve_edges

end module core
