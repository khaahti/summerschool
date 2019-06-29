! Main solver routines for heat equation solver
module core
  use heat

contains

  ! Exchange the boundary data between MPI tasks
  ! part 1: start communication
  subroutine exchange_init(field0, parallel)
    use mpi_f08

    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(inout) :: parallel
    integer :: ierr
    type(mpi_request) :: request(4)
    type(mpi_status) :: status(4)

    ! TODO
    ! implement halo exchange

    ! Send to left, receive from right
    call mpi_irecv(field0%data(:,field0%ny+1),field0%nx+2, MPI_DOUBLE_PRECISION, &
	parallel%nright, 999, MPI_COMM_WORLD, request(1)) 

    call mpi_isend(field0%data(:,1), field0%nx+2, MPI_DOUBLE_PRECISION, &
	parallel%nleft, 999, MPI_COMM_WORLD, request(2)) 

    ! Send to right, receive from left
    call mpi_irecv(field0%data(:,0), field0%nx+2, MPI_DOUBLE_PRECISION, &
	parallel%nleft, 999, MPI_COMM_WORLD, requests)

    call mpi_isend(field0%data(:,field0%ny), field0%nx+2, &
	MPI_DOUBLE_PRECISION, parallel%nright, 999, MPI_COMM_WORLD, request(4))

    ! TODO end

  end subroutine exchange_init

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

    ! TODO

  end subroutine evolve_interior

  ! Finalize the non-blocking communication
  subroutine exchange_finalize(parallel)
    use mpi_f08
    implicit none
    type(parallel_data), intent(inout) :: parallel
    integer :: ierr

    ! TODO
  end subroutine exchange_finalize

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

  end subroutine evolve_edges

end module core
