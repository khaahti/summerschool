! Main solver routines for heat equation solver
module core
  use heat

contains

  ! Exchange the boundary data between MPI tasks
  subroutine exchange(field0, parallel)
    use mpi_f08
    use omp_lib

    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(in) :: parallel
    type(mpi_status) :: status

    integer :: ierr, tid

    !$omp parallel private(tid)

    tid = omp_get_thread_num() + 1

    ! TODO start: implement halo exchange
    ! Send to left, receive from right 
    call mpi_sendrecv(field0%data(field0%displs(tid):field0%displs(tid+1)-1,1), & 
	field0%displs(tid+1)-field0%displs(tid), MPI_DOUBLE_PRECISION, &
	parallel%nleft, 999, &
	field0%data(field0%displs(tid):field0%displs(tid+1)-1,field0%ny+1), &
	field0%displs(tid+1)-field0%displs(tid), MPI_DOUBLE_PRECISION, &
	parallel%nright, 999, parallel%tcomm(tid), status, ierr) 

    ! Send to right, receive from left
    call mpi_sendrecv(field0%data(field0%displs(tid):field0%displs(tid+1)-1,field0%ny), &
	field0%displs(tid+1)-field0%displs(tid), MPI_DOUBLE_PRECISION, &
	parallel%nright, 999, & 
	field0%data(field0%displs(tid):field0%displs(tid+1)-1,0), &
	field0%displs(tid+1)-field0%displs(tid), MPI_DOUBLE_PRECISION, &
	parallel%nleft, 999, parallel%tcomm(tid), status, ierr)
    ! TODO end
    
    !$omp end parallel

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

    !$omp parallel private(i,j)
    !$omp do
    do j = 1, ny
       do i = 1, nx
          curr%data(i, j) = prev%data(i, j) + a * dt * &
               & ((prev%data(i-1, j) - 2.0 * prev%data(i, j) + &
               &   prev%data(i+1, j)) / curr%dx**2 + &
               &  (prev%data(i, j-1) - 2.0 * prev%data(i, j) + &
               &   prev%data(i, j+1)) / curr%dy**2)
       end do
    end do
    !$omp end do
    !$omp end parallel

  end subroutine evolve

end module core
