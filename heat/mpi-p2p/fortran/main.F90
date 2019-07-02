! Heat equation solver in 2D.

program heat_solve
  use heat
  use core
  use io
  use setup
  use utilities
  use mpi_f08

  implicit none

  real(dp), parameter :: a = 0.5 ! Diffusion constant
  type(field) :: current, previous    ! Current and previus temperature fields

  real(dp) :: dt     ! Time step
  integer :: nsteps       ! Number of time steps
  integer, parameter :: image_interval = 100 ! Image output interval

  type(parallel_data) :: parallelization
  integer :: provided, required=MPI_THREAD_MULTIPLE
  integer :: ierr

  integer :: iter, nthreads, rank_id, i

  real(kind=dp) :: start, stop ! Timers

  ! TODO start: initialize MPI
  call mpi_init_thread(required, provided, ierr)
  ! TODO end
  ! check
  if (provided == required) then
    write(*,*) 'Provided thread support matches required one'
  else
    write(*,*) 'Provided thread support does not match required one!'
  end if

  call initialize(current, previous, nsteps, parallelization)

  !number of threads
  nthreads = omp_get_max_threads()

  !communicator
  allocate(parallelization%tcomm(nthreads))

  allocate(previous%displs(nthreads+1))
  do i=1,nthreads
    call mpi_comm_split(MPI_COMM_WORLD, i, rank_id, parallelization%tcomm(i), ierr)
    previous%displs(i) = (i-1)*(current%nx+2)/nthreads+1  
  end do
  previous%displs(nthreads+1) = current%nx+2

  ! Draw the picture of the initial state
  call write_field(current, 0, parallelization)

  ! Largest stable time step
  dt = current%dx**2 * current%dy**2 / &
       & (2.0 * a * (current%dx**2 + current%dy**2))

  ! Main iteration loop, save a picture every
  ! image_interval steps

  start =  mpi_wtime()

  do iter = 1, nsteps
     call exchange(previous, parallelization)
     call evolve(current, previous, a, dt)
     if (mod(iter, image_interval) == 0) then
        call write_field(current, iter, parallelization)
     end if
     call swap_fields(current, previous)
  end do

  stop = mpi_wtime()

  if (parallelization % rank == 0) then
     write(*,'(A,F7.3,A)') 'Iteration took ', stop - start, ' seconds.'
     write(*,'(A,G0)') 'Reference value at 5,5: ', previous % data(5,5)
  end if

  call finalize(current, previous)

  ! TODO start: finalize MPI
  call mpi_finalize(ierr)
  ! TODO end

end program heat_solve
