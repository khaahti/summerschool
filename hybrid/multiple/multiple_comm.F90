program communication
  use omp_lib
  use mpi_f08
 
  implicit none
  
  integer :: rc, rank_id, thread_id, i, nthreads
  integer :: provided, required=MPI_THREAD_MULTIPLE
  integer :: message
  type(mpi_comm), allocatable :: tcomm(:)

  ! initialize and check
  call mpi_init_thread(required, provided, rc)

  if (provided == required) then
   write(*,*) 'Provided thread support matches required one'
  else
   write(*,*) 'Provided thread support does not match required one!'
  end if

  !number of threads
  nthreads = omp_get_max_threads()

  ! rank
  call mpi_comm_rank(MPI_COMM_WORLD, rank_id)
  
  !communicator
  allocate(tcomm(nthreads))

  do i=1,nthreads
    call mpi_comm_split(MPI_COMM_WORLD, i, rank_id, tcomm(i), rc)
  end do

  !$omp parallel private(thread_id, message)
    thread_id = omp_get_thread_num()
    message = rank_id * 10 + thread_id
    write(*,'(A,I0,A,I0,A,I0)') 'Before: Rank ', rank_id, ', thread ', thread_id, &
	', value ', message 
    call mpi_bcast(message, 1, MPI_INTEGER, 0, tcomm(thread_id+1), rc)
    write(*,'(A,I0,A,I0,A,I0)') 'After: Rank ', rank_id, ', thread ', thread_id, &
	', value ', message 
  !$omp end parallel

  call mpi_finalize(rc)
end program communication
