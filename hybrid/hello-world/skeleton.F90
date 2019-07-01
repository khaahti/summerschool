program hello
  use omp_lib
  use mpi_f08
  implicit none
  integer :: my_id, tid, rc
  integer :: provided, required=MPI_THREAD_FUNNELED

  ! TODO: Initialize MPI with thread support.
  call mpi_init_thread(required, provided, rc)

  ! TODO: Find out the MPI rank and thread ID of each thread and print
  !       out the results.
  call mpi_comm_rank(MPI_COMM_WORLD, my_id)
  !$omp parallel private(tid)
    tid = omp_get_thread_num()
    write(*,'(A,I0,A, I0)') 'Hello world! Rank ', my_id, ', thread ', tid 
  !$omp end parallel

  ! TODO: Investigate the provided thread support level.
  if (provided == MPI_THREAD_MULTIPLE) then
     write(*,*) 'MPI library supports MPI_THREAD_MULTIPLE'
  else if (provided == MPI_THREAD_SERIALIZED) then
     write(*,*) 'MPI library supports MPI_THREAD_SERIALIZED'
  else if (provided == MPI_THREAD_FUNNELED) then
     write(*,*) 'MPI library supports MPI_THREAD_FUNNELED'
  else
     write(*,*) 'No multithreading support'
  end if

  call mpi_finalize(rc)
end program hello
