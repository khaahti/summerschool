program exchange
  use mpi_f08
  implicit none
  integer, parameter :: msgsize = 100000
  integer :: rc, myid, ntasks
  type(mpi_status) :: status
  integer :: message(msgsize)
  integer :: receiveBuffer(msgsize)

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  ! initialize array integer array to own rank
  message = myid

  ! TODO: Implement sending and receiving as defined in the assignment
  ! sending and receiving from 0 to 1
  if (myid == 0) then
    call mpi_recv(message, msgsize, MPI_INTEGER, 1, 10, &
	MPI_COMM_WORLD, status, rc)
  else if (myid == 1) then
    call mpi_send(receiveBuffer, msgsize, MPI_INTEGER, 0, 10, &
	MPI_COMM_WORLD, rc)
  end if

  ! sending and receiving from 1 to 0
  if (myid == 1) then
    call mpi_recv(message, msgsize, MPI_INTEGER, 0, 10, &
	 MPI_COMM_WORLD, status, rc)
  else if (myid == 0) then
    call mpi_send(receiveBuffer, msgsize, MPI_INTEGER, 1, 10, &
	 MPI_COMM_WORLD, rc)
  end if

  ! write results
  if (myid == 0) then
     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)
  else if (myid == 1) then
     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)
  end if

  call mpi_finalize(rc)

end program exchange
