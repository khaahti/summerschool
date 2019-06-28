program basic
  use mpi_f08
  use iso_fortran_env, only : REAL64

  implicit none
  integer, parameter :: msgsize = 10000
  integer :: rc, myid, myidleft, myidrigth, ntasks
  integer :: message(msgsize)
  integer :: receiveBuffer(msgsize)
  type(mpi_status) :: status

  real(REAL64) :: t0, t1

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid

  ! Start measuring the time spent in communication
  call mpi_barrier(mpi_comm_world, rc)
  t0 = mpi_wtime()

  ! TODO: Send and receive as defined in the assignment

  ! id of neigthbourgs
  myidleft = myid - 1
  myidrigth = myid + 1
  if (myidrigth >= ntasks) then
    myidrigth = MPI_PROC_NULL
  end if 
  if (myidleft < 0) then
    myidleft = MPI_PROC_NULL
  end if

  ! ensin parittomille parillisille
  if (mod(myid,2) == 0) then
    call mpi_recv(receiveBuffer, msgsize, MPI_INTEGER, myidleft, myid, &
	MPI_COMM_WORLD, status)
  else
    call mpi_send(message, msgsize, MPI_INTEGER, myidrigth, myid + 1, &
	MPI_COMM_WORLD)
  end if

  ! sitten parillisilta parittomille
  if (mod(myid,2) == 1) then
    call mpi_recv(receiveBuffer, msgsize, MPI_INTEGER, myidleft, myid, &
 	MPI_COMM_WORLD, status)
  else
    call mpi_send(message, msgsize, MPI_INTEGER, myidrigth, myid + 1, &
	MPI_COMM_WORLD)
  end if

  if (myid < ntasks-1) then
     write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
          ' Sent elements: ', msgsize, &
          '. Tag: ', myid+1, '. Receiver: ', myid+1
  end if

  if (myid > 0) then
     write(*,'(A10,I3,A,I3)') 'Receiver: ', myid, &
          ' First element: ', receiveBuffer(1)
  end if

  ! Finalize measuring the time and print it out
  t1 = mpi_wtime()
  call mpi_barrier(mpi_comm_world, rc)
  call flush(6)

  write(*, '(A20, I3, A, F6.3)') 'Time elapsed in rank', myid, ':', t1-t0

  call mpi_finalize(rc)

end program basic
