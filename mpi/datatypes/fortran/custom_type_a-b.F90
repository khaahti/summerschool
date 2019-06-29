program datatype1
  use mpi_f08
  implicit none

  integer, dimension(8,8) :: array
  integer :: rank, ierr
  !TODO: declare variable for datatype
  type(mpi_datatype) :: atype, btype
  integer :: i, j
  integer, dimension(4) :: blocklens, displs

  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank ,ierr)

  ! initialize arrays
  if (rank == 0) then
     do i=1,8
        do j=1,8
           array(i,j) = i*10 + j
        end do
     end do
  else
     array(:,:) = 0
  end if

  if (rank == 0) then
     write(*,*) 'Data in rank 0'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if


  !TODO: create datatype describing one row, use mpi_type_vector
  call mpi_type_vector(8, 1, 8, MPI_INTEGER, atype, ierr)
  call mpi_type_commit(atype, ierr)

  !TODO: send first row of matrix from rank 0 to 1
  if (rank==0) then
    call mpi_send(array(2,1), 1, atype, 1, 999, MPI_COMM_WORLD, ierr)
  else if (rank==1) then
    call mpi_recv(array(2,1), 1, atype, 0, 999, MPI_COMM_WORLD, &
	MPI_STATUS_IGNORE,  ierr)
  end if

  ! Print out the result
  if (rank == 1) then
     write(*,*) 'Received data'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if

  !TODO free datatype
  call mpi_type_free(atype, ierr)

  ! initialize arrays
  if (rank == 0) then
     do i=1,8
        do j=1,8
           array(i,j) = i*10 + j
        end do
     end do
  else
     array(:,:) = 0
  end if

  if (rank == 0) then
     write(*,*) 'Data in rank 0'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if

  do i=1,4
    blocklens(i)=i
    displs(i)=(i-1)*2*8 + (i-1)
  end do 

  !TODO: create datatype describing one row, use mpi_type_vector
  call mpi_type_indexed(4, blocklens, displs, MPI_INTEGER, btype, ierr)
  call mpi_type_commit(btype, ierr)

  !TODO: send first row of matrix from rank 0 to 1
  if (rank==0) then
    call mpi_send(array, 1, btype, 1, 999, MPI_COMM_WORLD, ierr)
  else if (rank==1) then
    call mpi_recv(array, 1, btype, 0, 999, MPI_COMM_WORLD, &
	MPI_STATUS_IGNORE,  ierr)
  end if

  ! Print out the result
  if (rank == 1) then
     write(*,*) 'Received data'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if

  !TODO free datatype
  call mpi_type_free(btype, ierr)

  call mpi_finalize(ierr)

end program datatype1
