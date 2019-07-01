program pario
  use mpi_f08
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  implicit none

  integer, parameter :: datasize = 64, writer_id = 0
  integer :: rc, my_id, ntasks, localsize, i
  integer, dimension(:), allocatable :: localvector
  integer, dimension(datasize) :: fullvector

  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, my_id, rc)

  if (ntasks > 64) then
     write(error_unit, *) 'Maximum number of tasks is 64!'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  if (mod(datasize, ntasks) /= 0) then
     write(error_unit,*) 'Datasize (64) should be divisible by number of tasks'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  localsize = datasize / ntasks
  allocate(localvector(localsize))

  localvector = [(i + my_id * localsize, i=1,localsize)]

  call single_writer()

  deallocate(localvector)
  call mpi_finalize(rc)

contains

  subroutine single_writer()
    implicit none
    character(len=85) :: filename, format

    ! TODO: Implement a function that writers the whole array of elements
    !       to a file so that single process is responsible for the file io
    call mpi_gather(localvector, localsize, mpi_integer, fullvector, &
	& localsize, mpi_integer, writer_id, mpi_comm_world, rc)
    if (my_id == 0) then
!       do i=1, ntasks - 1
!           call mpi_recv(fullvector(i*localsize+1:(i+1)*localsize), &
!		localsize, MPI_INTEGER, i, 10, MPI_COMM_WORLD, &
!		MPI_STATUS_IGNORE, rc)
!       end do
       
       open(10, file='singlewriter.dat', status='replace', form='formatted', &
		& access='stream')       
       write(10,'(64(I0,1x))') fullvector
       write(output_unit,'(A,I0,A)') 'Wrote ', size(fullvector), &
		' elements to file singlewriter.dat'
       close(10)
!    else
!       write(*,*) my_id, localvector
!       call mpi_send(localvector, localsize, MPI_INTEGER, 0, 10, &
!	    MPI_COMM_WORLD, rc)
    end if

    write(filename, '(A,I0,A)') 'writer_rank', my_id, '.dat'
    write(format, '(A,I0,A)') '(', localsize, '(I0,1x))'
    open(11, file=filename, status='replace', form='formatted', &
		& access='stream')       
    write(11,format) localvector    

  end subroutine single_writer

end program pario
