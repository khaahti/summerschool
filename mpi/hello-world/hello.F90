program hello

	use mpi_f08
	implicit none
	
	integer :: mpierror, nranks, rank

	call mpi_init(mpierror)
	
	! rank
	call MPI_Comm_rank(MPI_COMM_WORLD, rank, mpierror)	
	write (*,*) "Helli world! I am rank", rank

	! number of processes
	if (rank == 0) then
	   	call MPI_Comm_size(MPI_COMM_WORLD, nranks, mpierror)
		write (*,*) "Total number of MPI processes", nranks
	end if

	call mpi_finalize()

end program hello
