module io

contains

  ! Reads the temperature distribution from an input file
  subroutine read_field(field, filename)
    implicit none

    real, dimension(:,:), allocatable, intent(out) :: field
    character(len=*), intent(in) :: filename
	
	integer :: nx, ny, i

    ! TODO: implement function that will:
    ! open the file
	open(10, file=filename, status='old')
	
    ! read the first header line to get nx and ny (header format "# nx ny"
	read(10, fmt='(2x,i4,i4)') nx, ny
	write(*,*) nx
	write(*,*) ny
	
    ! allocate matrix called field
	allocate(field(nx,ny))
	
    ! read rest of the file into field
	do i = 1, nx
		 read(10,*) field(i,:)
	end do
	
    ! close the file
	close(10)
	
  end subroutine read_field

  ! Output routine, saves the temperature distribution as a png image
  subroutine write_field(field, iter)
    use iso_fortran_env, only : REAL64
    use pngwriter
    implicit none

    integer, parameter :: dp = REAL64
    real, intent(in) :: field(:,:)
    integer, intent(in) :: iter

    character(len=85) :: filename
    integer :: nx, ny, stat

    nx = size(field, 1)
    ny = size(field, 2)


    write(filename,'(A5,I4.4,A4,A)')  'heat_', iter, '.png'
    stat = save_png(real(field, kind=dp), nx, ny, filename)
    if (stat == 0) then
       write (*,*) 'Wrote the png file ', filename
       write (*,*) 'Use e.g. "eog" to open it.'
    end if
  end subroutine write_field

end module io
