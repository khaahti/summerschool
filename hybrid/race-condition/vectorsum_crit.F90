program vectorsum
  use iso_fortran_env, only: int64
  use omp_lib
  implicit none
  integer, parameter :: ik = int64
  integer(kind=ik), parameter :: nx = 102400_ik

  integer(kind=ik), dimension(nx) :: vecA
  integer(kind=ik) :: sum, psum, sumex
  integer(kind=ik) :: i

  ! Initialization of vector
  do i = 1, nx
     vecA(i) = i
  end do

  sumex = nx*(nx+1_ik)/2_ik
  write(*,*) 'Arithmetic sum formula (exact):                  ', sumex

  sum = 0
  ! TODO: Parallelize the computation
  !$omp parallel shared(vecA) private(i, psum)
  psum=0
  !$omp do
  do i = 1, nx
     psum = psum + vecA(i)
  end do
  !$omp end do
  !$omp critical 
  sum = sum + psum
  !$omp end critical
  !$omp end parallel
  write(*,*) 'Sum: ', sum
end program vectorsum
