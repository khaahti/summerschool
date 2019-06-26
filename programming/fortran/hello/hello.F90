program hello
  implicit none
  real :: a, b, c
  b = 1.0
  write (*,*) 'Hello world! Give a number: '
  read (*,*) a
  c = a * b + b
  write (*,*) ' a = ', a, ' b = ', b, ' c = ', c
end program hello
