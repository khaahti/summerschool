module vector_algebra
	use iso_fortran_env, only : REAL64
	implicit none
	type vector_t
		real(REAL64) :: x, y, z
	end type vector_t

	! TODO: overload operators needed by the parser
	interface abs
		module procedure vector_norm
	end interface abs
	
	interface operator (+)
		module procedure vector_sum
	end interface operator (+)

	interface operator (-)
		module procedure vector_substract
	end interface operator (-)
	
	interface operator (*)
		module procedure vector_dot
	end interface operator (*)

	interface operator (.x.)
		module procedure vector_cross
	end interface operator (.x.)

contains
	! TODO: implement the corresponding functions
	function vector_norm(v1) result(norm)
		type(vector_t), intent(in) :: v1
		real :: norm
		norm = v1 % x**2 + v1 % y**2 + v1 % z**2
	end function vector_norm

	function vector_sum(v1, v2) result(v3)
		type(vector_t), intent(in) :: v1, v2
		type(vector_t) :: v3
		v3 % x = v1 % x + v2 % x
		v3 % y = v1 % y + v2 % y
		v3 % z = v1 % z + v2 % z
	end function vector_sum

	function vector_substract(v1, v2) result(v3)
		type(vector_t), intent(in) :: v1, v2
		type(vector_t) :: v3
		v3 % x = v1 % x - v2 % x
		v3 % y = v1 % y - v2 % y
		v3 % z = v1 % z - v2 % z
	end function vector_substract

	function vector_dot(v1, v2) result(prod)
		type(vector_t), intent(in) :: v1, v2
		real :: prod
		prod = v1 % x * v2 % x &
			+ v1 % y * v2 % y &
			+ v1 % z * v2 % z
	end function vector_dot
	
	function vector_cross(v1, v2) result(v3)
		type(vector_t), intent(in) :: v1, v2
		type(vector_t) :: v3
		v3 % x = v1 % y * v2 % z - v1 % z * v2 % y
		v3 % y = v1 % z * v2 % x - v1 % x * v2 % z
		v3 % z = v1 % x * v2 % y - v1 % y * v2 % x
	end function vector_cross

end module vector_algebra
