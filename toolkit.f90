module toolkit

 implicit none

 integer :: ibound = 1
 real, parameter :: h_fac=1.2
 real :: dt = 0.001, maxtime=5
 integer, parameter :: n_max=1000, evwrite=2, ieos=1

 contains

 real function r_hat(pos1,pos2)
    real :: pos1, pos2
    if (abs(pos1 - pos2) > tiny(pos1)) then
       r_hat = (pos1 - pos2) / abs(pos1 - pos2)
    else
       r_hat = 0.
    endif
 end function r_hat

 subroutine shuffle_array(x,n,m)
    real, dimension(:), intent(inout) :: x
    real, dimension(m+n) :: y
    integer, intent(in) :: n,m

    y(:m) = x(:m)
    y(m+1:m+n) = x(1:n)
    x(:m) = y(n+1:m+n)

 end subroutine shuffle_array

end module toolkit
