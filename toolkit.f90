module toolkit

 implicit none

 real, parameter :: h_fac=1.2
 real :: dt = 0.001
 integer, parameter :: n_max=300, evwrite=2, maxsteps=5000, ieos=2

 contains

 real function r_hat(pos1,pos2)
    real :: pos1, pos2
    if (abs(pos1 - pos2) > tiny(pos1)) then
       r_hat = (pos1 - pos2) / abs(pos1 - pos2)
    else
       r_hat = 0.
    endif
 end function r_hat

end module toolkit
