module toolkit

 implicit none

 integer, parameter :: n_max = 120
 real, parameter :: xmax = 1., xmin = 0., c_s = 1., dt=0.01

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
