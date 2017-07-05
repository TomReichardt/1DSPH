module cubic

 implicit none

 real, parameter :: rkern = 2.

 contains

  subroutine cubic_spline(q, w, dw)
    real, intent(in) :: q
    real, intent(out) :: w, dw
    real :: sigma = 2./3.
    
    if (q > 2) then
       w = 0.
       dw = 0.
    elseif (q >= 1) then
       w = 0.25 * (2. - q)**3
       dw = -0.75 * (2. - q)**2
    elseif (q >= 0) then
       w = 0.25 * (2. - q)**3 - (1. - q)**3
       dw = -0.75 * (2. - q)**2 + 3. * (1. - q)**2
    else
       print*, "WARNING: q < 0!!!"
       w = 0.
       dw = 0.
    endif

    w = w * sigma
    dw = dw * sigma
 end subroutine cubic_spline

end module cubic
