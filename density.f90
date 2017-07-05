module density

 implicit none

 contains

 subroutine get_density(pos,mass,h,dens)
    real, dimension(:), intent(in) :: pos,mass,h
    real, dimension(:), intent(out) :: dens
    integer :: i,j
    real :: dx,q
    
    dens = 0.
    
    do i=1,100
       do j=1,100
          dx = abs(pos(i) - pos(j))
          q = dx/h(j)
          dens(i) = dens(i) + mass(j) * cubic_spline(q)
       enddo
    enddo

 end subroutine get_density

 real function cubic_spline(q)
    real, intent(in) :: q
    
    if (q > 2) then
       cubic_spline = 0.
    elseif (q >= 1) then
       cubic_spline = 0.25 * (2 - q)**3
    elseif (q >= 0) then
       cubic_spline = 0.25 * (2 - q)**3 - (1 - q)**3
    else
       print*, "WARNING: q < 0!!!"
       cubic_spline = 0.
    endif
 end function cubic_spline

end module density
