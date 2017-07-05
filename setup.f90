module setup

 implicit none

 real, parameter :: pi=4*atan(1.)

 contains

 subroutine setup_wave(pos,vel,mass,h,dens,xmin,xmax,n_max,n)
    real, dimension(n_max), intent(out) :: pos,vel,mass,h,dens
    real, intent(in) :: xmin,xmax
    integer, intent(in):: n_max
    integer, intent(out) :: n
    real, dimension(n_max) :: radian_pos
    real :: sep
    integer :: i

    n=100
    dens = 1.
    !Particle spacing
    sep = (xmax - xmin) / n

    do i=1,n
       pos(i) = (i * sep) + xmin

       !Particle masses based on density
       mass(i) = dens(i) * sep

       !Particle velocity
       radian_pos(i) = (pos(i) - xmin) * 2. * pi / (xmax - xmin)
       vel(i) = sin(radian_pos(i))*1.e-4

       !Particle smoothing length
       h(i) = 1.2 * sep    
    enddo
    

 end subroutine setup_wave

end module setup
