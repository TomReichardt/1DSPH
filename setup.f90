module setup

 implicit none

 real, parameter :: pi=4*atan(1.)

 contains

 subroutine setup_wave(pos,vel,mass,h,dens,xmin,xmax,n_max,n,c_s)
    real, dimension(n_max), intent(out) :: pos,vel,mass,h,dens
    real, intent(in) :: xmin,xmax,c_s
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
       pos(i) = ((i-0.5) * sep) + xmin

       !Particle masses based on density
       mass(i) = dens(i) * sep

       !Particle velocity
       radian_pos(i) = (pos(i) - xmin) * 2. * pi / (xmax - xmin)
       vel(i) = sin(radian_pos(i)) * 1.e-4 * c_s

       !Particle smoothing length
       h(i) = 1.2 * sep    
    enddo
 end subroutine setup_wave

 subroutine place_ghosts(pos,vel,mass,h,dens,xmin,xmax,n,n_ghosts)
    real, dimension(:), intent(inout) :: pos,vel,mass,h,dens
    real, intent(in) :: xmin,xmax
    integer, intent(in) :: n
    integer, intent(out) :: n_ghosts
    integer :: i

    n_ghosts = 0

    do i=1,n
       if (pos(i) >= xmax - 2 * h(i)) then
          n_ghosts = n_ghosts + 1
          pos(n_ghosts + n)  = pos(i) - xmax
          vel(n_ghosts + n)  = vel(i)
          mass(n_ghosts + n) = mass(i)
          dens(n_ghosts + n) = dens(i)
          h(n_ghosts + n)    = h(i)

       elseif (pos(i) <= xmin + 2 * h(i)) then
          n_ghosts = n_ghosts + 1
          pos(n_ghosts + n)  = pos(i) + xmax
          vel(n_ghosts + n)  = vel(i)
          mass(n_ghosts + n) = mass(i)
          dens(n_ghosts + n) = dens(i)
          h(n_ghosts + n)    = h(i)
       endif
    enddo

 end subroutine place_ghosts

end module setup
