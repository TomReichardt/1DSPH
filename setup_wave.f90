module setup

 use toolkit, only:ibound
 use eos, only:c_s,gamma
 implicit none

 real, parameter :: pi=4*atan(1.)
 real :: xmin, xmax
 contains

 subroutine setup_particles(pos,vel,mass,h,dens,u,n)
    use toolkit, only:n_max
    real, dimension(n_max), intent(out) :: pos,vel,mass,h,dens,u
    integer, intent(out) :: n
    real, dimension(n_max) :: radian_pos
    real :: sep
    integer :: i

    xmin = 0.
    xmax = 1.

    ibound = 1

    n=100
    dens = 1.
    !Particle spacing
    sep = (xmax - xmin) / n

    u = 1./(dens * (gamma - 1))

    do i=1,n
       pos(i) = ((i-0.5) * sep) + xmin

       !Particle masses based on density
       mass(i) = dens(i) * sep

       !Particle velocity
       radian_pos(i) = (pos(i) - xmin) * 2. * pi / (xmax - xmin)
       vel(i) = sin(radian_pos(i)) * 1.e-4 * c_s(0.,0.,2)

       !Particle smoothing length
       h(i) = 1.2 * sep    
    enddo
 end subroutine setup_particles

end module setup
