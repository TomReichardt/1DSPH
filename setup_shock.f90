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
    real :: sep1, sep2
    integer :: i, n1, n2

    xmin = -0.5
    xmax = 0.5

    ibound = 2

    !Particle spacing
    sep1 = 0.001
    sep2 = 0.008

    n1 = int((xmax - xmin) / (2. * sep1))
    n2 = int((xmax - xmin) / (2. * sep2))
    n = n1 + n2
    dens(1:n1) = 1.
    dens(n1+1:n) = 0.125

    u(1:n1) = 1./(dens(1:n1) * (gamma - 1))
    u(n1+1:n) = 0.1/(dens(n1+1:n) * (gamma - 1))

    vel = 0.

    do i=1,n1
       pos(i) = ((i-0.5) * sep1) + xmin

       !Particle masses based on density
       mass(i) = dens(i) * sep1

       !Particle smoothing length
       h(i) = 1.2 * sep1   
    enddo

    do i=1 + n1,n
       pos(i) = ((i-0.5-n1) * sep2) + (xmax + xmin) / 2.

       !Particle masses based on density
       mass(i) = dens(i) * sep2

       !Particle smoothing length
       h(i) = 1.2 * sep2  
    enddo
 end subroutine setup_particles

end module setup
