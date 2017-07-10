module setup

 use toolkit, only:ibound

 implicit none

 real, parameter :: pi=4*atan(1.)
 real :: xmin, xmax
 contains

 subroutine setup_wave(pos,vel,mass,h,dens,n)
    use toolkit, only:n_max
    use eos, only:c_s
    real, dimension(n_max), intent(out) :: pos,vel,mass,h,dens
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
 end subroutine setup_wave

 subroutine setup_shock(pos,vel,mass,h,dens,u,n)
    use toolkit, only:n_max
    use eos, only:c_s,gamma
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
 end subroutine setup_shock

end module setup
