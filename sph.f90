program sph

 use setup, only:setup_wave
 use output, only:write_out
 use density, only:get_density

 implicit none

 integer, parameter :: n_max = 120
 real, parameter :: xmax = 1., xmin = 0.
! real, parameter :: init_dens = 
! real, parameter :: xmax = 150., xmin = 10.
! real, parameter :: xmax = 150., xmin = 10.
 real, dimension(n_max) :: pos,vel,mass,h,dens,u
 integer :: n

 call setup_wave(pos,vel,mass,h,dens,xmin,xmax,n_max,n)

 call get_density(pos,mass,h,dens)

 call write_out(pos,vel,mass,h,dens,u,n)

 print*, "Hello World!"

end program sph
