program sph

 use setup, only:setup_wave,place_ghosts
 use output, only:write_out
 use density, only:get_density
 use eos, only:equationofstate

 implicit none

 integer, parameter :: n_max = 120
 real, parameter :: xmax = 1., xmin = 0., c_s = 1.
 real, dimension(n_max) :: pos,vel,mass,h,dens,pres,u=0.
 integer :: n, n_ghosts

 call setup_wave(pos,vel,mass,h,dens,xmin,xmax,n_max,n,c_s)

 call place_ghosts(pos,vel,mass,h,dens,xmin,xmax,n,n_ghosts)

 call get_density(pos,mass,h,dens,n,n_ghosts)

 call equationofstate(dens,c_s,pres)

 call write_out(pos,vel,mass,h,dens,u,pres,n+n_ghosts)

 print*, "Hello World!"

end program sph
