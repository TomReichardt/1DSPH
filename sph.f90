program sph

 use setup, only:setup_wave,place_ghosts
 use output, only:write_out
 use density, only:get_density
 use eos, only:equationofstate
 use acceleration, only:get_accel
 use toolkit, only:n_max,c_s
 use derivs, only:get_derivs

 implicit none

 real, dimension(n_max) :: pos,vel,acc,mass,h,dens,pres,u=0.
 integer :: n, n_ghosts

 call setup_wave(pos,vel,mass,h,dens,n,c_s)

 call place_ghosts(pos,vel,mass,h,dens,n,n_ghosts)

 call get_derivs(pos,vel,acc,mass,h,dens,pres,c_s,n,n_ghosts)
 !call get_density(pos,mass,h,dens,n,n_ghosts)

 !call place_ghosts(pos,vel,mass,h,dens,n,n_ghosts)

 !call equationofstate(dens,c_s,pres)

 !call get_accel(acc,pos,dens,mass,h,pres,n,n_ghosts)

 call write_out(pos,vel,acc,mass,h,dens,u,pres,n+n_ghosts)

 print*, "Hello World!"

end program sph
