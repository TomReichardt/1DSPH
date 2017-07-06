program sph

 use setup, only:setup_wave,place_ghosts
 use output, only:write_out
 use density, only:get_density
 use eos, only:equationofstate
 use acceleration, only:get_accel
 use toolkit, only:n_max,c_s,dt
 use derivs, only:get_derivs
 use step, only:step_leapfrog

 implicit none

 real, dimension(n_max) :: pos,vel,acc=0.,mass,h,dens,pres=0.,u=0.
 real :: t = 0.
 integer :: n, n_ghosts
 integer :: i

 call setup_wave(pos,vel,mass,h,dens,n,c_s)

 call place_ghosts(pos,vel,mass,h,dens,pres,n,n_ghosts)

 !call get_derivs(pos,vel,acc,mass,h,dens,pres,c_s,n,n_ghosts)

 call write_out(pos,vel,acc,mass,h,dens,u,pres,n+n_ghosts,t)

 do i=1,int(5./dt)
    print*,i
    call step_leapfrog(pos,vel,acc,mass,h,dens,pres,c_s,n,n_ghosts,dt)
    t = t + dt
    call write_out(pos,vel,acc,mass,h,dens,u,pres,n+n_ghosts,t)
 enddo

 !call write_out(pos,vel,acc,mass,h,dens,u,pres,n+n_ghosts,t)

 print*, "Hello World!"

end program sph
