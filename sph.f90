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
 integer :: i, maxsteps

 call setup_wave(pos,vel,mass,h,dens,n,c_s)

 call place_ghosts(pos,vel,mass,h,dens,pres,n,n_ghosts)

 !call get_derivs(pos,vel,acc,mass,h,dens,pres,c_s,n,n_ghosts)

 call write_out(pos,vel,acc,mass,h,dens,u,pres,n+n_ghosts,t)

 maxsteps = int(5./dt)

 do i=1,maxsteps
    if (mod(i,100)==0) then
       print*,i,' of ',maxsteps
    endif
    call step_leapfrog(pos,vel,acc,mass,h,dens,pres,c_s,n,n_ghosts,dt)
    t = t + dt
    if (mod(i,10)==0) then
       call write_out(pos,vel,acc,mass,h,dens,u,pres,n+n_ghosts,t)
    endif
 enddo

 print*, "Hello World!"

end program sph
