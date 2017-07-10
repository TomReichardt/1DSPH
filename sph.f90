program sph

 use setup, only:setup_wave,setup_shock,place_ghosts
 use output, only:write_out,write_ev_out
 use density, only:get_density
 use eos, only:equationofstate
 use toolkit, only:n_max,dt,maxsteps
 use derivs, only:get_derivs
 use step, only:step_leapfrog

 implicit none

 real, dimension(n_max) :: pos,vel,acc=0.,mass,h,dens,pres=0.,u=0.
 real :: t = 0., dumpt = 0.
 integer :: n, n_ghosts
 integer :: i, dumpn = 0
 !integer, parameter :: maxsteps = int(5./dt)

 call setup_shock(pos,vel,mass,h,dens,n)

 call place_ghosts(pos,vel,mass,h,dens,pres,n,n_ghosts)

 !call get_derivs(pos,vel,acc,mass,h,dens,pres,c_s,n,n_ghosts)

 call write_out(pos,vel,acc,mass,h,dens,u,pres,n+n_ghosts,dumpn)
 call write_ev_out(vel,mass,n,t,0)

 do i=1,maxsteps
    if (mod(i,100)==0) then
       print*,i,' of ',maxsteps
    endif
    call step_leapfrog(pos,vel,acc,mass,h,dens,pres,n,n_ghosts,dt)
    t = t + dt
    dumpt = dumpt + dt

    if (dumpt > 0.005) then
       dumpn = dumpn + 1
       call write_out(pos,vel,acc,mass,h,dens,u,pres,n+n_ghosts,dumpn)
       dumpt = 0.
    endif
    call write_ev_out(vel,mass,n,t,i)
 enddo

 print*, "Hello World!"

end program sph
