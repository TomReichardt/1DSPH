program sph

 use setup, only:setup_particles
 use boundary, only:place_boundaries
 use output, only:write_out,write_ev_out
 use density, only:get_density
 use eos, only:equationofstate
 use toolkit, only:n_max,dt,maxtime
 use derivs, only:get_derivs
 use step, only:step_leapfrog

 implicit none

 real, dimension(n_max) :: pos,vel,acc=0.,mass,h,dens,pres=0.,u,du
 real :: t = 0., dumpt = 0.
 integer :: n, n_boundaries
 integer :: dumpn = 0

 call setup_particles(pos,vel,mass,h,dens,u,n)

 call place_boundaries(pos,vel,mass,h,dens,u,du,pres,n,n_boundaries,.true.)

 call write_out(pos,vel,acc,mass,h,dens,u,pres,n+n_boundaries,dumpn,t)
 call write_ev_out(vel,mass,n,t)

 do while (t < maxtime)

    call step_leapfrog(pos,vel,acc,mass,h,dens,u,du,pres,n,n_boundaries,dt)
    t = t + dt
    dumpt = dumpt + dt

    if (dumpt > 0.005) then
       dumpn = dumpn + 1
       call write_out(pos,vel,acc,mass,h,dens,u,pres,n+n_boundaries,dumpn,t)
       dumpt = 0.
    endif
    call write_ev_out(vel,mass,n,t)
 enddo

 print*, "Hello World!"

end program sph
