module step

 implicit none

 contains

 subroutine step_leapfrog(pos,vel,acc,mass,h,dens,pres,c_s,n,n_ghosts,dt)
    use derivs, only:get_derivs
    use setup, only:place_ghosts
    real, dimension(:), intent(inout) :: pos,vel,acc,mass,h,dens,pres
    real, intent(in) :: c_s, dt
    integer, intent(in) :: n
    integer, intent(inout) ::  n_ghosts
    real, dimension(size(acc)) :: acc_0

    acc_0 = acc

    pos = pos + dt * vel + 0.5 * dt**2 * acc_0

    vel = vel + dt * acc_0
    call place_ghosts(pos,vel,mass,h,dens,pres,n,n_ghosts)

    call get_derivs(pos,vel,acc,mass,h,dens,pres,c_s,n,n_ghosts)

    vel = vel + 0.5 * dt * (acc - acc_0)
    call place_ghosts(pos,vel,mass,h,dens,pres,n,n_ghosts)
 end subroutine step_leapfrog

end module step
