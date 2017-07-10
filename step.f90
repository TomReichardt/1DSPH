module step

 implicit none

 contains

 subroutine step_leapfrog(pos,vel,acc,mass,h,dens,u,du,pres,n,n_boundaries,dt)
    use derivs, only:get_derivs
    use boundary, only:place_boundaries
    real, dimension(:), intent(inout) :: pos,vel,acc,mass,h,dens,u,du,pres
    real, intent(inout) :: dt
    integer, intent(inout) :: n
    integer, intent(inout) ::  n_boundaries
    real, dimension(size(acc)) :: acc_0, du_0
    real :: dt_new

    acc_0 = acc
    du_0 = du
    pos(:n) = pos(:n) + dt * vel(:n) + 0.5 * dt**2 * acc_0(:n)

    vel(:n) = vel(:n) + dt * acc_0(:n)
    u(:n) = u(:n) + dt * du_0(:n)
    call place_boundaries(pos,vel,mass,h,dens,u,du,pres,n,n_boundaries)

    call get_derivs(pos,vel,acc,mass,h,dens,u,du,pres,n,n_boundaries,dt_new)

    vel(:n) = vel(:n) + 0.5 * dt * (acc(:n) - acc_0(:n))
    u(:n) = u(:n) + 0.5 * dt * (du(:n) - du_0(:n))

    call place_boundaries(pos,vel,mass,h,dens,u,du,pres,n,n_boundaries)

    dt = dt_new
 end subroutine step_leapfrog

end module step
