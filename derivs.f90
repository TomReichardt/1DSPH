module derivs

 implicit none

 contains

 subroutine get_derivs(pos,vel,acc,mass,h,dens,u,du,pres,n,n_boundaries,dt)
    use boundary, only:place_boundaries
    use density, only:get_density,get_h,get_dens_h
    use eos, only:equationofstate
    use acceleration, only:get_accel
    real, dimension(:), intent(inout) :: pos,vel,acc,mass,h,dens,u,du,pres
    real, intent(out) :: dt
    integer, intent(inout) :: n
    integer, intent(inout):: n_boundaries

    call place_boundaries(pos,vel,mass,h,dens,u,du,pres,n,n_boundaries)
    call get_dens_h(pos,vel,mass,h,dens,u,du,pres,n,n_boundaries)
    call place_boundaries(pos,vel,mass,h,dens,u,du,pres,n,n_boundaries)
    call equationofstate(dens,pres,u,n)
    call place_boundaries(pos,vel,mass,h,dens,u,du,pres,n,n_boundaries)
    call get_accel(pos,vel,acc,dens,mass,h,du,pres,n,n_boundaries,dt)
 end subroutine get_derivs

end module derivs
