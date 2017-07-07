module derivs

 implicit none

 contains

 subroutine get_derivs(pos,vel,acc,mass,h,dens,pres,n,n_ghosts,dt)
    use setup, only:place_ghosts
    use density, only:get_density,get_h,get_dens_h
    use eos, only:equationofstate
    use acceleration, only:get_accel
    real, dimension(:), intent(inout) :: pos,vel,acc,mass,h,dens,pres
    real, intent(out) :: dt
    integer, intent(in) :: n
    integer, intent(inout):: n_ghosts

    call place_ghosts(pos,vel,mass,h,dens,pres,n,n_ghosts)
    call get_dens_h(pos,vel,mass,h,dens,pres,n,n_ghosts)
    call place_ghosts(pos,vel,mass,h,dens,pres,n,n_ghosts)
    call equationofstate(dens,pres,n)
    call place_ghosts(pos,vel,mass,h,dens,pres,n,n_ghosts)
    call get_accel(pos,vel,acc,dens,mass,h,pres,n,n_ghosts,dt)
 end subroutine get_derivs

end module derivs
