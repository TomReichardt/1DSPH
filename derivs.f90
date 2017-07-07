module derivs

 implicit none

 contains

 subroutine get_derivs(pos,vel,acc,mass,h,dens,pres,c_s,n,n_ghosts)
    use setup, only:place_ghosts
    use density, only:get_density,get_h,get_dens_h
    use eos, only:equationofstate
    use acceleration, only:get_accel
    real, dimension(:), intent(inout) :: pos,vel,acc,mass,h,dens,pres
    real, intent(in) :: c_s
    integer, intent(in) :: n
    integer, intent(inout):: n_ghosts

    call place_ghosts(pos,vel,mass,h,dens,pres,n,n_ghosts)
    call get_dens_h(pos,vel,mass,h,dens,pres,n,n_ghosts)
    call place_ghosts(pos,vel,mass,h,dens,pres,n,n_ghosts)
    call equationofstate(dens,c_s,pres)
    call place_ghosts(pos,vel,mass,h,dens,pres,n,n_ghosts)
    call get_accel(acc,pos,dens,mass,h,pres,n,n_ghosts)
 end subroutine get_derivs

end module derivs
