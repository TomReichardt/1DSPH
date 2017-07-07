module viscosity

 implicit none

 contains

 subroutine art_visc(alpha,beta,pos_a,pos_b,vel_a,vel_b,dens_a,dens_b,pres_a,pres_b,q_aab,q_bab)
    use toolkit, only:r_hat,ieos
    use eos, only:c_s
    real, intent(in) :: alpha,beta,pos_a,pos_b,vel_a,vel_b,dens_a,dens_b,pres_a,pres_b
    real, intent(out) :: q_aab,q_bab
    real :: v_ab,vdotr_ab,v_sig_a,v_sig_b
    
    v_ab = (vel_a - vel_b)
    vdotr_ab = v_ab * r_hat(pos_a,pos_b)
    v_sig_a = alpha * c_s(dens_a,pres_a,ieos) - beta * vdotr_ab
    v_sig_b = alpha * c_s(dens_b,pres_b,ieos) - beta * vdotr_ab

    if (vdotr_ab < 0) then
       q_aab = -0.5 * dens_a * v_sig_a * vdotr_ab
       q_bab = -0.5 * dens_b * v_sig_b * vdotr_ab
    else
       q_aab = 0.
       q_bab = 0.
    endif
 end subroutine art_visc
end module viscosity
