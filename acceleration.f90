module acceleration

 implicit none

 contains

 subroutine get_accel(pos,vel,acc,dens,mass,h,du,pres,n,n_boundaries,dt,opt_alpha,opt_beta)
    use cubic, only:cubic_spline,rkern
    use toolkit, only:r_hat,ieos
    use viscosity, only:art_visc
    use eos, only:c_s
    real, dimension(:), intent(in) :: pos,vel,dens,mass,h,pres
    real, intent(out) :: dt
    real, intent(in), optional :: opt_alpha,opt_beta
    real, dimension(size(pos)), intent(out) :: acc,du
    integer, intent(in) :: n, n_boundaries
    integer :: i,j
    real :: alpha,beta
    real :: dx,qi,qj,wi,wj,dwi,dwj,q_iij,q_jij

    dt = huge(dt)

    if (present(opt_alpha)) then
       alpha = opt_alpha
    else
       alpha = 1.
    endif
    
    if (present(opt_beta)) then
       beta = opt_beta
    else
       beta = 2.
    endif

    acc = 0.
    du = 0.

    do i=1,n
       do j=1,n + n_boundaries 
          dx = abs(pos(i) - pos(j))
          qi = dx/h(i)
          qj = dx/h(j)
          if (qi < rkern .or. qj < rkern) then
             call cubic_spline(qi, wi, dwi)
             call cubic_spline(qj, wj, dwj)
             call art_visc(alpha,beta,pos(i),pos(j),vel(i),vel(j),dens(i),dens(j),pres(i),pres(j),q_iij,q_jij)
             acc(i) = acc(i) - mass(j) *&
                      ((pres(i) + q_iij)/(dens(i)**2) * dwi * r_hat(pos(i),pos(j)) / (h(i)**2) +&
                       (pres(j) + q_jij)/(dens(j)**2) * dwj * r_hat(pos(i),pos(j)) / (h(j)**2))
             du(i) = du(i) + mass(j) * (pres(i) + q_iij)/(dens(i)**2) *&
                     (vel(i) - vel(j)) * dwi * r_hat(pos(i),pos(j)) / (h(i)**2)
             dt = min(dt,h(i)/c_s(dens(i),pres(i),ieos),h(j)/c_s(dens(j),pres(j),ieos))
          endif
       enddo
    enddo
 dt = dt*0.2

 end subroutine get_accel

end module acceleration
