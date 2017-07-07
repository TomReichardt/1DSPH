module acceleration

 implicit none

 contains

 subroutine get_accel(acc,pos,dens,mass,h,pres,n,n_ghosts)
    use cubic, only:cubic_spline,rkern
    use toolkit, only:r_hat
    real, dimension(:), intent(in) :: pos,dens,mass,h,pres
    real, dimension(size(pos)), intent(out) :: acc 
    integer, intent(in) :: n, n_ghosts
    integer :: i,j
    real :: dx,qi,qj,wi,wj,dwi,dwj

    acc = 0.

    do i=1,n
       do j=1,n + n_ghosts 
          dx = abs(pos(i) - pos(j))
          qi = dx/h(i)
          qj = dx/h(j)
          if (qi < rkern .or. qj < rkern) then
             call cubic_spline(qi, wi, dwi)
             call cubic_spline(qj, wj, dwj)
             acc(i) = acc(i) - mass(j) *&
                      (pres(i)/(dens(i)**2) * dwi * r_hat(pos(i),pos(j)) / (h(i)**2) +&
                       pres(j)/(dens(j)**2) * dwj * r_hat(pos(i),pos(j)) / (h(j)**2))
          endif
       enddo
    enddo

 end subroutine get_accel

end module acceleration
