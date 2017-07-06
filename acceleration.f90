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
    real :: dx,q,w,dw

    acc = 0.

    do i=1,n
       do j=1,n + n_ghosts 
          dx = abs(pos(i) - pos(j))
          q = dx/h(i)
          if (q < rkern) then
             call cubic_spline(q, w, dw)
             acc(i) = acc(i) - mass(j) *&
                      (pres(i)/(dens(i)**2) * dw * r_hat(pos(i),pos(j)) / (h(i)**2) +&
                       pres(j)/(dens(j)**2) * dw * r_hat(pos(j),pos(i)) / (h(j)**2))
          endif
       enddo
    enddo

 end subroutine get_accel

end module acceleration
