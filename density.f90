module density

 implicit none

 contains

 subroutine get_density(pos,mass,h,dens,n,n_ghosts)
    use cubic, only: cubic_spline,rkern
    real, dimension(:), intent(in) :: pos,mass,h
    real, dimension(:), intent(out) :: dens
    integer, intent(in) :: n, n_ghosts
    integer :: i,j
    real :: dx,q,w,dw
    
    dens = 0.
    
    do i=1,n
       do j=1,n + n_ghosts
          dx = abs(pos(i) - pos(j))
          q = dx/h(i)
          if (q < rkern) then
             call cubic_spline(q, w, dw)
             dens(i) = dens(i) + mass(j) * w / h(i)
          endif
       enddo
    enddo

 end subroutine get_density

end module density
