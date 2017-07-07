module density

 implicit none

 contains

 subroutine get_density(pos,mass,h,dens,n,n_ghosts)
    use cubic, only: cubic_spline,rkern
    real, dimension(:), intent(in) :: pos,mass
    real, dimension(:), intent(inout) :: h
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

 subroutine get_h(dens,mass,h)
    use toolkit, only:h_fac
    real, dimension(:), intent(in) :: dens,mass
    real, dimension(:), intent(out) :: h

    h = h_fac * (mass / dens)

 end subroutine get_h

 subroutine get_h_i(dens,mass,h)
    use toolkit, only:h_fac
    real, intent(in) :: dens,mass
    real, intent(out) :: h

    h = h_fac * (mass / dens)

 end subroutine get_h_i

 subroutine get_dens_h(pos,vel,mass,h,dens,pres,n,n_ghosts)
    use setup, only:place_ghosts
    real, dimension(:), intent(inout) :: pos,vel,mass,h,dens,pres
    integer, intent(in) :: n
    integer, intent(inout):: n_ghosts
    integer :: i

    do i=1,3
       call place_ghosts(pos,vel,mass,h,dens,pres,n,n_ghosts)
       call get_density(pos,mass,h,dens,n,n_ghosts)
       call get_h(dens,mass,h)
    enddo
  end subroutine get_dens_h
end module density
