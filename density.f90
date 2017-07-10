module density

 implicit none

 contains

 subroutine get_density(pos,mass,h,dens,n,n_boundaries)
    use cubic, only: cubic_spline,rkern
    real, dimension(:), intent(in) :: pos,mass
    real, dimension(:), intent(inout) :: h
    real, dimension(:), intent(out) :: dens
    integer, intent(in) :: n, n_boundaries
    integer :: i,j
    real :: dx,q,w,dw
    
    dens(1:n) = 0.
    
    do i=1,n
       do j=1,n + n_boundaries
          dx = abs(pos(i) - pos(j))
          q = dx/h(i)
          if (q < rkern) then
             call cubic_spline(q, w, dw)
             dens(i) = dens(i) + mass(j) * w / h(i)
          endif
       enddo
    enddo

 end subroutine get_density

 subroutine get_h(dens,mass,h,n)
    use toolkit, only:h_fac
    real, dimension(:), intent(in) :: dens,mass
    real, dimension(:), intent(out) :: h
    integer, intent(in) :: n

    h(1:n) = h_fac * (mass(1:n) / dens(1:n))

 end subroutine get_h

 subroutine get_h_i(dens,mass,h)
    use toolkit, only:h_fac
    real, intent(in) :: dens,mass
    real, intent(out) :: h

    h = h_fac * (mass / dens)

 end subroutine get_h_i

 subroutine get_dens_h(pos,vel,mass,h,dens,u,du,pres,n,n_boundaries)
    use boundary, only:place_boundaries
    real, dimension(:), intent(inout) :: pos,vel,mass,h,dens,u,du,pres
    integer, intent(inout) :: n
    integer, intent(inout):: n_boundaries
    integer :: i

    do i=1,3
       call place_boundaries(pos,vel,mass,h,dens,u,du,pres,n,n_boundaries)
       call get_density(pos,mass,h,dens,n,n_boundaries)
       call get_h(dens,mass,h,n)
    enddo
  end subroutine get_dens_h
end module density
