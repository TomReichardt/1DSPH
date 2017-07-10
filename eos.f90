module eos

 implicit none
 real, parameter :: gamma = 7./5.
 contains

 subroutine equationofstate(dens,pres,u,n)
    use toolkit, only:ieos
    real, dimension(:), intent(in) :: dens,u
    real, dimension(size(dens)), intent(out) :: pres
    integer, intent(in) :: n
    integer :: i    

    if (ieos == 1) then
       do i=1,n
          pres(i) = dens(i) * u(i) * (gamma - 1.)
       enddo
    elseif (ieos == 2) then
       do i=1,n
          pres(i) = dens(i) * c_s(pres(i),dens(i),ieos)**2
       enddo
    endif
 end subroutine equationofstate

 real function c_s(presi,densi,ieos)
    real, intent(in) :: presi,densi
    integer, intent(in) :: ieos
    
    if (ieos == 1) then
       c_s = sqrt(gamma * presi / densi)
    elseif (ieos == 2) then
       c_s = 1.
    else
       c_s = 1.
    endif

 end function c_s

end module eos
