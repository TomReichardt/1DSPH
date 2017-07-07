module eos

 implicit none

 contains

 subroutine equationofstate(dens,pres,n)
    use toolkit, only:ieos
    real, dimension(:), intent(in) :: dens
    real, dimension(size(dens)), intent(out) :: pres
    integer, intent(in) :: n
    integer :: i    

    do i=1,n
       pres(i) = dens(i) * c_s(pres(i),dens(i),ieos)**2
    enddo

 end subroutine equationofstate

 real function c_s(presi,densi,ieos)
    real, intent(in) :: presi,densi
    integer, intent(in) :: ieos
    real :: gamma
    
    if (ieos == 1) then
       gamma = 5./3.
       c_s = sqrt(gamma * presi / densi)
    elseif (ieos == 2) then
       c_s = 1.
    else
       c_s = 1.
    endif

 end function c_s

end module eos
