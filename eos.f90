module eos

 implicit none

 contains

 subroutine equationofstate(dens,c_s,pres)
    real, dimension(:), intent(in) :: dens
    real, intent(in) :: c_s
    real, dimension(size(dens)), intent(out) :: pres
    
    pres = dens * c_s**2

 end subroutine equationofstate

end module eos
