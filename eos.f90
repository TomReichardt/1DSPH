module eos

 implicit none

 contains

 subroutine equationofstate(rho,c_s,pres)
    real, dimension(:), intent(in) :: rho
    real, intent(in) :: c_s
    real, dimension(size(rho)), intent(out) :: pres
    
    pres = rho * c_s**2

 end subroutine equationofstate

end module eos
