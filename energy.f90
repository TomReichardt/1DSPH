module energy

 implicit none

 contains

 real function kinetic_energy(vel,mass,n)
    real, dimension(:), intent(in) :: vel, mass
    integer, intent(in) :: n
    integer :: i

    kinetic_energy = 0.
    do i=1,n
       kinetic_energy = kinetic_energy + 0.5 * mass(i) * vel(i)**2
    enddo

 end function kinetic_energy

end module energy
