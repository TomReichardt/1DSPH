module output

 implicit none

 contains

 subroutine write_out(pos,vel,mass,h,dens,u,n)
    integer, intent(in) :: n
    real, dimension(:), intent(in) :: pos,vel,mass,h,dens,u
    integer :: i

    open(unit=1,file='output.dat',status='replace')


    do i=1,n
       write(1,*) pos(i),vel(i),mass(i),h(i),dens(i),u(i)
    enddo

    close(1)

  end subroutine write_out
end module output
