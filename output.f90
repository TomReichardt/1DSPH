module output

 implicit none

 contains

 subroutine write_out(pos,vel,acc,mass,h,dens,u,pres,n,t)
    integer, intent(in) :: n
    real, intent(in) :: t
    real, dimension(:), intent(in) :: pos,vel,acc,mass,h,dens,u,pres
    integer :: i
    character(len=20) :: filename

    write(filename,'(A,F5.3,A)') 'output_',t,'.dat'
    print*,filename

    open(unit=1,file=filename,status='replace')


    do i=1,n
       write(1,*) pos(i),vel(i),acc(i),mass(i),h(i),dens(i),u(i),pres(i)
    enddo

    close(1)

  end subroutine write_out
end module output
