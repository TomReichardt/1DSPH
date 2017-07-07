module output

 implicit none

 contains

 subroutine write_out(pos,vel,acc,mass,h,dens,u,pres,n,t)
    integer, intent(in) :: n
    real, intent(in) :: t
    real, dimension(:), intent(in) :: pos,vel,acc,mass,h,dens,u,pres
    integer :: i
    character(len=20) :: filename

    write(filename,'(A,F5.3,A)') 'dumps_',t,'.out'

    open(unit=1,file=filename,status='replace')


    do i=1,n
       write(1,*) pos(i),vel(i),acc(i),mass(i),h(i),dens(i),u(i),pres(i)
    enddo

    close(1)

  end subroutine write_out

 subroutine write_ev_out(vel,mass,n,t,i)
    use toolkit, only:maxsteps,evwrite
    use energy, only:kinetic_energy
    integer, intent(in) :: n, i
    real, intent(in) :: t
    real, dimension(:), intent(in) :: vel,mass
    character(len=20) :: filename

    write(filename,'(A)') 'energy.ev'
    if (i == 0) then
       open(unit=evwrite,file=filename,status='replace')
    endif

    write(evwrite,*) t, kinetic_energy(vel,mass,n)

    if (i == maxsteps) then
       close(evwrite)
    endif
  end subroutine write_ev_out

end module output
