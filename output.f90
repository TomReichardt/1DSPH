module output

 implicit none

 contains

 subroutine write_out(pos,vel,acc,mass,h,dens,u,pres,n,num,t)
    integer, intent(in) :: n,num
    real, dimension(:), intent(in) :: pos,vel,acc,mass,h,dens,u,pres
    real, intent(in) :: t
    integer :: i
    character(len=20) :: filename

    write(filename,'(A,I5.5,A)') 'dumps_',num,'.out'

    open(unit=1,file=filename,status='replace')

    write(1,*) t

    do i=1,n
       write(1,*) pos(i),vel(i),acc(i),mass(i),h(i),dens(i),u(i),pres(i)
    enddo

    close(1)

    print*, 'Particles at t = ', t, ' written successfully to ', filename

  end subroutine write_out

 subroutine write_ev_out(vel,mass,n,t)
    use toolkit, only:maxtime,evwrite
    use energy, only:kinetic_energy
    integer, intent(in) :: n
    real, intent(in) :: t
    real, dimension(:), intent(in) :: vel,mass
    character(len=20) :: filename

    write(filename,'(A)') 'energy.ev'
    if (t < tiny(t)) then
       open(unit=evwrite,file=filename,status='replace')
    endif

    write(evwrite,*) t, kinetic_energy(vel,mass,n)

    if (t >= maxtime) then
       close(evwrite)
    endif
  end subroutine write_ev_out

end module output
