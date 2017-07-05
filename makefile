FC=gfortran
FFLAGS=-O3 -Wall -Wextra -fdefault-real-8 -fdefault-double-8
SRC=setup.f90 output.f90 density.f90 sph.f90
OBJ=${SRC:.f90=.o}

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

sph: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

clean:
	rm *.o *.mod sph
