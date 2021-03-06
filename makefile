FC=gfortran
FFLAGS=-O3 -Wall -Wextra -fdefault-real-8 -fdefault-double-8 -fcheck=all
SETUP=shock
SRC=toolkit.f90 eos.f90 setup_$(SETUP).f90 boundary.f90 energy.f90 output.f90 cubic.f90 viscosity.f90 density.f90 acceleration.f90 derivs.f90 step.f90 sph.f90
OBJ=${SRC:.f90=.o}

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

sph: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

clean:
	rm *.o *.mod sph
