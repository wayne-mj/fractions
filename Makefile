FC=gfortran
FFLAGS=-O3 -Wall -Wextra
MODULES=fractionmodule.f90
PROG=fractions.f90
SRC=$(MODULES) $(PROG)
OBJ=${SRC:.f90=.o}
BASE=${SRC:.f90=}

all: clean fractions

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

fractions: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

clean:
	rm -f *.o *.mod $(BASE)
