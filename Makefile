FC=gfortran
FFLAGS=-O3 -Wall -Wextra
AR=ar
ARARGS=r
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

lib: $(MODULES)
	$(FC) $(FFLAGS) -c $<
	$(AR) $(ARARGS) $(PROG:.f90=.a) $(MODULES:.f90=.o)

clean:
	rm -f *.o *.mod $(BASE) *.a
