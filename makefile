FC      = pgf90
FFLAGS  = -fast -g
FFLAGS += -tp=istanbul
#FFLAGS += -Kieee                  # use exact IEEE math
#FFLAGS += -Mbounds                # for bounds checking/debugging
#FFLAGS += -Ktrap=fp               # trap floating point exceptions
FFLAGS += -Bstatic_pgi            # to use static PGI libraries
#FFLAGS += -Bstatic                # to use static netCDF libraries
FFLAGS += -mp=nonuma -nomp        # fix for "can't find libnuma.so"

PROG    = interp-intermediate

TODAY   = 2020-04-16
VERSION = 1.3

%.o : %.f90
	$(FC) -c $(FFLAGS) $< -o $@

$(PROG): $(PROG).o
	$(FC) $(FFLAGS) -o $(PROG) $(PROG).o

distro: 
	zip $(PROG)_v$(VERSION)_$(TODAY).zip \
		$(PROG) *.f90 makefile README.txt 
#		SNOWH:2011-01-01_12 SNOWH:2011-01-02_00 SNOWH:2011-01-02_12

install:
	cp $(PROG) /usr/local/bin

clean:
	rm $(PROG) *.o 

