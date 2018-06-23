CC=gcc
F=ifort
PFUNIT = /opt/funit/pfunit-serial
F90_VENDOR = Intel
# f2py bundled with numpy
F2P=f2py2
DOX=doxygen


SDIR=src
ODIR=out
PDIR=py

IMGS=4

PATHS = PATH='/opt/intel/compilers_and_libraries_2018.2.199/linux/mpi/intel64/bin:'"$$PATH" LD_LIBRARY_PATH='/opt/intel/compilers_and_libraries_2018.2.199/linux/compiler/lib/intel64_lin' 
FC:=$(PATHS) $(F)
FFLAGS = -funroll-all-loops -std08 -implicitnone -fpp -warn all -pedantic -module $(ODIR) -coarray
FFLAGS += -I$(PFUNIT)/mod -I$(PFUNIT)/include 
FFLAGS += -WB -g -O0 
LIBS = $(PFUNIT)/lib/libpfunit$(LIB_EXT)

FS = $(filter-out $(SDIR)/main.F90, $(wildcard $(SDIR)/*.F90))
OBJS = $(FS:$(SDIR)/%.F90=$(ODIR)/%.o)

.PHONY: all main run clean

all: $(ODIR)/main.o

run: $(ODIR)/main.o
	FOR_COARRAY_NUM_IMAGES=$(IMGS) $(PATHS) $(ODIR)/main.o $(ARGS)

main: $(ODIR)/main.o

doc: $(SDIR)/*.F90
	$(DOX)

$(ODIR)/main.o: $(SDIR)/main.F90 $(OBJS)
	$(PATHS) $(FC) $^ -o $@ $(FFLAGS)

$(ODIR)/%.o: $(SDIR)/%.F90
	$(PATHS) $(FC) -c $< -o $@ $(FFLAGS)

$(PDIR)/%.so: $(SDIR)/%.F90
	F90=$(F) CC=$(CC) cd $(PDIR); $(F2P) -c ../$< -m $* 

clean:
	find -type p -delete
