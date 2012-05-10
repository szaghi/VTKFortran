#!/usr/bin/make
#----------------------------------------------------------------------------------------------------------------------------------
# make init

# shell
SHELL = /bin/bash
# no verbose
$(VERBOSE).SILENT:
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# User options
SHARED   = no
COMPILER = gnu
DEBUG    = no
F03STD   = no
OPTIMIZE = no
OPENMP   = no
R16P     = no

.PHONY : DEFAULTRULE
DEFAULTRULE: VTK_IO

.PHONY : help
help:
	@echo
	@echo -e '\033[1;31m Make options of Lib_VTK_IO code\033[0m'
	@echo
	@echo -e '\033[1;31m Compiler choice\033[0m'
	@echo -e '\033[1;31m  COMPILER=gnu  \033[0m\033[1m => GNU gfortran          \033[0m'
	@echo -e '\033[1;31m  COMPILER=intel\033[0m\033[1m => Intel Fortran         \033[0m'
	@echo -e '\033[1;31m  COMPILER=$(COMPILER)  \033[0m\033[1m => default         \033[0m'
	@echo
	@echo -e '\033[1;31m Compiling options\033[0m'
	@echo -e '\033[1;31m  SHARED=yes(no)  \033[0m\033[1m => on(off) shared library         (default $(SHARED))\033[0m'
	@echo -e '\033[1;31m  DEBUG=yes(no)   \033[0m\033[1m => on(off) debug                  (default $(DEBUG))\033[0m'
	@echo -e '\033[1;31m  F03STD=yes(no)  \033[0m\033[1m => on(off) check standard fortran (default $(F03STD))\033[0m'
	@echo -e '\033[1;31m  OPTIMIZE=yes(no)\033[0m\033[1m => on(off) optimization           (default $(OPTIMIZE))\033[0m'
	@echo -e '\033[1;31m  OPENMP=yes(no)  \033[0m\033[1m => on(off) OpenMP directives      (default $(OPENMP))\033[0m'
	@echo
	@echo -e '\033[1;31m Preprocessing options\033[0m'
	@echo -e '\033[1;31m  R16P=yes(no)\033[0m\033[1m => on(off) definition of real with "128 bit" (default $(R16P))\033[0m'
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# directory & file
DSRC  = ./src/
DOBJ  = ./obj/
DMOD  = ./mod/
VPATH = $(DSRC) $(DOBJ) $(DMOD)
ifeq "$(SHARED)" "yes"
  DLIB    = ./shared/
  MAIN    = $(DLIB)Lib_VTK_IO.so
  MAKELIB = $(FC) $(OPTSL) $(DOBJ)lib_vtk_io.o -o $(MAIN)
else
  DLIB    = ./static/
  MAIN    = $(DLIB)Lib_VTK_IO.a
  MAKELIB = ar -rcs $(MAIN) $(DOBJ)lib_vtk_io.o ; ranlib $(MAIN)
endif
MKDIRS = $(DOBJ) $(DMOD) $(DLIB)
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# compiling and linking options
ifeq "$(COMPILER)" "gnu"
  FC      = gfortran
  OPTSC   = -cpp -c -J$(DMOD)
  OPTSL   =
  PREPROC =
  ifeq "$(SHARED)" "yes"
    OPTSC := $(OPTSC) -fPIC
    OPTSL := $(OPTSL) -shared
  endif
  # debug
  ifeq "$(DEBUG)" "yes"
    PREPROC := $(PREPROC) -DDEBUG
    OPTSC := $(OPTSC) -O0 -Wall -Warray-bounds -fcheck=all -fbacktrace -ffpe-trap=invalid,overflow,underflow,precision,denormal
    OPTSL := $(OPTSL) -O0 -Wall -Warray-bounds -fcheck=all -fbacktrace -ffpe-trap=invalid,overflow,underflow,precision,denormal
  endif
  # standard
  ifeq "$(F03STD)" "yes"
    OPTSC := $(OPTSC) -std=f2008 -fall-intrinsics
    OPTSL := $(OPTSL) -std=f2008 -fall-intrinsics
  endif
  # optimization
  ifeq "$(OPTIMIZE)" "yes"
    OPTSC := $(OPTSC) -O3
    OPTSL := $(OPTSL) -O3
  endif
  # openmp
  ifeq "$(OPENMP)" "yes"
    OPTSC := $(OPTSC) -fopenmp
    OPTSL := $(OPTSL) -fopenmp
    PREPROC := $(PREPROC) -DOPENMP
  endif
endif
ifeq "$(COMPILER)" "intel"
  FC      = ifort
  OPTSC   = -cpp -c -module $(DMOD)
  OPTSL   =
  PREPROC =
  # debug
  ifeq "$(SHARED)" "yes"
    OPTSC := $(OPTSC) -fpic
    OPTSL := $(OPTSL) -shared
  endif
  ifeq "$(DEBUG)" "yes"
    PREPROC := $(PREPROC) -DDEBUG
    CHK = -check all -check noarg_temp_created
    DEB = -debug all
    WRN = -warn all
    OPTSC := $(OPTSC) -O0 -fpe-all=0 -fp-stack-check -traceback $(WRN) $(CHK) $(DEB)
    OPTSL := $(OPTSL) -O0 -fpe-all=0 -fp-stack-check -traceback $(WRN) $(CHK) $(DEB)
  endif
  # standard
  ifeq "$(F03STD)" "yes"
    OPTSC := $(OPTSC) -std03
    OPTSL := $(OPTSL) -std03
  endif
  # optimization
  ifeq "$(OPTIMIZE)" "yes"
    OPTSC := $(OPTSC) -O3 -ipo
    OPTSL := $(OPTSL) -O3 -ipo
  endif
  # openmp
  ifeq "$(OPENMP)" "yes"
    OPTSC := $(OPTSC) -openmp
    OPTSL := $(OPTSL) -openmp
    PREPROC := $(PREPROC) -DOPENMP
  endif
endif
# pre-processing options
# R16 precision
R16PCHK = (Unknown R16P switch) Used default R16P=no
ifeq "$(R16P)" "no"
  R16PCHK = (Known R16P switch) Used R16P=$(R16P)
endif
ifeq "$(R16P)" "yes"
  R16PCHK = (Known R16P switch) Used R16P=$(R16P)
  PREPROC := $(PREPROC) -Dr16p
endif

OPTSC := $(OPTSC) $(PREPROC)
OPTSL := $(OPTSL) $(PREPROC)

WHICHFC = $(shell which $(FC))

PRINTCHK = "\\033[1;31m Compiler used \\033[0m\\033[1m $(COMPILER) => $(WHICHFC)\\033[0m \n\
            \\033[1;31mSource dir    \\033[0m\\033[1m $(DSRC)\\033[0m \n\
            \\033[1;31m Lib dir       \\033[0m\\033[1m $(DLIB)\\033[0m \n\
            \\033[1;31m Libray        \\033[0m\\033[1m $(MAIN)\\033[0m \n\
            \\033[1;31m Shared lib    \\033[0m\\033[1m $(SHARED)\\033[0m \n\
            \\033[1;31m Debug         \\033[0m\\033[1m $(DEBUG)\\033[0m \n\
            \\033[1;31m F-standard    \\033[0m\\033[1m $(F03STD)\\033[0m \n\
            \\033[1;31m Optimize      \\033[0m\\033[1m $(OPTIMIZE)\\033[0m \n\
            \\033[1;31m OpenMP        \\033[0m\\033[1m $(OPENMP)\\033[0m \n\
            \\033[1;31m R16P          \\033[0m\\033[1m $(R16PCHK)\\033[0m"
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
.PHONY : PRINTINFO
.NOTPARALLEL : PRINTINFO
PRINTINFO:
	@echo | tee make.log
	@echo -e $(PRINTCHK) | tee -a make.log
	@echo | tee -a make.log
	@echo -e '\033[1;31m Compiling options\033[0m' | tee -a make.log
	@echo -e '\033[1m [$(OPTSC)]\033[0m' | tee -a make.log
	@echo | tee -a make.log
	@echo -e '\033[1;31m Linking options \033[0m' | tee -a make.log
	@echo -e '\033[1m [$(OPTSL)]\033[0m' | tee -a make.log
	@echo | tee -a make.log

.PHONY : cleanobj
cleanobj:
	@echo -e "\033[1;31m deleting objects \033[0m" | tee make.log
	@rm -fr $(DOBJ)

.PHONY : cleanmod
cleanmod:
	@echo -e "\033[1;31m deleting mods \033[0m" | tee -a make.log
	@rm -fr $(DMOD)

.PHONY : cleanlib
cleanlib:
	@echo -e "\033[1;31m deleting exes \033[0m" | tee -a make.log
	@rm -rf $(DLIB)

.PHONY : cleanmsg
cleanmsg:
	@rm -f diagnostic_messages
	@rm -f error_messages

.PHONY : clean
clean: cleanobj cleanmod cleanmsg

.PHONY : cleanall
cleanall: clean cleanlib

.PHONY : tar
tar: cleanall
	@echo -e "\033[1;31m Creating tar archive of the code \033[0m" | tee make.log
	@mkdir -p VTK_IO
	@cp -rL src makefile VTK_IO/
	@tar czf VTK_IO.tgz VTK_IO
	@rm -rf VTK_IO

.PHONY : doc
doc:
	@echo -e "\033[1;31m Building documentation\033[0m" | tee make.log
	@mkdir -p doc
	@doxygen .doxygenconfig

.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@
#----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
# rules of linking and compiling
COTEXT  = -e "\033[1;31m Compiling\033[0m\033[1m $(<F)\033[0m"
LITEXT  = -e "\033[1;31m Linking library\033[0m\033[1m $@\033[0m"

$(DOBJ)ir_precision.o : IR_Precision.f90
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io.o : Lib_VTK_IO.f90 \
	$(DOBJ)ir_precision.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

VTK_IO : PRINTINFO $(MKDIRS) $(DOBJ)ir_precision.o $(DOBJ)lib_vtk_io.o
	@echo $(LITEXT) | tee -a make.log
	@$(MAKELIB) 1>> diagnostic_messages 2>> error_messages
#-----------------------------------------------------------------------------------------------------------------------------------
