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
MPI      = no
R16P     = no
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# compiler specific rules
# GNU
WRN_GNU = -fmax-errors=0 -Wall -Wno-array-temporaries -Warray-bounds -Wcharacter-truncation -Wline-truncation -Wconversion-extra -Wimplicit-interface -Wimplicit-procedure -Wunderflow -Wextra -Wuninitialized
CHK_GNU = -fcheck=all
DEB_GNU = -fmodule-private -ffree-line-length-132 -fimplicit-none -ffpe-trap=invalid,overflow -fbacktrace -fdump-core -finit-real=nan #-fno-range-check  ,precision,denormal,underflow
STD_GNU = -std=f2003 -fall-intrinsics
OMP_GNU = -fopenmp
OPT_GNU = -O3
PRF_GNU =
# Intel
WRN_INT = -warn all
CHK_INT = -check all
DEB_INT = -debug all -extend-source 132 -fpe-all=0 -fp-stack-check -fstack-protector-all -ftrapuv -no-ftz -traceback -gen-interfaces
STD_INT = -std03
OMP_INT = -openmp
OPT_INT = -O3 -ipo -inline all -ipo-jobs4 -vec-report1
PRF_INT = #-p
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
.PHONY : DEFAULTRULE
DEFAULTRULE: Lib_VTK_IO

.PHONY : help
help:
	@echo
	@echo -e '\033[1;31m Make options of Lib_VTK_IO code\033[0m'
	@echo
	@echo -e '\033[1;31m Compiler choice: COMPILER=$(COMPILER)\033[0m\033[1m => default\033[0m'
	@echo -e '\033[1;31m  COMPILER=gnu  \033[0m\033[1m => GNU gfortran          \033[0m'
	@echo -e '\033[1;31m  COMPILER=intel\033[0m\033[1m => Intel Fortran         \033[0m'
	@echo
	@echo -e '\033[1;31m Compiling options\033[0m'
	@echo -e '\033[1;31m  SHARED=yes(no)  \033[0m\033[1m => on(off) shared library         (default $(SHARED))\033[0m'
	@echo -e '\033[1;31m  DEBUG=yes(no)   \033[0m\033[1m => on(off) debug                  (default $(DEBUG))\033[0m'
	@echo -e '\033[1;31m  F03STD=yes(no)  \033[0m\033[1m => on(off) check standard fortran (default $(F03STD))\033[0m'
	@echo -e '\033[1;31m  OPTIMIZE=yes(no)\033[0m\033[1m => on(off) optimization           (default $(OPTIMIZE))\033[0m'
	@echo -e '\033[1;31m  OPENMP=yes(no)  \033[0m\033[1m => on(off) OpenMP directives      (default $(OPENMP))\033[0m'
	@echo -e '\033[1;31m  MPI=yes(no)     \033[0m\033[1m => on(off) MPI library            (default $(MPI))\033[0m'
	@echo
	@echo -e '\033[1;31m Preprocessing options\033[0m'
	@echo -e '\033[1;31m  R16P=yes(no)\033[0m\033[1m => on(off) definition of real with "128 bit" (default $(R16P))\033[0m'
	@echo
	@echo -e '\033[1;31m Provided Rules: default=Lib_VTK_IO\033[0m\033[1m => compile the library\033[0m'
	@echo -e '\033[1;31m  Defualt rule =>\033[0m\033[1m Lib_VRK_IO \033[0m'
	@echo -e '\033[1;31m  help         =>\033[0m\033[1m printing this help message\033[0m'
	@echo -e '\033[1;31m  Lib_VTK_IO   =>\033[0m\033[1m compile the library\033[0m'
	@echo -e '\033[1;31m  Test_Driver  =>\033[0m\033[1m compile Test_Driver program\033[0m'
	@echo -e '\033[1;31m  cleanobj     =>\033[0m\033[1m cleaning compiled object\033[0m'
	@echo -e '\033[1;31m  cleanmod     =>\033[0m\033[1m cleaning .mod files\033[0m'
	@echo -e '\033[1;31m  cleanmsg     =>\033[0m\033[1m cleaning make-log massage files\033[0m'
	@echo -e '\033[1;31m  cleanlib     =>\033[0m\033[1m cleaning library\033[0m'
	@echo -e '\033[1;31m  clean        =>\033[0m\033[1m running cleanobj, cleanmod and cleanmsg\033[0m'
	@echo -e '\033[1;31m  cleanall     =>\033[0m\033[1m running clean and cleanexe\033[0m'
	@echo -e '\033[1;31m  tar          =>\033[0m\033[1m creating a tar archive of the project\033[0m'
	@echo -e '\033[1;31m  doc          =>\033[0m\033[1m building the documentation\033[0m'
	@echo
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# directory & file
DSRC  = ./src/lib/ ./src/test/ ./src/third_party/BeFoR64/src/lib/ ./src/third_party/PENF/src/lib/
DEXE  = ./
ifeq "$(SHARED)" "yes"
  DLIB    = ./shared/
  DOBJ    = $(DLIB)obj/
  DMOD    = $(DLIB)mod/
  MAIN    = $(DLIB)Lib_VTK_IO.so
  MAKELIB = $(FC) $(OPTSL) $(DOBJ)lib_vtk_io.o -o $(MAIN)
else
  DLIB    = ./static/
  DOBJ    = $(DLIB)obj/
  DMOD    = $(DLIB)mod/
  MAIN    = $(DLIB)Lib_VTK_IO.a
  MAKELIB = ar -rcs $(MAIN) $(DOBJ)*.o ; ranlib $(MAIN)
endif
MKDIRS = $(DOBJ) $(DMOD) $(DLIB)
VPATH = $(DSRC) $(DOBJ) $(DMOD)
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# compiling and linking options
ifeq "$(COMPILER)" "gnu"
  FC      = gfortran
  OPTSC   = -cpp -c -J$(DMOD)
  OPTSL   =
  WRN = $(WRN_GNU)
  CHK = $(CHK_GNU)
  DEB = $(DEB_GNU)
  STD = $(STD_GNU)
  OMP = $(OMP_GNU)
  OPT = $(OPT_GNU)
  PRF = $(PRF_GNU)
  PREPROC =
  ifeq "$(SHARED)" "yes"
    OPTSC := $(OPTSC) -fPIC
    OPTSL := $(OPTSL) -shared
  endif
endif
ifeq "$(COMPILER)" "intel"
  FC      = ifort
  OPTSC   = -cpp -c -module $(DMOD)
  OPTSL   =
  WRN = $(WRN_INT)
  CHK = $(CHK_INT)
  DEB = $(DEB_INT)
  STD = $(STD_INT)
  OMP = $(OMP_INT)
  OPT = $(OPT_INT)
  PRF = $(PRF_INT)
  PREPROC =
  ifeq "$(SHARED)" "yes"
    OPTSC := $(OPTSC) -fpic
    OPTSL := $(OPTSL) -shared
  endif
endif
ifeq "$(DEBUG)" "yes"
  PREPROC := $(PREPROC) -DDEBUG
  OPTSC := $(OPTSC) -O0 -C -g $(WRN) $(CHK) $(DEB)
  OPTSL := $(OPTSL) -O0 -C -g $(WRN) $(CHK) $(DEB)
endif
ifeq "$(F03STD)" "yes"
  OPTSC := $(OPTSC) $(STD)
  OPTSL := $(OPTSL) $(STD)
endif
ifeq "$(OPTIMIZE)" "yes"
  OPTSC := $(OPTSC) $(OPT)
  OPTSL := $(OPTSL) $(OPT)
endif
ifeq "$(OPENMP)" "yes"
  PREPROC := $(PREPROC) -DOPENMP
  OPTSC := $(OPTSC) $(OMP)
  OPTSL := $(OPTSL) $(OMP)
endif
ifeq "$(MPI)" "yes"
  PREPROC := $(PREPROC) -DMPI2
  FC = mpif90
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
            \\033[1;31m MPI           \\033[0m\\033[1m $(MPI)\\033[0m \n\
            \\033[1;31m R16P          \\033[0m\\033[1m $(R16PCHK)\\033[0m"
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
.PHONY : PRINTINFO
.NOTPARALLEL : PRINTINFO
PRINTINFO:
	@echo | tee make.log
	@echo -e $(PRINTCHK) | tee -a make.log
	@echo | tee -a make.log
	@echo -e "\033[1;31m Compiling options\033[0m" | tee -a make.log
	@echo -e "\033[1m [$(OPTSC)]\033[0m" | tee -a make.log
	@echo | tee -a make.log
	@echo -e "\033[1;31m Linking options \033[0m" | tee -a make.log
	@echo -e "\033[1m [$(OPTSL)]\033[0m" | tee -a make.log
	@echo | tee -a make.log

.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@

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
	@echo -e "\033[1;31m deleting library \033[0m" | tee -a make.log
	@rm -rf $(DLIB)

.PHONY : cleanmsg
cleanmsg:
	@rm -f diagnostic_messages
	@rm -f error_messages

.PHONY : clean
clean: cleanobj cleanmod cleanmsg

.PHONY : cleanall
cleanall: clean cleanlib
	@rm -f $(DEXE)Test_Driver

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
	@doxygen .doxygenconfig
#----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
COTEXT  = -e "\033[1;31m Compiling\033[0m\033[1m $(<F)\033[0m"
LITEXT  = -e "\033[1;31m Linking library\033[0m\033[1m $@\033[0m"

# linking rules
Lib_VTK_IO : PRINTINFO $(MKDIRS) $(DOBJ)penf.o $(DOBJ)lib_vtk_io.o
	@echo $(LITEXT) | tee -a make.log
	@$(MAKELIB) 1>> diagnostic_messages 2>> error_messages

$(DEXE)Test_Driver : PRINTINFO $(MKDIRS) $(DOBJ)Test_Driver.o
	@echo | tee -a make.log
	@echo $(LITEXT) | tee -a make.log
	@$(FC) $(OPTSL) $(DOBJ)*.o $(LIBS) -o $@ 1>> diagnostic_messages 2>> error_messages

# compiling rules
$(DOBJ)penf.o : penf.F90
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_pack_data.o : Lib_Pack_Data.f90 \
	$(DOBJ)penf.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)befor64.o : befor64.F90 \
	$(DOBJ)penf.o\
	$(DOBJ)lib_pack_data.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_pvtk_xml.o: Lib_VTK_IO_PVTK_XML.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)befor64.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_end_xml.o: Lib_VTK_IO_END_XML.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)befor64.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_geo.o: Lib_VTK_IO_GEO.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_ini_xml.o: Lib_VTK_IO_INI_XML.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)befor64.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_fld_xml.o: Lib_VTK_IO_FLD_XML.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)befor64.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_dat_var_xml.o: Lib_VTK_IO_DAT_VAR_XML.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)befor64.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_con_xml.o: Lib_VTK_IO_CON_XML.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)befor64.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_back_end.o: Lib_VTK_IO_Back_End.f90 \
	$(DOBJ)penf.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_ini.o: Lib_VTK_IO_INI.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)befor64.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_pvd_xml.o: Lib_VTK_IO_PVD_XML.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)befor64.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_geo_xml.o: Lib_VTK_IO_GEO_XML.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)befor64.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_end.o: Lib_VTK_IO_END.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_con.o: Lib_VTK_IO_CON.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_dat_var.o: Lib_VTK_IO_DAT_VAR.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io_vtm_xml.o: Lib_VTK_IO_VTM_XML.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)befor64.o \
	$(DOBJ)lib_vtk_io_back_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_vtk_io.o : Lib_VTK_IO.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)befor64.o \
	$(DOBJ)lib_vtk_io_back_end.o \
	$(DOBJ)lib_vtk_io_ini_xml.o \
	$(DOBJ)lib_vtk_io_fld_xml.o \
	$(DOBJ)lib_vtk_io_geo_xml.o \
	$(DOBJ)lib_vtk_io_con_xml.o \
	$(DOBJ)lib_vtk_io_dat_var_xml.o \
	$(DOBJ)lib_vtk_io_end_xml.o \
	$(DOBJ)lib_vtk_io_vtm_xml.o \
	$(DOBJ)lib_vtk_io_pvtk_xml.o \
	$(DOBJ)lib_vtk_io_pvd_xml.o \
	$(DOBJ)lib_vtk_io_ini.o \
	$(DOBJ)lib_vtk_io_geo.o \
	$(DOBJ)lib_vtk_io_con.o \
	$(DOBJ)lib_vtk_io_dat_var.o \
	$(DOBJ)lib_vtk_io_end.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)Test_Driver.o : Test_Driver.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)lib_vtk_io.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages
#-----------------------------------------------------------------------------------------------------------------------------------
