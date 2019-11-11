#!/usr/bin/make
#----------------------------------------------------------------------------------------------------------------------------------
# make init
MAKEFLAGS = -j 1

# shell
SHELL = /bin/bash
# no verbose
$(VERBOSE).SILENT:
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# User options
TESTS    = no
SHARED   = no
STATIC   = yes
COMPILER = gnu
DEBUG    = no
F03STD   = no
OPTIMIZE = no
OPENMP   = no
MPI      = no
ifeq "$(TESTS)" "yes"
  RULE   = ALLTESTS
  DEXE   = exe/
  SHARED = no
  STATIC = no
else
  RULE = VTKFortran
endif
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
CHK_INT = -check arg_temp_created -check format -check assume -check format -check output_conversion -check pointers -check stack -check uninit
DEB_INT = -debug all -extend-source 132 -fpe-all=0 -fp-stack-check -fstack-protector-all -ftrapuv -no-ftz -traceback -gen-interfaces
STD_INT = -std03
OMP_INT = -openmp
OPT_INT = -O3 -ipo -inline all -ipo-jobs4 -vec-report1
PRF_INT = #-p
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
firstrule : $(RULE)

.PHONY : help
help:
	@echo
	@echo -e '\033[1;31m Make options of VTKFortran code\033[0m'
	@echo
	@echo -e '\033[1;31m Compiler choice: COMPILER=$(COMPILER)\033[0m\033[1m => default\033[0m'
	@echo -e '\033[1;31m  COMPILER=gnu  \033[0m\033[1m => GNU gfortran          \033[0m'
	@echo -e '\033[1;31m  COMPILER=intel\033[0m\033[1m => Intel Fortran         \033[0m'
	@echo
	@echo -e '\033[1;31m Compiling options\033[0m'
	@echo -e '\033[1;31m  TESTS=yes(no)   \033[0m\033[1m => on(off) tests build            (default $(TESTS))\033[0m'
	@echo -e '\033[1;31m  SHARED=yes(no)  \033[0m\033[1m => on(off) shared library         (default $(SHARED))\033[0m'
	@echo -e '\033[1;31m  DEBUG=yes(no)   \033[0m\033[1m => on(off) debug                  (default $(DEBUG))\033[0m'
	@echo -e '\033[1;31m  F03STD=yes(no)  \033[0m\033[1m => on(off) check standard fortran (default $(F03STD))\033[0m'
	@echo -e '\033[1;31m  OPTIMIZE=yes(no)\033[0m\033[1m => on(off) optimization           (default $(OPTIMIZE))\033[0m'
	@echo -e '\033[1;31m  OPENMP=yes(no)  \033[0m\033[1m => on(off) OpenMP directives      (default $(OPENMP))\033[0m'
	@echo -e '\033[1;31m  MPI=yes(no)     \033[0m\033[1m => on(off) MPI library            (default $(MPI))\033[0m'
	@echo
	@echo -e '\033[1;31m Preprocessing options\033[0m'
	@echo
	@echo -e '\033[1;31m Provided Rules: default=VTKFortran\033[0m\033[1m => compile the library\033[0m'
	@echo -e '\033[1;31m  Defualt rule =>\033[0m\033[1m Lib_VRK_IO \033[0m'
	@echo -e '\033[1;31m  help         =>\033[0m\033[1m printing this help message\033[0m'
	@echo -e '\033[1;31m  VTKFortran   =>\033[0m\033[1m compile the library\033[0m'
	@echo -e '\033[1;31m  Test_Driver  =>\033[0m\033[1m compile Test_Driver program\033[0m'
	@echo -e '\033[1;31m  cleanobj     =>\033[0m\033[1m cleaning compiled object\033[0m'
	@echo -e '\033[1;31m  cleanmod     =>\033[0m\033[1m cleaning .mod files\033[0m'
	@echo -e '\033[1;31m  cleanmsg     =>\033[0m\033[1m cleaning make-log massage files\033[0m'
	@echo -e '\033[1;31m  cleanlib     =>\033[0m\033[1m cleaning library\033[0m'
	@echo -e '\033[1;31m  clean        =>\033[0m\033[1m running cleanobj, cleanmod and cleanmsg\033[0m'
	@echo -e '\033[1;31m  cleanall     =>\033[0m\033[1m running clean and cleanexe\033[0m'
	@echo -e '\033[1;31m  tar          =>\033[0m\033[1m creating a tar archive of the project\033[0m'
	@echo
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# directory & file
DSRC  = src/
ifeq "$(SHARED)" "yes"
  DEXE    = ./shared/
  MAIN    = $(DEXE)libvtkfortran.so
  MAKELIB = $(FC) $(OPTSL) $(DOBJ)vtkfortran.o -o $(MAIN)
endif
ifeq "$(STATIC)" "yes"
  DEXE    = ./static/
  MAIN    = $(DEXE)libvtkfortran.a
  MAKELIB = ar -rcs $(MAIN) $(DOBJ)*.o ; ranlib $(MAIN)
endif
DOBJ    = $(DEXE)obj/
DMOD    = $(DEXE)mod/
MKDIRS  = $(DOBJ) $(DMOD) $(DEXE)
VPATH   = $(DSRC) $(DOBJ) $(DMOD)
LCEXES  = $(shell echo $(EXES) | tr '[:upper:]' '[:lower:]')
EXESPO  = $(addsuffix .o,$(LCEXES))
EXESOBJ = $(addprefix $(DOBJ),$(EXESPO))
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

OPTSC := $(OPTSC) $(PREPROC)
OPTSL := $(OPTSL) $(PREPROC)

WHICHFC = $(shell which $(FC))

PRINTCHK = "\\033[1;31m Compiler used \\033[0m\\033[1m $(COMPILER) => $(WHICHFC)\\033[0m \n\
            \\033[1;31mSource dir    \\033[0m\\033[1m $(DSRC)\\033[0m \n\
            \\033[1;31m Lib dir       \\033[0m\\033[1m $(DEXE)\\033[0m \n\
            \\033[1;31m Libray        \\033[0m\\033[1m $(MAIN)\\033[0m \n\
            \\033[1;31m Shared lib    \\033[0m\\033[1m $(SHARED)\\033[0m \n\
            \\033[1;31m Debug         \\033[0m\\033[1m $(DEBUG)\\033[0m \n\
            \\033[1;31m F-standard    \\033[0m\\033[1m $(F03STD)\\033[0m \n\
            \\033[1;31m Optimize      \\033[0m\\033[1m $(OPTIMIZE)\\033[0m \n\
            \\033[1;31m OpenMP        \\033[0m\\033[1m $(OPENMP)\\033[0m \n\
            \\033[1;31m MPI           \\033[0m\\033[1m $(MPI)\\033[0m "
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
.PHONY : PRINTINFO
.NOTPARALLEL : PRINTINFO
PRINTINFO:
	@echo
	@echo -e $(PRINTCHK)
	@echo
	@echo -e "\033[1;31m Compile options\033[0m"
	@echo -e "\033[1m [$(OPTSC)]\033[0m"
	@echo
	@echo -e "\033[1;31m Link options \033[0m"
	@echo -e "\033[1m [$(OPTSL)]\033[0m"
	@echo

.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@

.PHONY : cleanobj
cleanobj:
	@echo -e "\033[1;31m deleting objects \033[0m"
	@rm -fr $(DOBJ)

.PHONY : cleanmod
cleanmod:
	@echo -e "\033[1;31m deleting mods \033[0m"
	@rm -fr $(DMOD)

.PHONY : cleanlib
cleanlib:
	@echo -e "\033[1;31m deleting library \033[0m"
	@rm -rf $(DEXE)

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
	@echo -e "\033[1;31m Create tar archive of the code \033[0m"
	@tar --xform="s%^%VTKFortran/%" -czf VTKFortran.tar.gz *
#----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
COTEXT  = -e "\033[1;31m Compile\033[0m\033[1m $(<F)\033[0m"
LITEXT  = -e "\033[1;31m Assemble\033[0m\033[1m $@\033[0m"

# linking rules
.NOTPARALLEL : ALLTESTS
ALLTESTS : $(DEXE)USE_MODULE_BASIC $(DEXE)WRITE_VTS $(DEXE)WRITE_VTR $(DEXE)WRITE_VTU

VTKFortran : PRINTINFO $(MKDIRS) $(DOBJ)vtk_fortran.o
	@echo $(LITEXT)
	@$(MAKELIB)

$(DEXE)USE_MODULE_BASIC : PRINTINFO $(MKDIRS) $(DOBJ)use_module_basic.o
	@rm -f $(filter-out $(DOBJ)use_module_basic.o,$(EXESOBJ))
	@echo $(LITEXT)
	@$(FC) $(OPTSL) $(DOBJ)*.o $(LIBS) -o $@
EXES := $(EXES) USE_MODULE_BASIC

$(DEXE)WRITE_VTS : PRINTINFO $(MKDIRS) $(DOBJ)write_vts.o
	@rm -f $(filter-out $(DOBJ)write_vts.o,$(EXESOBJ))
	@echo $(LITEXT)
	@$(FC) $(OPTSL) $(DOBJ)*.o $(LIBS) -o $@
EXES := $(EXES) WRITE_VTS

$(DEXE)WRITE_VTR : PRINTINFO $(MKDIRS) $(DOBJ)write_vtr.o
	@rm -f $(filter-out $(DOBJ)write_vtr.o,$(EXESOBJ))
	@echo $(LITEXT)
	@$(FC) $(OPTSL) $(DOBJ)*.o $(LIBS) -o $@
EXES := $(EXES) WRITE_VTR

$(DEXE)WRITE_VTU : PRINTINFO $(MKDIRS) $(DOBJ)write_vtu.o
	@rm -f $(filter-out $(DOBJ)write_vtu.o,$(EXESOBJ))
	@echo $(LITEXT)
	@$(FC) $(OPTSL) $(DOBJ)*.o $(LIBS) -o $@
EXES := $(EXES) WRITE_VTU

# compiling rules
$(DOBJ)vtk_fortran.o: src/lib/vtk_fortran.f90 \
	$(DOBJ)vtk_fortran_pvtk_file.o \
	$(DOBJ)vtk_fortran_vtk_file.o \
	$(DOBJ)vtk_fortran_vtm_file.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)vtk_fortran_vtm_file.o: src/lib/vtk_fortran_vtm_file.F90 \
	$(DOBJ)befor64.o \
	$(DOBJ)penf.o \
	$(DOBJ)vtk_fortran_vtk_file_xml_writer_abstract.o \
	$(DOBJ)vtk_fortran_vtk_file_xml_writer_ascii_local.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)vtk_fortran_vtk_file_xml_writer_ascii_local.o: src/lib/vtk_fortran_vtk_file_xml_writer_ascii_local.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)stringifor.o \
	$(DOBJ)vtk_fortran_dataarray_encoder.o \
	$(DOBJ)vtk_fortran_vtk_file_xml_writer_abstract.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)vtk_fortran_dataarray_encoder.o: src/lib/vtk_fortran_dataarray_encoder.f90 \
	$(DOBJ)befor64.o \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)vtk_fortran_pvtk_file.o: src/lib/vtk_fortran_pvtk_file.f90 \
	$(DOBJ)befor64.o \
	$(DOBJ)penf.o \
	$(DOBJ)vtk_fortran_vtk_file_xml_writer_abstract.o \
	$(DOBJ)vtk_fortran_vtk_file_xml_writer_ascii_local.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)vtk_fortran_vtk_file_xml_writer_binary_local.o: src/lib/vtk_fortran_vtk_file_xml_writer_binary_local.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)stringifor.o \
	$(DOBJ)vtk_fortran_dataarray_encoder.o \
	$(DOBJ)vtk_fortran_vtk_file_xml_writer_abstract.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)vtk_fortran_vtk_file_xml_writer_abstract.o: src/lib/vtk_fortran_vtk_file_xml_writer_abstract.f90 \
	$(DOBJ)foxy.o \
	$(DOBJ)penf.o \
	$(DOBJ)stringifor.o \
	$(DOBJ)vtk_fortran_parameters.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)vtk_fortran_vtk_file_xml_writer_appended.o: src/lib/vtk_fortran_vtk_file_xml_writer_appended.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)stringifor.o \
	$(DOBJ)vtk_fortran_dataarray_encoder.o \
	$(DOBJ)vtk_fortran_parameters.o \
	$(DOBJ)vtk_fortran_vtk_file_xml_writer_abstract.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)vtk_fortran_vtk_file.o: src/lib/vtk_fortran_vtk_file.f90 \
	$(DOBJ)befor64.o \
	$(DOBJ)penf.o \
	$(DOBJ)stringifor.o \
	$(DOBJ)vtk_fortran_vtk_file_xml_writer_abstract.o \
	$(DOBJ)vtk_fortran_vtk_file_xml_writer_appended.o \
	$(DOBJ)vtk_fortran_vtk_file_xml_writer_ascii_local.o \
	$(DOBJ)vtk_fortran_vtk_file_xml_writer_binary_local.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)vtk_fortran_parameters.o: src/lib/vtk_fortran_parameters.f90 \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)stringifor.o: src/third_party/StringiFor/src/lib/stringifor.F90 \
	$(DOBJ)penf.o \
	$(DOBJ)stringifor_string_t.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)stringifor_string_t.o: src/third_party/StringiFor/src/lib/stringifor_string_t.F90 \
	$(DOBJ)befor64.o \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)penf.o: src/third_party/PENF/src/lib/penf.F90 \
	$(DOBJ)penf_global_parameters_variables.o \
	$(DOBJ)penf_b_size.o \
	$(DOBJ)penf_stringify.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@
	@rm -f $(DOBJ)penf_global_parameters_variables.o $(DOBJ)penf_b_size.o $(DOBJ)penf_stringify.o

$(DOBJ)befor64_pack_data_m.o: src/third_party/BeFoR64/src/lib/befor64_pack_data_m.F90 \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)befor64.o: src/third_party/BeFoR64/src/lib/befor64.F90 \
	$(DOBJ)penf.o \
	$(DOBJ)befor64_pack_data_m.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)penf_b_size.o: src/third_party/PENF/src/lib/penf_b_size.F90 \
	$(DOBJ)penf_global_parameters_variables.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)penf_stringify.o: src/third_party/PENF/src/lib/penf_stringify.F90 \
	$(DOBJ)penf_b_size.o \
	$(DOBJ)penf_global_parameters_variables.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)penf_global_parameters_variables.o: src/third_party/PENF/src/lib/penf_global_parameters_variables.F90
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)foxy_xml_file.o: src/third_party/FoXy/src/lib/foxy_xml_file.f90 \
	$(DOBJ)foxy_xml_tag.o \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)foxy_xml_tag.o: src/third_party/FoXy/src/lib/foxy_xml_tag.F90 \
	$(DOBJ)penf.o \
	$(DOBJ)stringifor.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)foxy.o: src/third_party/FoXy/src/lib/foxy.f90 \
	$(DOBJ)foxy_xml_file.o \
	$(DOBJ)foxy_xml_tag.o \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)use_module_basic.o: src/tests/use_module_basic.f90 \
	$(DOBJ)vtk_fortran.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)write_vts.o: src/tests/write_vts.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)vtk_fortran.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)write_vtr.o: src/tests/write_vtr.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)vtk_fortran.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)write_vtu.o: src/tests/write_vtu.f90 \
	$(DOBJ)penf.o \
	$(DOBJ)vtk_fortran.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@
#-----------------------------------------------------------------------------------------------------------------------------------
