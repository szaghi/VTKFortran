#################################################################
# HEADER
#################################################################
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.12)
SET(CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS TRUE)


PROJECT(VTK_IO Fortran C)

# Note the agreement between the first number of the triplet 
# VERSION and the SOVERSION
SET(${PROJECT_NAME}_VERSION 0.0.1)
SET(${PROJECT_NAME}_SOVERSION 0)

SET(LIB ${PROJECT_NAME})  

SET(CMAKE_VERBOSE_MAKEFILE TRUE)


#################################################################
# DEFINE PATHS
#################################################################

SET(SRC_PATH ${CMAKE_SOURCE_DIR}/src)
SET(LIB_SRC_PATH ${SRC_PATH}/lib)
SET(TEST_SRC_PATH ${SRC_PATH}/test)

SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMake/Modules/")


#################################################################
# BUILD PATHS
#################################################################

SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
SET(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
INCLUDE_DIRECTORIES(${CMAKE_Fortran_MODULE_DIRECTORY})


#################################################################
# CONFIGURATION TYPES & BUILD MODE
#################################################################

SET(CMAKE_CONFIGURATION_TYPES DEBUG RELEASE)
IF(NOT CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: NONE DEBUG RELEASE"
      FORCE)

  SET_PROPERTY(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS NONE DEBUG RELEASE)
ENDIF(NOT CMAKE_BUILD_TYPE)


#################################################################
# Setting up options
#################################################################

GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

OPTION(${PROJECT_NAME}_ENABLE_F03STD "To force strict conformance to the Fortran 2003 standard" OFF)
OPTION(${PROJECT_NAME}_ENABLE_OPTIMIZE "To activate optimization flags" OFF)
OPTION(${PROJECT_NAME}_ENABLE_OPENMP "To activate the OpenMP extensions for Fortran" OFF)
OPTION(${PROJECT_NAME}_ENABLE_MPI "To compile MPI tests and link against MPI library" OFF)
OPTION(${PROJECT_NAME}_ENABLE_R16P "To define a real precission type with 128 bits" OFF)
#OPTION(${PROJECT_NAME}_ENABLE_HUGE "To use 64 bit instead 32 bit integer as a bit counter" OFF)



IF (${${PROJECT_NAME}_ENABLE_F03STD})
    IF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" OR Fortran_COMPILER_NAME MATCHES "gfortran*")
        SET(EXTRA_FLAGS "${EXTRA_FLAGS}  -std=f2003 -fall-intrinsics")
    ELSEIF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR Fortran_COMPILER_NAME MATCHES "ifort*")
        SET(EXTRA_FLAGS "${EXTRA_FLAGS}  -std03")
    ENDIF ()
ENDIF ()

IF (${${PROJECT_NAME}_ENABLE_OPTIMIZE})
    IF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" OR Fortran_COMPILER_NAME MATCHES "gfortran*")
        SET(EXTRA_FLAGS "${EXTRA_FLAGS}  -O3")
    ELSEIF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR Fortran_COMPILER_NAME MATCHES "ifort*")
        SET(EXTRA_FLAGS "${EXTRA_FLAGS}  -O3 -ipo -inline all -ipo-jobs4 -vec-report1")
    ENDIF ()
ENDIF ()

IF (${${PROJECT_NAME}_ENABLE_OPENMP})
    FIND_PACKAGE( OpenMP )
    IF (${OPENMP_FOUND})
        SET(EXTRA_FLAGS ${OpenMP_Fortran_FLAGS})
        ADD_DEFINITIONS("-DOPENMP")
    ELSE ()
        OPTION(${PROJECT_NAME}_ENABLE_OPENMP "To activate the OpenMP extensions for Fortran" OFF)
    ENDIF ()
ENDIF ()

IF (${${PROJECT_NAME}_ENABLE_MPI})
    FIND_PACKAGE(MPI)
    IF(${MPI_FOUND})
        INCLUDE(CMakeForceCompiler)
        CMAKE_FORCE_Fortran_COMPILER(${MPI_Fortran_COMPILER} ${CMAKE_Fortran_COMPILER_ID})
        ADD_DEFINITIONS(${MPI_Fortran_COMPILE_FLAGS})
        INCLUDE_DIRECTORIES(${MPI_Fortran_INCLUDE_PATH})
        LINK_DIRECTORIES(${MPI_Fortran_LIBRARIES})
        ADD_DEFINITIONS("-DMPI2")
    ELSE ()
        OPTION(${PROJECT_NAME}_ENABLE_MPI "To compile MPI tests and link against MPI library" OFF)
    ENDIF ()
ENDIF ()

IF (${${PROJECT_NAME}_ENABLE_R16P})
    ADD_DEFINITIONS("-Dr16p")
ENDIF ()

#IF (${${PROJECT_NAME}_ENABLE_HUGE})
#    ADD_DEFINITIONS("-DHUGE")
#ENDIF ()


#################################################################
# FFLAGS depend on the compiler and the build type
#################################################################


IF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" OR Fortran_COMPILER_NAME MATCHES "gfortran*")
  # gfortran 
  set(FORTRAN_FLAGS "-cpp -frealloc-lhs ")
  set (CMAKE_Fortran_FLAGS "${FORTRAN_FLAGS} " CACHE STRING "" FORCE)
  set (CMAKE_Fortran_FLAGS_RELEASE "${EXTRA_FLAGS} " CACHE STRING "" FORCE)
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g3 -Warray-bounds -Wcharacter-truncation -Wline-truncation -Wimplicit-interface -Wimplicit-procedure -Wunderflow -Wuninitialized -fcheck=all -fmodule-private -ffree-line-length-132 -fimplicit-none -fbacktrace -fdump-core -finit-real=nan -std=f2003 -fall-intrinsics " CACHE STRING "" FORCE)
ELSEIF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR Fortran_COMPILER_NAME MATCHES "ifort*")
  # ifort
  set(FORTRAN_FLAGS "-cpp -assume realloc_lhs ")
  set (CMAKE_Fortran_FLAGS "${FORTRAN_FLAGS} " CACHE STRING "" FORCE)
  set (CMAKE_Fortran_FLAGS_RELEASE "${EXTRA_FLAGS} " CACHE STRING "" FORCE)
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -debug all -check all -warn all -extend-source 132 -traceback -gen-interfaces " CACHE STRING "" FORCE)
#-fpe-all=0 -fp-stack-check -fstack-protector-all -ftrapuv -no-ftz -std03
ELSE ()
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
ENDIF ()

message("Fortra_FLAGS: ${FORTRAN_FLAGS}")
message("CMAKE_Fortran_FLAGS: ${CMAKE_Fortran_FLAGS}")
message("CMAKE_Fortran_FLAGS_RELEASE: ${CMAKE_Fortran_FLAGS_RELEASE}")
message("CMAKE_Fortra_FLAGS_DEBUG: ${CMAKE_Fortran_FLAGS_DEBUG}")


#################################################################
# ADD SOURCE SUBDIRS
#################################################################


ADD_SUBDIRECTORY(${LIB_SRC_PATH})
ADD_SUBDIRECTORY(${TEST_SRC_PATH})

