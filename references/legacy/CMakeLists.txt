
#################################################################
# Search F90 files recursively in all subdirs
#################################################################

FILE(GLOB_RECURSE LIB_SRC *.f90 *.F90)
SET(LIB_SRC ${LIB_SRC} PARENT_SCOPE)


#################################################################
# Library target
#################################################################

ADD_LIBRARY(${LIB} ${LIB_SRC})
IF (${MPI_FOUND})
    TARGET_LINK_LIBRARIES(${LIB} ${MPI_Fortran_LIBRARIES})
ENDIF ()

SET_TARGET_PROPERTIES(${LIB} PROPERTIES VERSION ${${PROJECT_NAME}_VERSION} SOVERSION ${${PROJECT_NAME}_SOVERSION})

