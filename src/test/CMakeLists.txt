#################################################################
# Test_driver target
#################################################################

FILE(GLOB_RECURSE DRIVERS_PAR_SRC *.f90  *.F90)
SET(TEST_DRIVERS_SRC ${DRIVERS_PAR_SRC} PARENT_SCOPE)

FOREACH(DRIVER_SRC ${DRIVERS_PAR_SRC})
    GET_FILENAME_COMPONENT(EXE_NAME ${DRIVER_SRC} NAME_WE)
    ADD_EXECUTABLE(${EXE_NAME} ${DRIVER_SRC})
    TARGET_LINK_LIBRARIES(${EXE_NAME} ${LIB})
ENDFOREACH()
