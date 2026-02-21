# set type specific output defaults
include(GNUInstallDirs)

SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}")
SET(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}")
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR}")
SET(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/modules")

set(LIB VTKFortran)
add_library(${LIB}
	vtk_fortran.f90
	vtk_fortran_dataarray_encoder.f90
	vtk_fortran_parameters.f90
	vtk_fortran_pvtk_file.f90
	vtk_fortran_vtk_file.f90
	vtk_fortran_vtk_file_xml_writer_abstract.f90
	vtk_fortran_vtk_file_xml_writer_appended.f90
	vtk_fortran_vtk_file_xml_writer_ascii_local.f90
	vtk_fortran_vtk_file_xml_writer_binary_local.f90
	vtk_fortran_vtm_file.F90
	)

target_link_libraries(${LIB} PENF::PENF BeFoR64::BeFoR64 FACE::FACE StringiFor::StringiFor FoXy::FoXy)

option(VTKFORTRAN_USE_ZLIB "Enable VTK XML internal zlib compression for RAW-ZLIB output." OFF)
if (VTKFORTRAN_USE_ZLIB)
  if (NOT TARGET ZLIB::ZLIB)
    find_package(ZLIB REQUIRED)
  endif()
  target_sources(${LIB} PRIVATE vtk_fortran_zlib.f90)
  target_compile_definitions(${LIB} PRIVATE VTKFORTRAN_USE_ZLIB)
  # Keep plain-signature linking to avoid mixing keyword/plain signatures for this target.
  target_link_libraries(${LIB} ZLIB::ZLIB)
endif()

add_library(${NAMESPACE}${LIB} ALIAS ${LIB})

target_include_directories(${LIB}
    INTERFACE
        $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
)

## set variables used for compile definitions of targets after support check
#include(CheckFortranSourceRuns)
#check_fortran_source_runs(
#    "program r16p_support;
#         integer, parameter :: r16p = selected_real_kind(33, 4931);
#         if(r16p < 0) stop 1;
#     end program r16p_support"
#     R16P_SUPPORTED
#     SRC_EXT f90)
#if(R16P_SUPPORTED)
#    set(r16p_supported "_R16P_SUPPORTED")
#endif()
#
#check_fortran_source_runs(
#    "program ascii_support;
#         integer, parameter :: ascii = selected_char_kind('ascii');
#         if(ascii < 0) stop 1;
#     end program ascii_support"
#    ASCII_SUPPORTED
#    SRC_EXT f90)
#if(ASCII_SUPPORTED)
#    set(ascii_supported "_ASCII_SUPPORTED")
#endif()
#
#check_fortran_source_runs(
#    "program ascii_neq_default;
#         integer, parameter :: ascii = selected_char_kind('ascii');
#         integer, parameter :: default = selected_char_kind('default');
#         if(ascii == default) stop 1;
#     end program ascii_neq_default"
#    ASCII_NEQ_DEFAULT
#    SRC_EXT f90
#)
#if(ASCII_NEQ_DEFAULT)
#    set(ascii_neq_default "_ASCII_NEQ_DEFAULT")
#endif()
#
#check_fortran_source_runs(
#    "program ucs4_support;
#         integer, parameter :: ucs4 = selected_char_kind('iso_10646');
#         if(ucs4 < 0) stop 1;
#     end program ucs4_support"
#    UCS4_SUPPORTED
#    SRC_EXT f90)
#if(UCS4_SUPPORTED)
#    set(ucs4_supported "_UCS4_SUPPORTED")
#endif()
#
#target_compile_definitions(${LIB}
#    PRIVATE
#        ${r16p_supported}
#        ${ascii_supported}
#        ${ascii_neq_default}
#        ${ucs4_supported}
#)

set_target_properties(${LIB} PROPERTIES
    VERSION
        ${PROJECT_VERSION}
    SOVERSION
        ${PROJECT_VERSION_MAJOR}
)

# installation and export of targets
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}
    COMPONENT Developement
)

install(TARGETS ${LIB} EXPORT ${TARGETS_EXPORT_NAME}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT Development
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT RuntimeLibraries
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT RuntimeLibraries
    INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}
)
