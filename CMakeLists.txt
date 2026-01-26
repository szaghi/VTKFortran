# The target of the library VTKFortran is exported
# as VTKFortran::VTKFortran to a package config file for VTKFortran
#
# usage:
#     find_package(VTKFortran)
#     ...
#     target_link_library(<target> VTKFortran)
#
# the config file is generatet in the build and install directories

cmake_minimum_required(VERSION 3.13)
project(VTKFortran VERSION 0.9.9 LANGUAGES Fortran)

set(CMAKE_VERBOSE_MAKEFILE TRUE)
# seach path for additional cmake modules
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake/Modules)


# set export variables needed for building
set(TARGETS_EXPORT_NAME "${PROJECT_NAME}Targets")
set(NAMESPACE "${PROJECT_NAME}::")

if(NOT TARGET PENF)
    add_subdirectory(src/third_party/PENF)
endif()

if(NOT TARGET BeFoR64)
    add_subdirectory(src/third_party/BeFoR64)
endif()

if(NOT TARGET FACE)
    add_subdirectory(src/third_party/FACE)
endif()

if(NOT TARGET StringiFor)
	add_subdirectory(src/third_party/StringiFor)
endif()

if(NOT TARGET FoXy)
	add_subdirectory(src/third_party/FoXy)
endif()


# ---------- Fortran module dirs + explicit deps for third_party ----------
# Put all third-party .mod files in a stable, known place
set(_vtkf_modroot "${CMAKE_BINARY_DIR}/src/third_party/")
file(MAKE_DIRECTORY "${_vtkf_modroot}")

foreach(tgt PENF BeFoR64 FACE StringiFor FoXy)
  if (TARGET ${tgt})
    set_target_properties(${tgt} PROPERTIES Fortran_MODULE_DIRECTORY "${_vtkf_modroot}/${tgt}/modules")
  endif()
endforeach()

# BeFoR64 depends on PENF
if (TARGET BeFoR64 AND TARGET PENF)
  add_dependencies(BeFoR64 PENF)
  target_include_directories(BeFoR64 PRIVATE "${_vtkf_modroot}/PENF/modules")
  # Optional: enforce link-order in some generators
  target_link_libraries(BeFoR64 PENF)
endif()

# StringiFor depends on BeFoR64, FACE, PENF
if (TARGET StringiFor)
  foreach(dep BeFoR64 FACE PENF)
    if (TARGET ${dep})
      add_dependencies(StringiFor ${dep})
      target_include_directories(StringiFor PRIVATE "${_vtkf_modroot}/${dep}/modules")
      target_link_libraries(StringiFor ${dep})
    endif()
  endforeach()
endif()

# FoXy depends on StringiFor, BeFoR64, FACE, PENF
if (TARGET FoXy)
  foreach(dep StringiFor BeFoR64 FACE PENF)
    if (TARGET ${dep})
      add_dependencies(FoXy ${dep})
      target_include_directories(FoXy PRIVATE "${_vtkf_modroot}/${dep}/modules")
      target_link_libraries(FoXy ${dep})
    endif()
  endforeach()
endif()

# generate the library and install instructions
add_subdirectory(src/lib)

# ---------- VTKFortran depends on all third_party ----------
if (TARGET VTKFortran)
  foreach(dep FoXy StringiFor BeFoR64 FACE PENF)
    if (TARGET ${dep})
      add_dependencies(VTKFortran ${dep})
      target_include_directories(VTKFortran PRIVATE "${_vtkf_modroot}/${dep}/modules")
      target_link_libraries(VTKFortran ${dep})   # plain signature is fine
    endif()
  endforeach()
endif()

# testing
if(${PROJECT_SOURCE_DIR} STREQUAL ${CMAKE_SOURCE_DIR})
    set(main_project TRUE)
else()
    set(main_project FALSE)
endif()

include(CMakeDependentOption)
cmake_dependent_option(BUILD_TESTING_${PROJECT_NAME}
     "Build the testing tree for project ${PROJECT_NAME}." OFF
     "BUILD_TESTING;NOT main_project" OFF
)

if (main_project AND NOT DEFINED BUILD_TESTING)
    set(BUILD_TESTING ON)
endif()

if((main_project AND BUILD_TESTING) OR BUILD_TESTING_${PROJECT_NAME})
    enable_testing()
    add_subdirectory(src/tests)
endif()

# generate package config files
include(GNUInstallDirs)
set(project_config "${PROJECT_NAME}Config.cmake")
set(cmake_files_dir "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles")
set(config_install_dir "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")
set(config_build_dir "${CMAKE_CURRENT_BINARY_DIR}/${config_install_dir}")

# export targets for install
install(EXPORT ${TARGETS_EXPORT_NAME}
    NAMESPACE
        ${NAMESPACE}
    DESTINATION
        ${config_install_dir}
    COMPONENT Development
)

# export targets into build
export(EXPORT ${TARGETS_EXPORT_NAME}
    NAMESPACE
        ${NAMESPACE}
    FILE
        ${config_build_dir}/${TARGETS_EXPORT_NAME}.cmake
)

#create package config
include(CMakePackageConfigHelpers)
configure_package_config_file(cmake/PackageConfig.cmake.in ${cmake_files_dir}/${project_config}
    INSTALL_DESTINATION ${config_install_dir}
)
install(FILES ${cmake_files_dir}/${project_config}
    DESTINATION ${config_install_dir}
)

configure_package_config_file(cmake/PackageConfig.cmake.in ${config_build_dir}/${project_config}
    INSTALL_DESTINATION ${config_build_dir}
    INSTALL_PREFIX ${CMAKE_CURRENT_BINARY_DIR}
)
