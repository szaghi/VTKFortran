# Test programs: each one writes its VTK files in the current directory, prints "Are all tests passed? T" (or F) and exits
# with a non-zero status when a check fails. CTest runs each of them in its own directory under the build tree.
set(VTKFORTRAN_TESTS
  vtk_fortran_use_module_basic.f90
  vtk_fortran_write_active_arrays.f90
  vtk_fortran_write_compressed.F90
  vtk_fortran_write_fielddata.f90
  vtk_fortran_write_header_uint64.f90
  vtk_fortran_write_large.f90
  vtk_fortran_write_multipiece.f90
  vtk_fortran_write_pvd.f90
  vtk_fortran_write_pvtr.f90
  vtk_fortran_write_pvts.f90
  vtk_fortran_write_pvtu.f90
  vtk_fortran_write_volatile.f90
  vtk_fortran_write_vti.f90
  vtk_fortran_write_vtm.f90
  vtk_fortran_write_vtp.f90
  vtk_fortran_write_vtr.f90
  vtk_fortran_write_vts.f90
  vtk_fortran_write_vtu.f90
  vtk_fortran_write_vtu_polyhedron.f90
  vtk_fortran_write_vtu_tensor.f90
  )
if (VTKFORTRAN_USE_ZLIB)
  list(APPEND VTKFORTRAN_TESTS vtk_fortran_write_vtu_zlib.F90)
endif()

foreach(test_source ${VTKFORTRAN_TESTS})
  get_filename_component(test_name ${test_source} NAME_WE)
  add_executable(${test_name} ${test_source})
  target_link_libraries(${test_name} PRIVATE VTKFortran)
  if (VTKFORTRAN_USE_ZLIB)
    target_compile_definitions(${test_name} PRIVATE VTKFORTRAN_USE_ZLIB)
  endif()
  set(test_dir ${CMAKE_CURRENT_BINARY_DIR}/run/${test_name})
  file(MAKE_DIRECTORY ${test_dir})
  add_test(NAME ${test_name} COMMAND ${test_name} WORKING_DIRECTORY ${test_dir})
endforeach()
