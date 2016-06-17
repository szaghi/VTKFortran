!< VTK_Fortran, pure Fortran (2003+) library to parse and emitt VTK files.
module vtk_fortran
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK_Fortran, pure Fortran (2003+) library to parse and emitt VTK files.
!-----------------------------------------------------------------------------------------------------------------------------------
use vtk_file_class, only : vtk_file
use vtk_fortran_vtm_file, only : vtm_file
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: vtk_file
public :: vtm_file
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule vtk_fortran
