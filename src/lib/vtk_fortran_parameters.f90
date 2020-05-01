!< VTK_Fortran parameters.
module vtk_fortran_parameters
!< VTK_Fortran parameters.
use, intrinsic :: iso_fortran_env, only : output_unit, error_unit
use penf

implicit none
private
save
public :: stderr
public :: stdout
public :: end_rec

integer(I4P), parameter :: stderr = error_unit  !< Standard error unit.
integer(I4P), parameter :: stdout = output_unit !< Standard output unit.
character(1), parameter :: end_rec = char(10)   !< End-character for binary-record finalize.
endmodule vtk_fortran_parameters
