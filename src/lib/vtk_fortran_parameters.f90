!< Global parameters.
module vtk_fortran_parameters
!-----------------------------------------------------------------------------------------------------------------------------------
!< Global parameters.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, stderr=>error_unit
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public :: stdout
public :: stderr
public :: end_rec
public :: ascii
public :: binary
public :: raw
public :: bin_app
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
character(1), parameter :: end_rec = char(10) !< End-character for binary-record finalize.
integer(I4P), parameter :: ascii   = 0        !< Ascii-output-format parameter identifier.
integer(I4P), parameter :: binary  = 1        !< Base64-output-format parameter identifier.
integer(I4P), parameter :: raw     = 2        !< Raw-appended-binary-output-format parameter identifier.
integer(I4P), parameter :: bin_app = 3        !< Base64-appended-output-format parameter identifier.
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule vtk_fortran_parameters
