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
public :: ACTIVE_ROLES
public :: active_role

integer(I4P), parameter :: stderr = error_unit  !< Standard error unit.
integer(I4P), parameter :: stdout = output_unit !< Standard output unit.
character(1), parameter :: end_rec = char(10)   !< End-character for binary-record finalize.
character(*), parameter :: ACTIVE_ROLES(12) = [character(18) :: 'Scalars', 'Vectors', 'Normals', 'Tensors', 'TCoords', &
                                               'GlobalIds', 'PedigreeIds', 'EdgeFlag', 'Tangents', 'RationalWeights', &
                                               'HigherOrderDegrees', 'ProcessIds'] !< Attribute roles of VTK (its names).

contains
  pure function active_role(role) result(r)
  !< The index of a role in `ACTIVE_ROLES` (case insensitive), 0 if it is none.
  character(*), intent(in) :: role !< Role.
  integer(I4P)             :: r    !< Index.
  character(len=len_trim(adjustl(role))) :: lower, known !< Role and a known one, lower case.
  integer                  :: c    !< Counter.

  lower = trim(adjustl(role))
  do c=1, len(lower)
    if (lower(c:c) >= 'A' .and. lower(c:c) <= 'Z') lower(c:c) = achar(iachar(lower(c:c)) + 32)
  enddo
  do r=1, size(ACTIVE_ROLES)
    if (len_trim(ACTIVE_ROLES(r)) /= len(lower)) cycle
    known = ACTIVE_ROLES(r)
    do c=1, len(known)
      if (known(c:c) >= 'A' .and. known(c:c) <= 'Z') known(c:c) = achar(iachar(known(c:c)) + 32)
    enddo
    if (known == lower) return
  enddo
  r = 0
  endfunction active_role
endmodule vtk_fortran_parameters
