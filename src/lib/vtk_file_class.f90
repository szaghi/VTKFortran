!< VTK file class.
module vtk_file_class
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK file class.
!-----------------------------------------------------------------------------------------------------------------------------------
use befor64
use penf
use stringifor
use vtk_fortran_parameters
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public :: vtk_file
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: vtk_file
  !< VTK file class.
  private
  integer(I4P) :: format=ascii  !< Output format, integer code.
  type(string) :: format_ch     !< Output format, string code.
  type(string) :: topology      !< Mesh topology.
  integer(I4P) :: indent=0_I4P  !< Indent count.
  integer(I8P) :: ioffset=0_I8P !< Offset count.
  integer(I4P) :: xml=0_I4P     !< XML Logical unit.
  integer(I4P) :: scratch=0_I4P !< Scratch logical unit.
  integer(I4P) :: error=0_I4P   !< IO Error status.
  contains
    include 'initialize_method.inc'                 ! generic                        :: initialize
    include 'finalize_method.inc'                   ! generic                        :: finalize
    include 'write_piece_method.inc'                ! generic                        :: write_piece
    include 'write_geo_method.inc'                  ! generic                        :: write_geo
    include 'write_dataarray_method.inc'            ! generic                        :: write_dataarray
    include 'file_methods.inc'
                                                    ! procedure, pass(self), private :: open_xml_file
                                                    ! procedure, pass(self), private :: open_scratch_file
                                                    ! procedure, pass(self), private :: ioffset_update
    include 'tag_methods.inc'
                                                    ! procedure, pass(self), private :: self_closing_tag
                                                    ! procedure, pass(self), private :: tag
                                                    ! procedure, pass(self), private :: start_tag
                                                    ! procedure, pass(self), private :: end_tag
    include 'write_tag_methods.inc'
                                                    ! procedure, pass(self), private :: write_self_closing_tag
                                                    ! procedure, pass(self), private :: write_tag
                                                    ! procedure, pass(self), private :: write_start_tag
                                                    ! procedure, pass(self), private :: write_end_tag
                                                    ! procedure, pass(self), private :: write_header_tag
                                                    ! procedure, pass(self), private :: write_topology_tag
                                                    ! procedure, pass(self), private :: write_dataarray_tag
                                                    ! procedure, pass(self), private :: write_dataarray_tag_appended
                                                    ! procedure, pass(self), private :: write_dataarray_appended
    include 'write_on_scratch_dataarray_method.inc' ! generic,               private :: write_on_scratch_dataarray
    include 'encode_ascii_dataarray_method.inc'     ! generic,               private :: encode_ascii_dataarray
    include 'encode_base64_dataarray_method.inc'    ! generic,               private :: encode_base64_dataarray
endtype vtk_file
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  include 'initialize.inc'
  include 'finalize.inc'
  include 'write_piece.inc'
  include 'write_geo_strg.inc'
  include 'write_dataarray.inc'
  include 'file.inc'
  include 'tag.inc'
  include 'write_tag.inc'
  include 'write_on_scratch_dataarray.inc'
  include 'encode_ascii_dataarray.inc'
  include 'encode_base64_dataarray.inc'
endmodule vtk_file_class
