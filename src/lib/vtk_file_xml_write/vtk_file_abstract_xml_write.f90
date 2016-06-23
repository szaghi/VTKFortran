!< VTK file abstract XML write.
module vtk_file_abstract_xml_write
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK file abstract XML write.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: abstract_xml_write
  !< VTK file abstract XML write.
  contains
    procedure(abstract_xml_write_procedure), deferred, pass :: set_state !< Set state.
endtype abstract_xml_write
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: abstract_xml_write_state
  !< VTK file abstract XML write state.
  contains
    procedure(abstract_xml_write_state_procedure), deferred, pass :: write !< Write VTK file data.
endtype abstract_xml_write_state
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
abstract interface
  subroutine abstract_xml_write_procedure(self, state)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Interface of abstract XML write procedures.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: abstract_xml_write
  import :: abstract_xml_write_state
  class(abstract_xml_write),       intent(inout) :: self  !< Write object.
  class(abstract_xml_write_state), intent(in)    :: state !< Write object state.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine abstract_xml_write_procedure

  subroutine abstract_xml_write_state_procedure(self, xml_write)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Interface of abstract XML write state procedure.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: abstract_xml_write
  import :: abstract_xml_write_state
  class(abstract_xml_write_state), intent(inout) :: self      !< Write state object.
  class(abstract_xml_write),       intent(inout) :: xml_write !< Write object.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine abstract_xml_write_state_procedure
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule vtk_file_abstract_xml_write
