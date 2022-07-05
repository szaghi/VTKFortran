!< VTK_Fortran, pure Fortran (2003+) library to parse and emitt VTK files.
module vtk_fortran
!< VTK_Fortran, pure Fortran (2003+) library to parse and emitt VTK files.
use penf
use vtk_fortran_pvtk_file, only : pvtk_file
use vtk_fortran_vtk_file, only : vtk_file
use vtk_fortran_vtm_file, only : vtm_file

implicit none
private
public :: pvtk_file
public :: vtk_file
public :: vtm_file
public :: write_xml_volatile

contains
   function write_xml_volatile(xml_volatile, filename) result(error)
   !< Write the volatile file into a real file.
   !< This is what a master process should do into a parallel scenario where it being the only process allowed to access to
   !< filesystem: slave processes create XML volatile file econded into a characters string and master process collects and writes
   !< them by means of `write_xml_volatile`.
   character(*), intent(in) :: xml_volatile !< XML volatile file.
   character(*), intent(in) :: filename     !< XML file name.
   integer(I4P)             :: error        !< Status error.
   integer(I4P)             :: xml_unit     !< XML file unit.

   open(newunit=xml_unit,             &
        file=trim(adjustl(filename)), &
        form='UNFORMATTED',           &
        access='STREAM',              &
        action='WRITE',               &
        status='REPLACE',             &
        iostat=error)
   write(unit=xml_unit, iostat=error) xml_volatile
   endfunction write_xml_volatile
endmodule vtk_fortran
