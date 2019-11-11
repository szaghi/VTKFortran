!< FoXy, Fortran XML parser for poor people
module foxy
!< FoXy, Fortran XML parser for poor people
use foxy_xml_file, only : xml_file
use foxy_xml_tag, only : xml_tag
use penf

implicit none
private
public :: I1P, I2P, I4P, I8P, R4P, R8P
public :: xml_file
public :: xml_tag
endmodule foxy
