!< Pure Fortran (2003+) library to write and read data conforming the VTK standard
module Lib_VTK_IO
!-----------------------------------------------------------------------------------------------------------------------------------
!< Pure Fortran (2003+) library to write and read data conforming the VTK standard
!<{!README-Lib_VTK_IO.md!}
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision           ! Integers and reals precision definition.
USE Lib_Base64             ! Base64 encoding/decoding procedures.
USE Lib_VTK_IO_Back_End    ! Lib_VTK_IO back end module.
USE Lib_VTK_IO_INI_XML     ! INI_XML interface module.
USE Lib_VTK_IO_FLD_XML     ! FLD_XML interface module.
USE Lib_VTK_IO_GEO_XML     ! GEO_XML interface module.
USE Lib_VTK_IO_CON_XML     ! CON_XML interface module.
USE Lib_VTK_IO_DAT_VAR_XML ! DAT_XML and VAR_XML interface module.
USE Lib_VTK_IO_END_XML     ! END_XML interface module.
USE Lib_VTK_IO_VTM_XML     ! VTM_XML interface module.
USE Lib_VTK_IO_PVTK_XML    ! PVTK_XML interface module.
USE Lib_VTK_IO_PVD_XML     ! PVD_XML interface module.
USE Lib_VTK_IO_INI         ! INI interface module.
USE Lib_VTK_IO_GEO         ! GEO interface module.
USE Lib_VTK_IO_CON         ! CON interface module.
USE Lib_VTK_IO_DAT_VAR     ! DAT and VAR interface module.
USE Lib_VTK_IO_END         ! END interface module.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save

public:: VTK_INI_XML_WRITE, VTK_INI_XML_READ
public:: VTK_FLD_XML
public:: VTK_GEO_XML_WRITE, VTK_GEO_XML_READ
public:: VTK_CON_XML
public:: VTK_DAT_XML
public:: VTK_VAR_XML
public:: VTK_END_XML

public:: VTM_INI_XML
public:: VTM_BLK_XML
public:: VTM_WRF_XML
public:: VTM_END_XML

public:: PVTK_INI_XML
public:: PVTK_GEO_XML
public:: PVTK_DAT_XML
public:: PVTK_VAR_XML
public:: PVTK_END_XML

public:: PVD_INI_XML
public:: PVD_DAT_XML
public:: PVD_END_XML

public:: VTK_INI
public:: VTK_GEO
public:: VTK_CON
public:: VTK_DAT
public:: VTK_VAR
public:: VTK_END
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule Lib_VTK_IO
