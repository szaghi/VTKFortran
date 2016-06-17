!< Geo-structured write submodule interface for VTK file class.
  module function write_geo_strg_data1_rank2_R8P(self, nx1, nx2, ny1, ny2, nz1, nz2, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 1, rank 2, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self       !< VTK file.
  integer(I4P),    intent(in)    :: nx1        !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2        !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1        !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2        !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1        !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2        !< Final node of z axis.
  real(R8P),       intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,:].
  integer(I4P)                   :: error      !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data1_rank2_R8P

  module function write_geo_strg_data1_rank2_R4P(self, nx1, nx2, ny1, ny2, nz1, nz2, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 1, rank 2, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self       !< VTK file.
  integer(I4P),    intent(in)    :: nx1        !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2        !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1        !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2        !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1        !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2        !< Final node of z axis.
  real(R4P),       intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,:].
  integer(I4P)                   :: error      !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data1_rank2_R4P

  module function write_geo_strg_data1_rank4_R8P(self, nx1, nx2, ny1, ny2, nz1, nz2, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 1, rank 4, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self             !< VTK file.
  integer(I4P),    intent(in)    :: nx1              !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2              !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1              !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2              !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1              !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2              !< Final node of z axis.
  real(R8P),       intent(in)    :: xyz(1:,1:,1:,1:) !< X, y, z coordinates [1:3,:,:,:].
  integer(I4P)                   :: error            !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data1_rank4_R8P

  module function write_geo_strg_data1_rank4_R4P(self, nx1, nx2, ny1, ny2, nz1, nz2, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 1, rank 4, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self             !< VTK file.
  integer(I4P),    intent(in)    :: nx1              !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2              !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1              !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2              !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1              !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2              !< Final node of z axis.
  real(R4P),       intent(in)    :: xyz(1:,1:,1:,1:) !< X, y, z coordinates [1:3,:,:,:].
  integer(I4P)                   :: error            !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data1_rank4_R4P

  module function write_geo_strg_data3_rank1_R8P(self, nx1, nx2, ny1, ny2, nz1, nz2, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 3, rank 1, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  integer(I4P),    intent(in)    :: nx1      !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2      !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1      !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2      !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1      !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2      !< Final node of z axis.
  real(R8P),       intent(in)    :: x(1:)    !< X coordinates.
  real(R8P),       intent(in)    :: y(1:)    !< Y coordinates.
  real(R8P),       intent(in)    :: z(1:)    !< Z coordinates.
  integer(I4P)                   :: error    !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data3_rank1_R8P

  module function write_geo_strg_data3_rank1_R4P(self, nx1, nx2, ny1, ny2, nz1, nz2, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 3, rank 1, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  integer(I4P),    intent(in)    :: nx1      !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2      !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1      !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2      !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1      !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2      !< Final node of z axis.
  real(R4P),       intent(in)    :: x(1:)    !< X coordinates.
  real(R4P),       intent(in)    :: y(1:)    !< Y coordinates.
  real(R4P),       intent(in)    :: z(1:)    !< Z coordinates.
  integer(I4P)                   :: error    !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data3_rank1_R4P

  module function write_geo_strg_data3_rank3_R8P(self, nx1, nx2, ny1, ny2, nz1, nz2, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 3, rank 3, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  integer(I4P),    intent(in)    :: nx1         !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2         !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1         !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2         !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1         !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2         !< Final node of z axis.
  real(R8P),       intent(in)    :: x(1:,1:,1:) !< X coordinates.
  real(R8P),       intent(in)    :: y(1:,1:,1:) !< Y coordinates.
  real(R8P),       intent(in)    :: z(1:,1:,1:) !< Z coordinates.
  integer(I4P)                   :: error       !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data3_rank3_R8P

  module function write_geo_strg_data3_rank3_R4P(self, nx1, nx2, ny1, ny2, nz1, nz2, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 3, rank 3, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  integer(I4P),    intent(in)    :: nx1         !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2         !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1         !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2         !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1         !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2         !< Final node of z axis.
  real(R4P),       intent(in)    :: x(1:,1:,1:) !< X coordinates.
  real(R4P),       intent(in)    :: y(1:,1:,1:) !< Y coordinates.
  real(R4P),       intent(in)    :: z(1:,1:,1:) !< Z coordinates.
  integer(I4P)                   :: error       !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data3_rank3_R4P
