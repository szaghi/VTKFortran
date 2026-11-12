!< Minimal HDF5 bindings (C API) used by the VTKHDF format.
module vtk_fortran_hdf5
!< Minimal HDF5 bindings (C API) used by the VTKHDF format: files, groups, links, attributes, datasets (fixed or
!< appendable, optionally deflated, or written by blocks), and their reading back.
!<
!< The HDF5 C API is bound with `iso_c_binding`, not through the HDF5 Fortran module: no `hdf5.mod` (specific to the
!< compiler and to the HDF5 build) is needed, only the C library (`-lhdf5`, plus the MPI libraries when HDF5 is a
!< parallel build). The predefined types are the C globals behind the HDF5 macros (`H5T_NATIVE_DOUBLE` is
!< `H5T_NATIVE_DOUBLE_g`), valid after `H5open`. Checked with HDF5 1.10.11 and 2.3.0.
!<
!< The module is always compiled: without `VTKFORTRAN_USE_HDF5` the procedures are stubs returning an error, and
!< `is_hdf5_enabled` is `.false.`.
!<
!< Datasets are written by the procedure of their kind (`h5_dataset_write_R8P`, ...), read through the generic
!< `h5_dataset_read`. Dimensions are given in the Fortran order (the first one varies fastest) and reversed for HDF5
!< (C order): the array
!< `x(nc, ni, nj, nk)` is the HDF5 dataset `[nk, nj, ni, nc]`. An appendable dataset grows along its last Fortran
!< dimension (the first HDF5 one). A dataset written by blocks (`h5_dataset_slab_<kind>`: in a parallel file, each rank
!< its block of a grid) is created with its whole shape, then each block written where it goes; with `append` (a time
!< series of a grid) it grows along its last dimension, each block written in the new rows.
!< An appendable or deflated dataset is chunked by whole rows (its last Fortran dimension): as many as fit in the optional
!< `chunk_bytes` of `h5_dataset_write_<kind>` (default `DEFAULT_CHUNK_BYTES`, 1 MiB), at least one, at most the rows
!< written when it is created; `h5_dataset_chunk` reads the chunk shape back.
use, intrinsic :: iso_c_binding
use penf, only : I1P, I2P, I4P, I8P, R4P, R8P
#ifdef VTKFORTRAN_USE_MPI
use mpi_f08, only : MPI_Comm, MPI_Allgather, MPI_Allreduce, MPI_Comm_size, MPI_Comm_rank, MPI_INTEGER8, MPI_SUM, MPI_MAX
#endif
implicit none
private

public :: is_hdf5_enabled
public :: hid_kind
public :: h5_initialize
public :: h5_file_create
public :: h5_mpi_sum
public :: is_mpi_enabled
public :: h5_file_open
public :: h5_file_flush
public :: h5_file_close
public :: h5_group_create
public :: h5_group_open
public :: h5_group_close
public :: h5_link_exists
public :: h5_link_soft
public :: h5_link_target
public :: h5_object_copy
public :: h5_group_names
public :: h5_attribute_write
public :: h5_attribute_read
public :: h5_attribute_exists
! the datasets are written and appended by the procedure of their kind, not through a generic: the values are an
! assumed-size array x(*), of any rank by sequence association, which a generic cannot resolve. An assumed-rank x(..)
! would allow a generic, but nvfortran 26.9 gives a wrong c_loc of an assumed-rank dummy of rank > 1 (and has no
! SELECT RANK): see issue #72
public :: h5_dataset_write_R8P, h5_dataset_write_R4P, h5_dataset_write_I8P, h5_dataset_write_I4P, h5_dataset_write_I2P
public :: h5_dataset_write_I1P
public :: h5_dataset_append_R8P, h5_dataset_append_R4P, h5_dataset_append_I8P, h5_dataset_append_I4P
public :: h5_dataset_append_I2P, h5_dataset_append_I1P
public :: h5_dataset_slab_R8P, h5_dataset_slab_R4P, h5_dataset_slab_I8P, h5_dataset_slab_I4P, h5_dataset_slab_I2P
public :: h5_dataset_slab_I1P
public :: h5_mpi_allgather
public :: h5_mpi_rank
public :: h5_mpi_max
public :: h5_name_invalid
public :: h5_dataset_open
public :: h5_dataset_close
public :: h5_dataset_dims
public :: h5_dataset_chunk
public :: h5_dataset_resize
public :: h5_dataset_info
public :: h5_dataset_read_bytes
public :: h5_dataset_read
public :: h5_deflate_available
public :: h5_parallel_filters_available
public :: DEFAULT_CHUNK_BYTES

integer, parameter :: hid_kind = c_int64_t !< Kind of the HDF5 identifiers (`hid_t`, 64-bit since HDF5 1.10).

#ifdef VTKFORTRAN_USE_HDF5
logical, parameter :: is_hdf5_enabled = .true.  !< The library is built with HDF5 (VTKFORTRAN_USE_HDF5).
#ifdef VTKFORTRAN_USE_MPI
logical, parameter :: is_mpi_enabled = .true.   !< The library is built with MPI (VTKFORTRAN_USE_MPI): parallel HDF5 files.
#else
logical, parameter :: is_mpi_enabled = .false.  !< The library is built with MPI (VTKFORTRAN_USE_MPI): parallel HDF5 files.
#endif
#else
logical, parameter :: is_hdf5_enabled = .false. !< The library is built with HDF5 (VTKFORTRAN_USE_HDF5).
logical, parameter :: is_mpi_enabled = .false.  !< The library is built with MPI (VTKFORTRAN_USE_MPI): parallel HDF5 files.
#endif

integer(c_int64_t), parameter :: H5P_DEFAULT = 0_c_int64_t           !< Default property list.
integer(c_int64_t), parameter :: H5S_ALL = 0_c_int64_t               !< The whole dataspace.
integer(c_int64_t), parameter :: H5E_DEFAULT = 0_c_int64_t           !< Default error stack.
integer(c_int),     parameter :: H5F_ACC_RDONLY = 0_c_int            !< Open read-only.
integer(c_int),     parameter :: H5F_ACC_RDWR = 1_c_int              !< Open for reading and writing.
integer(c_int),     parameter :: H5F_ACC_TRUNC = 2_c_int             !< Create, overwriting an existing file.
integer(c_int),     parameter :: H5F_SCOPE_LOCAL = 0_c_int           !< Flush scope: this file.
integer(c_int),     parameter :: H5S_SCALAR = 0_c_int                !< Scalar dataspace class.
integer(c_int),     parameter :: H5S_SELECT_SET = 0_c_int            !< Hyperslab selection: replace.
integer(c_int),     parameter :: H5T_STR_NULLPAD = 1_c_int           !< Fixed-length string padded with NULs.
integer(c_int),     parameter :: H5Z_FILTER_DEFLATE = 1_c_int        !< The deflate (zlib) filter.
integer(c_int),     parameter :: H5P_CRT_ORDER_TRACKED = 1_c_int     !< Track the creation order of links.
integer(c_int),     parameter :: H5P_CRT_ORDER_INDEXED = 2_c_int     !< Index the creation order of links.
integer(c_long_long), parameter :: H5S_UNLIMITED = -1_c_long_long    !< Unlimited dimension ((hsize_t)-1).
integer(I8P),       parameter :: DEFAULT_CHUNK_BYTES = 1048576_I8P   !< Bytes of a chunk at most (but one row), default.

#ifdef VTKFORTRAN_USE_HDF5
! the predefined types and property list classes: C globals, defined by HDF5 and valid after H5open. They must be PUBLIC:
! gfortran gives PRIVATE module variables hidden visibility, bind(C) or not, so a private one is a variable of its own,
! never bound to the HDF5 one (always 0). PROTECTED: read-only outside this module.
integer(c_int64_t), bind(C, name='H5T_NATIVE_DOUBLE_g'), public, protected          :: H5T_NATIVE_DOUBLE_g
integer(c_int64_t), bind(C, name='H5T_NATIVE_FLOAT_g'), public, protected           :: H5T_NATIVE_FLOAT_g
integer(c_int64_t), bind(C, name='H5T_NATIVE_INT8_g'), public, protected            :: H5T_NATIVE_INT8_g
integer(c_int64_t), bind(C, name='H5T_NATIVE_INT16_g'), public, protected           :: H5T_NATIVE_INT16_g
integer(c_int64_t), bind(C, name='H5T_NATIVE_INT32_g'), public, protected           :: H5T_NATIVE_INT32_g
integer(c_int64_t), bind(C, name='H5T_NATIVE_INT64_g'), public, protected           :: H5T_NATIVE_INT64_g
integer(c_int64_t), bind(C, name='H5T_NATIVE_UINT8_g'), public, protected           :: H5T_NATIVE_UINT8_g
integer(c_int64_t), bind(C, name='H5T_NATIVE_UINT16_g'), public, protected          :: H5T_NATIVE_UINT16_g
integer(c_int64_t), bind(C, name='H5T_NATIVE_UINT32_g'), public, protected          :: H5T_NATIVE_UINT32_g
integer(c_int64_t), bind(C, name='H5T_NATIVE_UINT64_g'), public, protected          :: H5T_NATIVE_UINT64_g
integer(c_int64_t), bind(C, name='H5T_C_S1_g'), public, protected                   :: H5T_C_S1_g
integer(c_int64_t), bind(C, name='H5P_CLS_DATASET_CREATE_ID_g'), public, protected  :: H5P_CLS_DATASET_CREATE_ID_g
integer(c_int64_t), bind(C, name='H5P_CLS_GROUP_CREATE_ID_g'), public, protected    :: H5P_CLS_GROUP_CREATE_ID_g

interface
  integer(c_int) function H5open() bind(C, name='H5open')
    import :: c_int
  end function H5open
  integer(c_int) function H5Eset_auto2(estack, func, client_data) bind(C, name='H5Eset_auto2')
    import :: c_int, c_int64_t, c_funptr, c_ptr
    integer(c_int64_t), value :: estack
    type(c_funptr),     value :: func
    type(c_ptr),        value :: client_data
  end function H5Eset_auto2
  integer(c_int64_t) function vtkfortran_h5_fapl_mpio(fcomm) bind(C, name='vtkfortran_h5_fapl_mpio')
    import :: c_int, c_int64_t
    integer(c_int), value :: fcomm
  end function vtkfortran_h5_fapl_mpio
  integer(c_int64_t) function vtkfortran_h5_dxpl_mpio_collective() bind(C, name='vtkfortran_h5_dxpl_mpio_collective')
    import :: c_int64_t
  end function vtkfortran_h5_dxpl_mpio_collective
  integer(c_int) function H5Tconvert(src, dst, nelmts, buf, background, plist) bind(C, name='H5Tconvert')
    import :: c_int, c_int64_t, c_size_t, c_ptr
    integer(c_int64_t), value :: src, dst
    integer(c_size_t),  value :: nelmts
    type(c_ptr),        value :: buf, background
    integer(c_int64_t), value :: plist
  end function H5Tconvert
  integer(c_int) function H5Tequal(type1, type2) bind(C, name='H5Tequal')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: type1, type2
  end function H5Tequal
  integer(c_int) function H5Sselect_none(space) bind(C, name='H5Sselect_none')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: space
  end function H5Sselect_none
  integer(c_int) function H5get_libversion(majnum, minnum, relnum) bind(C, name='H5get_libversion')
    import :: c_int
    integer(c_int), intent(out) :: majnum, minnum, relnum
  end function H5get_libversion
  integer(c_int64_t) function H5Fcreate(name, flags, fcpl, fapl) bind(C, name='H5Fcreate')
    import :: c_int64_t, c_char, c_int
    character(kind=c_char), intent(in) :: name(*)
    integer(c_int),     value          :: flags
    integer(c_int64_t), value          :: fcpl, fapl
  end function H5Fcreate
  integer(c_int64_t) function H5Fopen(name, flags, fapl) bind(C, name='H5Fopen')
    import :: c_int64_t, c_char, c_int
    character(kind=c_char), intent(in) :: name(*)
    integer(c_int),     value          :: flags
    integer(c_int64_t), value          :: fapl
  end function H5Fopen
  integer(c_int) function H5Fflush(id, scope) bind(C, name='H5Fflush')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: id
    integer(c_int),     value :: scope
  end function H5Fflush
  integer(c_int) function H5Fclose(id) bind(C, name='H5Fclose')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: id
  end function H5Fclose
  integer(c_int64_t) function H5Gcreate2(loc, name, lcpl, gcpl, gapl) bind(C, name='H5Gcreate2')
    import :: c_int64_t, c_char
    integer(c_int64_t), value          :: loc
    character(kind=c_char), intent(in) :: name(*)
    integer(c_int64_t), value          :: lcpl, gcpl, gapl
  end function H5Gcreate2
  integer(c_int64_t) function H5Gopen2(loc, name, gapl) bind(C, name='H5Gopen2')
    import :: c_int64_t, c_char
    integer(c_int64_t), value          :: loc
    character(kind=c_char), intent(in) :: name(*)
    integer(c_int64_t), value          :: gapl
  end function H5Gopen2
  integer(c_int) function H5Gclose(id) bind(C, name='H5Gclose')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: id
  end function H5Gclose
  integer(c_int) function H5Lexists(loc, name, lapl) bind(C, name='H5Lexists')
    import :: c_int, c_int64_t, c_char
    integer(c_int64_t), value          :: loc
    character(kind=c_char), intent(in) :: name(*)
    integer(c_int64_t), value          :: lapl
  end function H5Lexists
  integer(c_int) function H5Lcreate_soft(target, loc, name, lcpl, lapl) bind(C, name='H5Lcreate_soft')
    import :: c_int, c_int64_t, c_char
    character(kind=c_char), intent(in) :: target(*)
    integer(c_int64_t), value          :: loc
    character(kind=c_char), intent(in) :: name(*)
    integer(c_int64_t), value          :: lcpl, lapl
  end function H5Lcreate_soft
  integer(c_int) function H5Lget_val(loc, name, buf, size, lapl) bind(C, name='H5Lget_val')
    import :: c_int, c_int64_t, c_char, c_size_t
    integer(c_int64_t), value              :: loc
    character(kind=c_char), intent(in)     :: name(*)
    character(kind=c_char), intent(out)    :: buf(*)
    integer(c_size_t), value               :: size
    integer(c_int64_t), value              :: lapl
  end function H5Lget_val
  integer(c_int64_t) function H5Screate(class) bind(C, name='H5Screate')
    import :: c_int64_t, c_int
    integer(c_int), value :: class
  end function H5Screate
  integer(c_int64_t) function H5Screate_simple(rank, dims, maxdims) bind(C, name='H5Screate_simple')
    import :: c_int64_t, c_int, c_long_long, c_ptr
    integer(c_int),       value      :: rank
    integer(c_long_long), intent(in) :: dims(*)
    type(c_ptr),          value      :: maxdims
  end function H5Screate_simple
  integer(c_int) function H5Sclose(id) bind(C, name='H5Sclose')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: id
  end function H5Sclose
  integer(c_int) function H5Sselect_hyperslab(space, op, start, stride, count, block) bind(C, name='H5Sselect_hyperslab')
    import :: c_int, c_int64_t, c_long_long, c_ptr
    integer(c_int64_t),   value      :: space
    integer(c_int),       value      :: op
    integer(c_long_long), intent(in) :: start(*)
    type(c_ptr),          value      :: stride
    integer(c_long_long), intent(in) :: count(*)
    type(c_ptr),          value      :: block
  end function H5Sselect_hyperslab
  integer(c_int) function H5Sget_simple_extent_ndims(space) bind(C, name='H5Sget_simple_extent_ndims')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: space
  end function H5Sget_simple_extent_ndims
  integer(c_int) function H5Sget_simple_extent_dims(space, dims, maxdims) bind(C, name='H5Sget_simple_extent_dims')
    import :: c_int, c_int64_t, c_long_long, c_ptr
    integer(c_int64_t),   value       :: space
    integer(c_long_long), intent(out) :: dims(*)
    type(c_ptr),          value       :: maxdims
  end function H5Sget_simple_extent_dims
  integer(c_int64_t) function H5Dcreate2(loc, name, dtype, space, lcpl, dcpl, dapl) bind(C, name='H5Dcreate2')
    import :: c_int64_t, c_char
    integer(c_int64_t), value          :: loc
    character(kind=c_char), intent(in) :: name(*)
    integer(c_int64_t), value          :: dtype, space, lcpl, dcpl, dapl
  end function H5Dcreate2
  integer(c_int64_t) function H5Dopen2(loc, name, dapl) bind(C, name='H5Dopen2')
    import :: c_int64_t, c_char
    integer(c_int64_t), value          :: loc
    character(kind=c_char), intent(in) :: name(*)
    integer(c_int64_t), value          :: dapl
  end function H5Dopen2
  integer(c_int) function H5Dwrite(dset, memtype, memspace, filespace, dxpl, buf) bind(C, name='H5Dwrite')
    import :: c_int, c_int64_t, c_ptr
    integer(c_int64_t), value :: dset, memtype, memspace, filespace, dxpl
    type(c_ptr),        value :: buf
  end function H5Dwrite
  integer(c_int) function H5Dread(dset, memtype, memspace, filespace, dxpl, buf) bind(C, name='H5Dread')
    import :: c_int, c_int64_t, c_ptr
    integer(c_int64_t), value :: dset, memtype, memspace, filespace, dxpl
    type(c_ptr),        value :: buf
  end function H5Dread
  integer(c_int) function H5Dset_extent(dset, size) bind(C, name='H5Dset_extent')
    import :: c_int, c_int64_t, c_long_long
    integer(c_int64_t),   value      :: dset
    integer(c_long_long), intent(in) :: size(*)
  end function H5Dset_extent
  integer(c_int64_t) function H5Dget_space(dset) bind(C, name='H5Dget_space')
    import :: c_int64_t
    integer(c_int64_t), value :: dset
  end function H5Dget_space
  integer(c_int) function H5Dclose(id) bind(C, name='H5Dclose')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: id
  end function H5Dclose
  integer(c_int64_t) function H5Acreate2(loc, name, dtype, space, acpl, aapl) bind(C, name='H5Acreate2')
    import :: c_int64_t, c_char
    integer(c_int64_t), value          :: loc
    character(kind=c_char), intent(in) :: name(*)
    integer(c_int64_t), value          :: dtype, space, acpl, aapl
  end function H5Acreate2
  integer(c_int64_t) function H5Aopen(loc, name, aapl) bind(C, name='H5Aopen')
    import :: c_int64_t, c_char
    integer(c_int64_t), value          :: loc
    character(kind=c_char), intent(in) :: name(*)
    integer(c_int64_t), value          :: aapl
  end function H5Aopen
  integer(c_int) function H5Ocopy(src_loc, src_name, dst_loc, dst_name, ocpypl, lcpl) bind(C, name='H5Ocopy')
    import :: c_int, c_int64_t, c_char
    integer(c_int64_t), value          :: src_loc
    character(kind=c_char), intent(in) :: src_name(*)
    integer(c_int64_t), value          :: dst_loc
    character(kind=c_char), intent(in) :: dst_name(*)
    integer(c_int64_t), value          :: ocpypl, lcpl
  end function H5Ocopy
  integer(c_int) function H5Adelete(loc, name) bind(C, name='H5Adelete')
    import :: c_int, c_int64_t, c_char
    integer(c_int64_t), value          :: loc
    character(kind=c_char), intent(in) :: name(*)
  end function H5Adelete
  integer(c_int64_t) function H5Lget_name_by_idx(loc, group, idx_type, order, n, name, size, lapl) &
      bind(C, name='H5Lget_name_by_idx')
    import :: c_int, c_int64_t, c_char, c_long_long, c_size_t, c_ptr
    integer(c_int64_t),   value          :: loc
    character(kind=c_char), intent(in)   :: group(*)
    integer(c_int),       value          :: idx_type, order
    integer(c_long_long), value          :: n
    type(c_ptr),          value          :: name
    integer(c_size_t),    value          :: size
    integer(c_int64_t),   value          :: lapl
  end function H5Lget_name_by_idx
  integer(c_int) function H5Aexists(loc, name) bind(C, name='H5Aexists')
    import :: c_int, c_int64_t, c_char
    integer(c_int64_t), value          :: loc
    character(kind=c_char), intent(in) :: name(*)
  end function H5Aexists
  integer(c_int) function H5Awrite(attr, memtype, buf) bind(C, name='H5Awrite')
    import :: c_int, c_int64_t, c_ptr
    integer(c_int64_t), value :: attr, memtype
    type(c_ptr),        value :: buf
  end function H5Awrite
  integer(c_int) function H5Aread(attr, memtype, buf) bind(C, name='H5Aread')
    import :: c_int, c_int64_t, c_ptr
    integer(c_int64_t), value :: attr, memtype
    type(c_ptr),        value :: buf
  end function H5Aread
  integer(c_int64_t) function H5Aget_type(attr) bind(C, name='H5Aget_type')
    import :: c_int64_t
    integer(c_int64_t), value :: attr
  end function H5Aget_type
  integer(c_int64_t) function H5Aget_space(attr) bind(C, name='H5Aget_space')
    import :: c_int64_t
    integer(c_int64_t), value :: attr
  end function H5Aget_space
  integer(c_int) function H5Aclose(id) bind(C, name='H5Aclose')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: id
  end function H5Aclose
  integer(c_int64_t) function H5Tcopy(dtype) bind(C, name='H5Tcopy')
    import :: c_int64_t
    integer(c_int64_t), value :: dtype
  end function H5Tcopy
  integer(c_int) function H5Tset_size(dtype, size) bind(C, name='H5Tset_size')
    import :: c_int, c_int64_t, c_size_t
    integer(c_int64_t), value :: dtype
    integer(c_size_t),  value :: size
  end function H5Tset_size
  integer(c_size_t) function H5Tget_size(dtype) bind(C, name='H5Tget_size')
    import :: c_size_t, c_int64_t
    integer(c_int64_t), value :: dtype
  end function H5Tget_size
  integer(c_int) function H5Tset_strpad(dtype, strpad) bind(C, name='H5Tset_strpad')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: dtype
    integer(c_int),     value :: strpad
  end function H5Tset_strpad
  integer(c_int64_t) function H5Dget_type(dset) bind(C, name='H5Dget_type')
    import :: c_int64_t
    integer(c_int64_t), value :: dset
  end function H5Dget_type
  integer(c_int) function H5Tget_class(dtype) bind(C, name='H5Tget_class')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: dtype
  end function H5Tget_class
  integer(c_int) function H5Tget_sign(dtype) bind(C, name='H5Tget_sign')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: dtype
  end function H5Tget_sign
  integer(c_int64_t) function H5Tget_native_type(dtype, direction) bind(C, name='H5Tget_native_type')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: dtype
    integer(c_int),     value :: direction
  end function H5Tget_native_type
  integer(c_int) function H5Tclose(id) bind(C, name='H5Tclose')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: id
  end function H5Tclose
  integer(c_int) function H5Tis_variable_str(dtype) bind(C, name='H5Tis_variable_str')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: dtype
  end function H5Tis_variable_str
  integer(c_int) function H5free_memory(mem) bind(C, name='H5free_memory')
    import :: c_int, c_ptr
    type(c_ptr), value :: mem
  end function H5free_memory
  integer(c_size_t) function c_strlen(s) bind(C, name='strlen')
    import :: c_size_t, c_ptr
    type(c_ptr), value :: s
  end function c_strlen
  integer(c_int64_t) function H5Pcreate(cls) bind(C, name='H5Pcreate')
    import :: c_int64_t
    integer(c_int64_t), value :: cls
  end function H5Pcreate
  integer(c_int) function H5Pset_chunk(plist, ndims, dims) bind(C, name='H5Pset_chunk')
    import :: c_int, c_int64_t, c_long_long
    integer(c_int64_t),   value      :: plist
    integer(c_int),       value      :: ndims
    integer(c_long_long), intent(in) :: dims(*)
  end function H5Pset_chunk
  integer(c_int) function H5Pget_chunk(plist, max_ndims, dims) bind(C, name='H5Pget_chunk')
    import :: c_int, c_int64_t, c_long_long
    integer(c_int64_t),   value       :: plist
    integer(c_int),       value       :: max_ndims
    integer(c_long_long), intent(out) :: dims(*)
  end function H5Pget_chunk
  integer(c_int) function H5Pget_layout(plist) bind(C, name='H5Pget_layout')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: plist
  end function H5Pget_layout
  integer(c_int64_t) function H5Dget_create_plist(dset) bind(C, name='H5Dget_create_plist')
    import :: c_int64_t
    integer(c_int64_t), value :: dset
  end function H5Dget_create_plist
  integer(c_int) function H5Pset_deflate(plist, level) bind(C, name='H5Pset_deflate')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: plist
    integer(c_int),     value :: level
  end function H5Pset_deflate
  integer(c_int) function H5Pset_link_creation_order(plist, flags) bind(C, name='H5Pset_link_creation_order')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: plist
    integer(c_int),     value :: flags
  end function H5Pset_link_creation_order
  integer(c_int) function H5Pclose(id) bind(C, name='H5Pclose')
    import :: c_int, c_int64_t
    integer(c_int64_t), value :: id
  end function H5Pclose
  integer(c_int) function H5Zfilter_avail(filter) bind(C, name='H5Zfilter_avail')
    import :: c_int
    integer(c_int), value :: filter
  end function H5Zfilter_avail
endinterface
#endif

interface h5_attribute_write
  !< Write an attribute: a string, or a rank-1 array of `I4P`, `I8P` or `R8P` values.
  module procedure h5_attribute_write_string, h5_attribute_write_I4P, h5_attribute_write_I8P, h5_attribute_write_R8P
endinterface h5_attribute_write

interface h5_attribute_read
  !< Read an attribute: a string, or a rank-1 array of `I8P` or `R8P` values (converted from the stored type).
  module procedure h5_attribute_read_string, h5_attribute_read_I8P, h5_attribute_read_R8P
endinterface h5_attribute_read

interface h5_dataset_read
  !< Read the values of a dataset, converted to the kind of the output.
  module procedure h5_dataset_read_R8P, h5_dataset_read_R4P, h5_dataset_read_I8P, h5_dataset_read_I4P, &
                   h5_dataset_read_I2P, h5_dataset_read_I1P
endinterface h5_dataset_read

contains
  function h5_initialize() result(error)
  !< Initialize HDF5 (the predefined types) and silence its automatic error printing: the procedures return errors.
  integer(I4P) :: error !< Error status.

  error = 1_I4P
#ifdef VTKFORTRAN_USE_HDF5
  if (H5open() < 0) return
  if (H5Eset_auto2(H5E_DEFAULT, c_null_funptr, c_null_ptr) < 0) return
  error = 0_I4P
#endif
  endfunction h5_initialize

  function h5_file_create(filename, file_id, comm) result(error)
  !< Create a file (an existing one is overwritten); with `comm` (a Fortran MPI communicator) a parallel file, created by
  !< every rank of it (the library built with MPI, `VTKFORTRAN_USE_MPI`, and a parallel HDF5).
  character(*),          intent(in)           :: filename !< File name.
  integer(hid_kind),     intent(out)          :: file_id  !< File identifier.
  integer(I4P),          intent(in), optional :: comm     !< MPI communicator (Fortran handle, `MPI_Comm%MPI_VAL`).
  integer(I4P)                                :: error    !< Error status.
#ifdef VTKFORTRAN_USE_HDF5
  integer(c_int64_t)                          :: fapl     !< File access properties.
#endif

  file_id = -1_hid_kind
  error = h5_initialize()
  if (error /= 0) return
#ifdef VTKFORTRAN_USE_HDF5
  fapl = H5P_DEFAULT
  if (present(comm)) then
    error = 1_I4P
#ifdef VTKFORTRAN_USE_MPI
    fapl = vtkfortran_h5_fapl_mpio(int(comm, c_int))
#endif
    if (fapl < 0 .or. .not.is_mpi_enabled) return
    error = 0_I4P
  endif
  file_id = H5Fcreate(trim(filename)//c_null_char, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)
  if (file_id < 0) error = 1_I4P
  if (present(comm)) then
    if (H5Pclose(fapl) < 0) error = 1_I4P
  endif
#else
  if (present(comm)) error = 1_I4P
#endif
  endfunction h5_file_create

  function h5_mpi_sum(comm, n) result(total)
  !< The sum of `n` over the ranks of `comm` (a Fortran MPI communicator); `n` without MPI.
  integer(I4P), intent(in) :: comm  !< MPI communicator (Fortran handle).
  integer(I8P), intent(in) :: n     !< Value of the rank.
  integer(I8P)             :: total !< Sum over the ranks.
#ifdef VTKFORTRAN_USE_MPI
  type(MPI_Comm)           :: c     !< Communicator.

  c%MPI_VAL = comm
  call MPI_Allreduce(n, total, 1, MPI_INTEGER8, MPI_SUM, c)
#else
  total = n + 0_I8P * comm
#endif
  endfunction h5_mpi_sum

  function h5_file_open(filename, file_id, readwrite, comm) result(error)
  !< Open an existing file, read-only or (`readwrite=.true.`) for reading and writing; with `comm` (a Fortran MPI
  !< communicator) as a parallel file, opened by every rank of it (as `h5_file_create`).
  character(*),          intent(in)           :: filename  !< File name.
  integer(hid_kind),     intent(out)          :: file_id   !< File identifier.
  logical,               intent(in), optional :: readwrite !< Open for writing too.
  integer(I4P),          intent(in), optional :: comm      !< MPI communicator (Fortran handle).
  integer(I4P)                                :: error     !< Error status.
  integer(c_int)                              :: flags     !< Access flags.
#ifdef VTKFORTRAN_USE_HDF5
  integer(c_int64_t)                          :: fapl      !< File access properties.
#endif

  file_id = -1_hid_kind
  error = h5_initialize()
  if (error /= 0) return
  flags = H5F_ACC_RDONLY
  if (present(readwrite)) then
    if (readwrite) flags = H5F_ACC_RDWR
  endif
#ifdef VTKFORTRAN_USE_HDF5
  fapl = H5P_DEFAULT
  if (present(comm)) then
    error = 1_I4P
#ifdef VTKFORTRAN_USE_MPI
    fapl = vtkfortran_h5_fapl_mpio(int(comm, c_int))
#endif
    if (fapl < 0 .or. .not.is_mpi_enabled) return
    error = 0_I4P
  endif
  file_id = H5Fopen(trim(filename)//c_null_char, flags, fapl)
  if (file_id < 0) error = 1_I4P
  if (present(comm)) then
    if (H5Pclose(fapl) < 0) error = 1_I4P
  endif
#else
  if (present(comm)) error = 1_I4P
#endif
  endfunction h5_file_open

  function h5_file_flush(file_id) result(error)
  !< Flush the file to disk.
  integer(hid_kind), intent(in) :: file_id !< File identifier.
  integer(I4P)                  :: error   !< Error status.

  error = 1_I4P
#ifdef VTKFORTRAN_USE_HDF5
  if (H5Fflush(file_id, H5F_SCOPE_LOCAL) >= 0) error = 0_I4P
#endif
  endfunction h5_file_flush

  function h5_file_close(file_id) result(error)
  !< Close a file.
  integer(hid_kind), intent(inout) :: file_id !< File identifier, -1 once closed.
  integer(I4P)                     :: error   !< Error status.

  error = 1_I4P
#ifdef VTKFORTRAN_USE_HDF5
  if (file_id >= 0) then
    if (H5Fclose(file_id) >= 0) error = 0_I4P
  endif
#endif
  file_id = -1_hid_kind
  endfunction h5_file_close

  function h5_group_create(loc_id, name, group_id, track_order) result(error)
  !< Create a group; with `track_order=.true.` the creation order of its links is tracked and indexed (VTKHDF assemblies).
  integer(hid_kind), intent(in)           :: loc_id      !< Parent (file or group).
  character(*),      intent(in)           :: name        !< Group name.
  integer(hid_kind), intent(out)          :: group_id    !< Group identifier.
  logical,           intent(in), optional :: track_order !< Track the creation order of the links.
  integer(I4P)                            :: error       !< Error status.
#ifdef VTKFORTRAN_USE_HDF5
  integer(c_int64_t)                      :: gcpl        !< Group creation properties.

  error = 1_I4P
  group_id = -1_hid_kind
  gcpl = H5P_DEFAULT
  if (present(track_order)) then
    if (track_order) then
      gcpl = H5Pcreate(H5P_CLS_GROUP_CREATE_ID_g)
      if (gcpl < 0) return
      if (H5Pset_link_creation_order(gcpl, ior(H5P_CRT_ORDER_TRACKED, H5P_CRT_ORDER_INDEXED)) < 0) return
    endif
  endif
  group_id = H5Gcreate2(loc_id, trim(name)//c_null_char, H5P_DEFAULT, gcpl, H5P_DEFAULT)
  if (group_id >= 0) error = 0_I4P
  if (gcpl /= H5P_DEFAULT) then
    if (H5Pclose(gcpl) < 0) error = 1_I4P
  endif
#else
  error = 1_I4P
  group_id = -1_hid_kind
#endif
  endfunction h5_group_create

  function h5_group_open(loc_id, name, group_id) result(error)
  !< Open a group.
  integer(hid_kind), intent(in)  :: loc_id   !< Parent (file or group).
  character(*),      intent(in)  :: name     !< Group name (or path).
  integer(hid_kind), intent(out) :: group_id !< Group identifier.
  integer(I4P)                   :: error    !< Error status.

  error = 1_I4P
  group_id = -1_hid_kind
#ifdef VTKFORTRAN_USE_HDF5
  group_id = H5Gopen2(loc_id, trim(name)//c_null_char, H5P_DEFAULT)
  if (group_id >= 0) error = 0_I4P
#endif
  endfunction h5_group_open

  function h5_group_close(group_id) result(error)
  !< Close a group.
  integer(hid_kind), intent(inout) :: group_id !< Group identifier, -1 once closed.
  integer(I4P)                     :: error    !< Error status.

  error = 1_I4P
#ifdef VTKFORTRAN_USE_HDF5
  if (group_id >= 0) then
    if (H5Gclose(group_id) >= 0) error = 0_I4P
  endif
#endif
  group_id = -1_hid_kind
  endfunction h5_group_close

  function h5_link_exists(loc_id, name) result(exists)
  !< Return true if the link (group or dataset) `name` exists in `loc_id`.
  integer(hid_kind), intent(in) :: loc_id !< Parent (file or group).
  character(*),      intent(in) :: name   !< Link name (one level).
  logical                       :: exists !< True if the link exists.

  exists = .false.
#ifdef VTKFORTRAN_USE_HDF5
  exists = H5Lexists(loc_id, trim(name)//c_null_char, H5P_DEFAULT) > 0
#endif
  endfunction h5_link_exists

  function h5_link_soft(target, loc_id, name) result(error)
  !< Create the soft (symbolic) link `name` in `loc_id`, pointing to the path `target`.
  character(*),      intent(in) :: target !< Path of the linked object.
  integer(hid_kind), intent(in) :: loc_id !< Group of the link.
  character(*),      intent(in) :: name   !< Link name.
  integer(I4P)                  :: error  !< Error status.

  error = 1_I4P
#ifdef VTKFORTRAN_USE_HDF5
  if (H5Lcreate_soft(trim(target)//c_null_char, loc_id, trim(name)//c_null_char, H5P_DEFAULT, H5P_DEFAULT) >= 0) &
    error = 0_I4P
#endif
  endfunction h5_link_soft

  function h5_link_target(loc_id, name, target) result(error)
  !< Return the path a soft (symbolic) link `name` of `loc_id` points to; an error (`target` empty) for a hard link (a
  !< group or dataset) or a missing one.
  integer(hid_kind),             intent(in)  :: loc_id !< Group of the link.
  character(*),                  intent(in)  :: name   !< Link name.
  character(len=:), allocatable, intent(out) :: target !< Path of the linked object.
  integer(I4P)                               :: error  !< Error status.
#ifdef VTKFORTRAN_USE_HDF5
  integer(c_size_t), parameter               :: N = 4096_c_size_t !< Longest path read.
  character(kind=c_char)                     :: buf(N) !< The path, NUL terminated.
  integer                                    :: c      !< Counter.
#endif

  target = ''
  error = 1_I4P
#ifdef VTKFORTRAN_USE_HDF5
  if (.not.h5_link_exists(loc_id, name)) return
  buf = c_null_char
  if (H5Lget_val(loc_id, trim(name)//c_null_char, buf, N, H5P_DEFAULT) < 0) return ! a hard link (the stack silenced)
  do c=1, int(N)
    if (buf(c) == c_null_char) exit
    target = target//buf(c)
  enddo
  if (len(target) > 0) error = 0_I4P
#endif
  endfunction h5_link_target

  function h5_object_copy(src_loc, src_name, dst_loc, dst_name) result(error)
  !< Copy the object (group or dataset, with all it holds) `src_name` of `src_loc` to `dst_name` of `dst_loc`, in the same
  !< or another file.
  integer(hid_kind), intent(in) :: src_loc  !< Source file or group.
  character(*),      intent(in) :: src_name !< Source object.
  integer(hid_kind), intent(in) :: dst_loc  !< Destination file or group.
  character(*),      intent(in) :: dst_name !< Destination name.
  integer(I4P)                  :: error    !< Error status.

  error = 1_I4P
#ifdef VTKFORTRAN_USE_HDF5
  if (H5Ocopy(src_loc, trim(src_name)//c_null_char, dst_loc, trim(dst_name)//c_null_char, H5P_DEFAULT, H5P_DEFAULT) >= 0) &
    error = 0_I4P
#endif
  endfunction h5_object_copy

  function h5_group_names(loc_id, group, names) result(error)
  !< Return the names of the members (links) of the group `group` of `loc_id`, in their creation order when the group
  !< tracks it (the groups VTKFortran and VTK write), otherwise in the order of their names; each one followed by a NUL
  !< character (`achar(0)`); empty if the group has none.
  integer(hid_kind),             intent(in)  :: loc_id !< Parent (file or group).
  character(*),                  intent(in)  :: group  !< Group name (or path).
  character(len=:), allocatable, intent(out) :: names  !< Names, each followed by NUL.
#ifdef VTKFORTRAN_USE_HDF5
  integer(c_int64_t)                         :: n      !< Length of a name (ssize_t).
  integer(c_long_long)                       :: m      !< Member index.
  integer(c_int)                             :: by     !< Index: creation order (1), or names (0).
  character(kind=c_char), allocatable, target :: chars(:) !< A name, as C characters.
#endif
  integer(I4P)                               :: error  !< Error status.

  names = ''
  error = 1_I4P
#ifdef VTKFORTRAN_USE_HDF5
  if (.not.h5_link_exists(loc_id, group)) return
  error = 0_I4P
  ! the creation order if the group tracks it (the call fails otherwise, silently: the error stack is not printed)
  by = 1_c_int
  if (H5Lget_name_by_idx(loc_id, trim(group)//c_null_char, by, 0_c_int, 0_c_long_long, c_null_ptr, 0_c_size_t, &
                         H5P_DEFAULT) < 0) by = 0_c_int
  m = 0_c_long_long
  do
    n = H5Lget_name_by_idx(loc_id, trim(group)//c_null_char, by, 0_c_int, m, c_null_ptr, 0_c_size_t, H5P_DEFAULT)
    if (n < 0) exit ! past the last member
    allocate(chars(n + 1))
    n = H5Lget_name_by_idx(loc_id, trim(group)//c_null_char, by, 0_c_int, m, c_loc(chars), &
                           int(n + 1, c_size_t), H5P_DEFAULT)
    if (n < 0) then
      error = 1_I4P
      return
    endif
    names = names//transfer(chars(1:n), repeat(' ', int(n)))//achar(0)
    deallocate(chars)
    m = m + 1_c_long_long
  enddo
#endif
  endfunction h5_group_names

  function h5_attribute_exists(loc_id, name) result(exists)
  !< Return true if the attribute `name` of the object `loc_id` exists.
  integer(hid_kind), intent(in) :: loc_id !< Object (file, group or dataset).
  character(*),      intent(in) :: name   !< Attribute name.
  logical                       :: exists !< True if the attribute exists.

  exists = .false.
#ifdef VTKFORTRAN_USE_HDF5
  exists = H5Aexists(loc_id, trim(name)//c_null_char) > 0
#endif
  endfunction h5_attribute_exists

  function h5_attribute_write_string(loc_id, name, value, variable_length) result(error)
  !< Write a string attribute: fixed length, the length of the value, padded with NULs, as VTK writes `Type`; or, with
  !< `variable_length`, a variable-length string (as h5py writes a `str`).
  integer(hid_kind), intent(in)           :: loc_id          !< Object (file, group or dataset).
  character(*),      intent(in)           :: name            !< Attribute name.
  character(*),      intent(in)           :: value           !< Attribute value.
  logical,           intent(in), optional :: variable_length !< A variable-length string (default false).
  integer(I4P)                            :: error           !< Error status.
#ifdef VTKFORTRAN_USE_HDF5
  character(kind=c_char), target          :: chars(len(value) + 1) !< Value, as C characters, NUL terminated.
  type(c_ptr),            target          :: ptr    !< The value, for a variable-length string.
  integer                                 :: c      !< Counter.
  integer(c_int64_t)                      :: dtype  !< String type.
  integer(c_int64_t)                      :: space  !< Scalar dataspace.
  integer(c_int64_t)                      :: attr   !< Attribute.
  logical                                 :: is_vlen !< A variable-length string.

  error = 1_I4P
  is_vlen = .false.
  if (present(variable_length)) is_vlen = variable_length
  chars = c_null_char
  do c=1, len(value)
    chars(c) = value(c:c)
  enddo
  dtype = H5Tcopy(H5T_C_S1_g)
  if (dtype < 0) return
  if (is_vlen) then
    if (H5Tset_size(dtype, -1_c_size_t) < 0) return ! H5T_VARIABLE
  else
    if (H5Tset_size(dtype, int(max(len(value), 1), c_size_t)) < 0) return
    if (H5Tset_strpad(dtype, H5T_STR_NULLPAD) < 0) return
  endif
  if (H5Aexists(loc_id, trim(name)//c_null_char) > 0) then ! overwritten
    if (H5Adelete(loc_id, trim(name)//c_null_char) < 0) return
  endif
  space = H5Screate(H5S_SCALAR)
  attr = H5Acreate2(loc_id, trim(name)//c_null_char, dtype, space, H5P_DEFAULT, H5P_DEFAULT)
  if (attr >= 0) then
    if (is_vlen) then
      ptr = c_loc(chars)
      if (H5Awrite(attr, dtype, c_loc(ptr)) >= 0) error = 0_I4P
    else
      if (H5Awrite(attr, dtype, c_loc(chars)) >= 0) error = 0_I4P
    endif
    if (H5Aclose(attr) < 0) error = 1_I4P
  endif
  if (H5Sclose(space) < 0) error = 1_I4P
  if (H5Tclose(dtype) < 0) error = 1_I4P
#else
  error = 1_I4P
#endif
  endfunction h5_attribute_write_string

  function h5_attribute_write_I4P(loc_id, name, value) result(error)
  !< Write a rank-1 attribute of `I4P` values (HDF5 native int32).
  integer(hid_kind), intent(in)         :: loc_id   !< Object (file, group or dataset).
  character(*),      intent(in)         :: name     !< Attribute name.
  integer(I4P),      intent(in), target :: value(:) !< Attribute values.
  integer(I4P)                          :: error    !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = attribute_write(loc_id, name, H5T_NATIVE_INT32_g, size(value, kind=I8P), c_loc(value))
#else
  error = 1_I4P
#endif
  endfunction h5_attribute_write_I4P

  function h5_attribute_write_I8P(loc_id, name, value) result(error)
  !< Write a rank-1 attribute of `I8P` values (HDF5 native int64).
  integer(hid_kind), intent(in)         :: loc_id   !< Object (file, group or dataset).
  character(*),      intent(in)         :: name     !< Attribute name.
  integer(I8P),      intent(in), target :: value(:) !< Attribute values.
  integer(I4P)                          :: error    !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = attribute_write(loc_id, name, H5T_NATIVE_INT64_g, size(value, kind=I8P), c_loc(value))
#else
  error = 1_I4P
#endif
  endfunction h5_attribute_write_I8P

  function h5_attribute_write_R8P(loc_id, name, value) result(error)
  !< Write a rank-1 attribute of `R8P` values (HDF5 native double).
  integer(hid_kind), intent(in)         :: loc_id   !< Object (file, group or dataset).
  character(*),      intent(in)         :: name     !< Attribute name.
  real(R8P),         intent(in), target :: value(:) !< Attribute values.
  integer(I4P)                          :: error    !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = attribute_write(loc_id, name, H5T_NATIVE_DOUBLE_g, size(value, kind=I8P), c_loc(value))
#else
  error = 1_I4P
#endif
  endfunction h5_attribute_write_R8P

  function h5_attribute_read_string(loc_id, name, value) result(error)
  !< Read a string attribute: fixed length (trailing NULs dropped) or variable length (as h5py writes a `str`; the first
  !< string of an array of them), ASCII or UTF-8.
  integer(hid_kind),             intent(in)  :: loc_id !< Object (file, group or dataset).
  character(*),                  intent(in)  :: name   !< Attribute name.
  character(len=:), allocatable, intent(out) :: value  !< Attribute value.
  integer(I4P)                               :: error  !< Error status.
#ifdef VTKFORTRAN_USE_HDF5
  character(kind=c_char), allocatable, target :: chars(:) !< Raw value, as C characters.
  type(c_ptr),            allocatable, target :: ptrs(:)  !< Variable-length strings, as C pointers.
  character(kind=c_char), pointer            :: text(:)  !< A variable-length string.
  integer(c_int64_t)                         :: attr   !< Attribute.
  integer                                    :: c      !< Counter.
  integer(c_int64_t)                         :: dtype  !< Stored type.
  integer(c_int64_t)                         :: memtype !< Memory type of a variable-length string.
  integer(c_size_t)                          :: n      !< Stored length.

  error = 1_I4P
  value = ''
  attr = H5Aopen(loc_id, trim(name)//c_null_char, H5P_DEFAULT)
  if (attr < 0) return
  dtype = H5Aget_type(attr)
  if (H5Tis_variable_str(dtype) > 0) then ! read as C strings (the library allocates them), then freed
    memtype = H5Tcopy(dtype)
    allocate(ptrs(max(attribute_size(attr), 1_I8P)))
    ptrs = c_null_ptr
    if (H5Aread(attr, memtype, c_loc(ptrs)) >= 0) then
      if (c_associated(ptrs(1))) then
        n = c_strlen(ptrs(1))
        call c_f_pointer(ptrs(1), text, [n])
        do c=1, int(n)
          value = value//text(c)
        enddo
      endif
      error = 0_I4P
      do c=1, size(ptrs)
        if (c_associated(ptrs(c))) then
          if (H5free_memory(ptrs(c)) < 0) error = 1_I4P
        endif
      enddo
    endif
    if (H5Tclose(memtype) < 0) error = 1_I4P
  else
    n = H5Tget_size(dtype)
    allocate(chars(max(n, 1_c_size_t)))
    if (H5Aread(attr, dtype, c_loc(chars)) >= 0) then
      do c=1, int(n)
        if (chars(c) == c_null_char) exit
        value = value//chars(c)
      enddo
      error = 0_I4P
    endif
  endif
  if (H5Tclose(dtype) < 0) error = 1_I4P
  if (H5Aclose(attr) < 0) error = 1_I4P
#else
  error = 1_I4P
  value = ''
#endif
  endfunction h5_attribute_read_string

  function h5_attribute_read_I8P(loc_id, name, value) result(error)
  !< Read a numeric attribute as `I8P` values.
  integer(hid_kind),         intent(in)  :: loc_id   !< Object (file, group or dataset).
  character(*),              intent(in)  :: name     !< Attribute name.
  integer(I8P), allocatable, intent(out) :: value(:) !< Attribute values.
  integer(I4P)                           :: error    !< Error status.
#ifdef VTKFORTRAN_USE_HDF5
  integer(c_int64_t)                     :: attr     !< Attribute.
  integer(I8P), allocatable, target      :: buffer(:) !< Values read.

  error = 1_I4P
  attr = H5Aopen(loc_id, trim(name)//c_null_char, H5P_DEFAULT)
  if (attr < 0) then
    allocate(value(0))
    return
  endif
  allocate(buffer(attribute_size(attr)))
  if (H5Aread(attr, H5T_NATIVE_INT64_g, c_loc(buffer)) >= 0) error = 0_I4P
  if (H5Aclose(attr) < 0) error = 1_I4P
  call move_alloc(buffer, value)
#else
  error = 1_I4P
  allocate(value(0))
#endif
  endfunction h5_attribute_read_I8P

  function h5_attribute_read_R8P(loc_id, name, value) result(error)
  !< Read a numeric attribute as `R8P` values.
  integer(hid_kind),      intent(in)  :: loc_id   !< Object (file, group or dataset).
  character(*),           intent(in)  :: name     !< Attribute name.
  real(R8P), allocatable, intent(out) :: value(:) !< Attribute values.
  integer(I4P)                        :: error    !< Error status.
#ifdef VTKFORTRAN_USE_HDF5
  integer(c_int64_t)                  :: attr     !< Attribute.
  real(R8P), allocatable, target      :: buffer(:) !< Values read.

  error = 1_I4P
  attr = H5Aopen(loc_id, trim(name)//c_null_char, H5P_DEFAULT)
  if (attr < 0) then
    allocate(value(0))
    return
  endif
  allocate(buffer(attribute_size(attr)))
  if (H5Aread(attr, H5T_NATIVE_DOUBLE_g, c_loc(buffer)) >= 0) error = 0_I4P
  if (H5Aclose(attr) < 0) error = 1_I4P
  call move_alloc(buffer, value)
#else
  error = 1_I4P
  allocate(value(0))
#endif
  endfunction h5_attribute_read_R8P

  function h5_deflate_available() result(available)
  !< Return true if HDF5 has the deflate (zlib) filter.
  logical :: available !< True if the deflate filter is available.

  available = .false.
#ifdef VTKFORTRAN_USE_HDF5
  if (h5_initialize() /= 0) return
  available = H5Zfilter_avail(H5Z_FILTER_DEFLATE) > 0
#endif
  endfunction h5_deflate_available

  function h5_parallel_filters_available() result(available)
  !< Return true if the ranks of a communicator can write deflated datasets: the library built with MPI, an HDF5 with the
  !< deflate filter and parallel filtered writes (HDF5 1.10.2 or later; the writes are then collective).
  logical        :: available            !< True if deflated datasets can be written from the ranks.
  integer(c_int) :: majnum, minnum, relnum !< HDF5 version.

  available = .false.
#ifdef VTKFORTRAN_USE_HDF5
  if (.not.(is_mpi_enabled .and. h5_deflate_available())) return
  if (H5get_libversion(majnum, minnum, relnum) < 0) return
  available = majnum > 1 .or. (majnum == 1 .and. (minnum > 10 .or. (minnum == 10 .and. relnum >= 2)))
#else
  majnum = 0 ; minnum = 0 ; relnum = 0
#endif
  endfunction h5_parallel_filters_available

  function h5_dataset_open(loc_id, name, dset_id) result(error)
  !< Open a dataset (e.g. to write attributes on it).
  integer(hid_kind), intent(in)  :: loc_id  !< Parent (file or group).
  character(*),      intent(in)  :: name    !< Dataset name (or path).
  integer(hid_kind), intent(out) :: dset_id !< Dataset identifier.
  integer(I4P)                   :: error   !< Error status.

  error = 1_I4P
  dset_id = -1_hid_kind
#ifdef VTKFORTRAN_USE_HDF5
  dset_id = H5Dopen2(loc_id, trim(name)//c_null_char, H5P_DEFAULT)
  if (dset_id >= 0) error = 0_I4P
#endif
  endfunction h5_dataset_open

  function h5_dataset_close(dset_id) result(error)
  !< Close a dataset.
  integer(hid_kind), intent(inout) :: dset_id !< Dataset identifier, -1 once closed.
  integer(I4P)                     :: error   !< Error status.

  error = 1_I4P
#ifdef VTKFORTRAN_USE_HDF5
  if (dset_id >= 0) then
    if (H5Dclose(dset_id) >= 0) error = 0_I4P
  endif
#endif
  dset_id = -1_hid_kind
  endfunction h5_dataset_close

  function h5_dataset_dims(loc_id, name, dims) result(error)
  !< Return the shape of a dataset, in Fortran order (the first dimension varies fastest).
  integer(hid_kind),         intent(in)  :: loc_id  !< Parent (file or group).
  character(*),              intent(in)  :: name    !< Dataset name (or path).
  integer(I8P), allocatable, intent(out) :: dims(:) !< Shape, Fortran order.
  integer(I4P)                           :: error   !< Error status.
#ifdef VTKFORTRAN_USE_HDF5
  integer(c_int64_t)                     :: dset    !< Dataset.

  error = 1_I4P
  allocate(dims(0))
  dset = H5Dopen2(loc_id, trim(name)//c_null_char, H5P_DEFAULT)
  if (dset < 0) return
  error = dataset_dims(dset, dims)
  if (H5Dclose(dset) < 0) error = 1_I4P
#else
  error = 1_I4P
  allocate(dims(0))
#endif
  endfunction h5_dataset_dims

  function h5_dataset_chunk(loc_id, name, chunk) result(error)
  !< Return the chunk shape of a dataset, in Fortran order; empty for a dataset that is not chunked.
  integer(hid_kind),         intent(in)  :: loc_id   !< Parent (file or group).
  character(*),              intent(in)  :: name     !< Dataset name (or path).
  integer(I8P), allocatable, intent(out) :: chunk(:) !< Chunk shape, Fortran order.
  integer(I4P)                           :: error    !< Error status.
#ifdef VTKFORTRAN_USE_HDF5
  integer(c_int), parameter              :: H5D_CHUNKED = 2_c_int !< Chunked layout (H5D_layout_t).
  integer(c_int64_t)                     :: dset     !< Dataset.
  integer(c_int64_t)                     :: dcpl     !< Its creation properties.
  integer(c_int)                         :: rank     !< Rank of the chunks.
  integer(c_long_long)                   :: cchunk(32) !< Chunk shape, C order.
  integer                                :: d        !< Counter.

  error = 1_I4P
  allocate(chunk(0))
  dset = H5Dopen2(loc_id, trim(name)//c_null_char, H5P_DEFAULT)
  if (dset < 0) return
  dcpl = H5Dget_create_plist(dset)
  if (dcpl >= 0) then
    if (H5Pget_layout(dcpl) == H5D_CHUNKED) then
      rank = H5Pget_chunk(dcpl, int(size(cchunk), c_int), cchunk)
      if (rank >= 0) then
        chunk = [(int(cchunk(rank - d + 1), I8P), d=1, rank)]
        error = 0_I4P
      endif
    else
      error = 0_I4P
    endif
    if (H5Pclose(dcpl) < 0) error = 1_I4P
  endif
  if (H5Dclose(dset) < 0) error = 1_I4P
#else
  error = 1_I4P
  allocate(chunk(0))
#endif
  endfunction h5_dataset_chunk

  function h5_dataset_info(loc_id, name, data_type, dims) result(error)
  !< Return the VTK type of a numeric dataset (Float64, Float32, Int64, ..., UInt8) and its shape (Fortran order).
  integer(hid_kind),             intent(in)  :: loc_id    !< Parent (file or group).
  character(*),                  intent(in)  :: name      !< Dataset name (or path).
  character(len=:), allocatable, intent(out) :: data_type !< VTK type, empty if not a numeric type.
  integer(I8P),     allocatable, intent(out) :: dims(:)   !< Shape, Fortran order.
  integer(I4P)                               :: error     !< Error status.
#ifdef VTKFORTRAN_USE_HDF5
  integer(c_int64_t)                         :: dset      !< Dataset.
  integer(c_int64_t)                         :: dtype     !< Its type.
  integer(c_int)                             :: class     !< Type class: 0 integer, 1 float.
  integer(c_int)                             :: sign      !< Sign of an integer: 0 unsigned, 1 two's complement.
  integer(c_size_t)                          :: bytes     !< Size of a value.

  data_type = ''
  error = 1_I4P
  dset = H5Dopen2(loc_id, trim(name)//c_null_char, H5P_DEFAULT)
  if (dset < 0) return
  if (dataset_dims(dset, dims) == 0) then
    dtype = H5Dget_type(dset)
    if (dtype >= 0) then
      class = H5Tget_class(dtype)
      bytes = H5Tget_size(dtype)
      sign = H5Tget_sign(dtype)
      if (class == 1_c_int) then
        if (bytes == 8_c_size_t) data_type = 'Float64'
        if (bytes == 4_c_size_t) data_type = 'Float32'
      elseif (class == 0_c_int) then
        if (bytes == 8_c_size_t) data_type = 'Int64'
        if (bytes == 4_c_size_t) data_type = 'Int32'
        if (bytes == 2_c_size_t) data_type = 'Int16'
        if (bytes == 1_c_size_t) data_type = 'Int8'
        if (sign == 0_c_int .and. len(data_type) > 0) data_type = 'U'//data_type
      endif
      error = 0_I4P
      if (H5Tclose(dtype) < 0) error = 1_I4P
    endif
  endif
  if (H5Dclose(dset) < 0) error = 1_I4P
#else
  data_type = ''
  allocate(dims(0))
  error = 1_I4P
#endif
  endfunction h5_dataset_info

  function h5_dataset_read_bytes(loc_id, name, first, count, bytes) result(error)
  !< Read the slice `first+1 ... first+count` of the last Fortran dimension (the first HDF5 one) of a numeric dataset, all
  !< of its other dimensions, as the bytes of its values in the native type of the file's type (see `h5_dataset_info`).
  integer(hid_kind),         intent(in)  :: loc_id   !< Parent (file or group).
  character(*),              intent(in)  :: name     !< Dataset name (or path).
  integer(I8P),              intent(in)  :: first    !< Values (rows) skipped along the last dimension.
  integer(I8P),              intent(in)  :: count    !< Values (rows) read along the last dimension.
  integer(I1P), allocatable, intent(out) :: bytes(:) !< Bytes of the values.
  integer(I4P)                           :: error    !< Error status.
#ifdef VTKFORTRAN_USE_HDF5
  integer(c_int64_t)                     :: dset      !< Dataset.
  integer(c_int64_t)                     :: dtype     !< Its type.
  integer(c_int64_t)                     :: memtype   !< Native type of the values in memory.
  integer(c_int64_t)                     :: filespace !< Dataspace of the dataset.
  integer(c_int64_t)                     :: memspace  !< Dataspace of the values read.
  integer(I8P), allocatable              :: dims(:)   !< Shape, Fortran order.
  integer(c_long_long), allocatable      :: cstart(:), ccount(:) !< Slice, C order.
  integer(I1P), allocatable, target      :: buffer(:) !< Bytes read.

  allocate(bytes(0))
  error = 1_I4P
  dset = H5Dopen2(loc_id, trim(name)//c_null_char, H5P_DEFAULT)
  if (dset < 0) return
  if (dataset_dims(dset, dims) /= 0) allocate(dims(0)) ! no short-circuit in Fortran: dims is checked apart
  if (size(dims) > 0) then
    if (first >= 0_I8P .and. count >= 0_I8P .and. first + count <= dims(size(dims))) then
      dtype = H5Dget_type(dset)
      memtype = H5Tget_native_type(dtype, 1_c_int) ! H5T_DIR_ASCEND
      dims(size(dims)) = count
      allocate(buffer(max(1_I8P, product(dims) * int(H5Tget_size(memtype), I8P))))
      if (product(dims) == 0_I8P) then
        error = 0_I4P
      else
        ccount = c_dims(dims)
        allocate(cstart(size(dims)))
        cstart = 0_c_long_long
        cstart(1) = int(first, c_long_long)
        filespace = H5Dget_space(dset)
        memspace = H5Screate_simple(int(size(dims), c_int), ccount, c_null_ptr)
        if (filespace >= 0 .and. memspace >= 0) then
          if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, cstart, c_null_ptr, ccount, c_null_ptr) >= 0) then
            if (H5Dread(dset, memtype, memspace, filespace, H5P_DEFAULT, c_loc(buffer)) >= 0) error = 0_I4P
          endif
        endif
        if (memspace >= 0) then
          if (H5Sclose(memspace) < 0) error = 1_I4P
        endif
        if (filespace >= 0) then
          if (H5Sclose(filespace) < 0) error = 1_I4P
        endif
      endif
      if (error == 0_I4P) bytes = buffer(1:product(dims) * int(H5Tget_size(memtype), I8P))
      if (H5Tclose(memtype) < 0) error = 1_I4P
      if (H5Tclose(dtype) < 0) error = 1_I4P
    endif
  endif
  if (H5Dclose(dset) < 0) error = 1_I4P
#else
  allocate(bytes(0))
  error = 1_I4P
#endif
  endfunction h5_dataset_read_bytes

  function h5_dataset_resize(loc_id, name, n) result(error)
  !< Resize an appendable dataset along its last Fortran dimension (the first HDF5 one) to `n`: values past it are
  !< dropped, the other dimensions are kept.
  integer(hid_kind), intent(in) :: loc_id  !< Parent (file or group).
  character(*),      intent(in) :: name    !< Dataset name (or path).
  integer(I8P),      intent(in) :: n       !< New size of the last dimension.
  integer(I4P)                  :: error   !< Error status.
#ifdef VTKFORTRAN_USE_HDF5
  integer(c_int64_t)            :: dset    !< Dataset.
  integer(I8P), allocatable     :: dims(:) !< Current shape, Fortran order.

  error = 1_I4P
  dset = H5Dopen2(loc_id, trim(name)//c_null_char, H5P_DEFAULT)
  if (dset < 0) return
  if (dataset_dims(dset, dims) == 0) then
    if (size(dims) > 0) then
      dims(size(dims)) = n
      if (H5Dset_extent(dset, c_dims(dims)) >= 0) error = 0_I4P
    endif
  endif
  if (H5Dclose(dset) < 0) error = 1_I4P
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_resize

  ! datasets of every kind: thin front ends of dataset_write, dataset_append and dataset_read
  function h5_dataset_write_R8P(loc_id, name, x, dims, appendable, deflate, comm, chunk_bytes) result(error)
  !< Create a dataset of `R8P` values (HDF5 native double), shape `dims` in Fortran order.
  integer(hid_kind), intent(in)           :: loc_id     !< Parent (file or group).
  character(*),      intent(in)           :: name       !< Dataset name.
  real(R8P),         intent(in), target   :: x(*)       !< Values, any rank (sequence association).
  integer(I8P),      intent(in)           :: dims(:)    !< Shape, Fortran order.
  logical,           intent(in), optional :: appendable !< Chunked, unlimited along the last dimension.
  integer(I4P),      intent(in), optional :: deflate    !< Deflate level (1-9), implies chunking.
  integer(I4P),      intent(in), optional :: comm       !< MPI communicator: every rank writes its rows (appendable, collective).
  integer(I8P),      intent(in), optional :: chunk_bytes !< Bytes of a chunk at most (default 1 MiB), at least 1.
  integer(I4P)                            :: error      !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_write(loc_id, name, H5T_NATIVE_DOUBLE_g, 8_I8P, dims, c_loc(x), appendable, deflate, comm, chunk_bytes)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_write_R8P

  function h5_dataset_write_R4P(loc_id, name, x, dims, appendable, deflate, comm, chunk_bytes) result(error)
  !< Create a dataset of `R4P` values (HDF5 native float), shape `dims` in Fortran order.
  integer(hid_kind), intent(in)           :: loc_id     !< Parent (file or group).
  character(*),      intent(in)           :: name       !< Dataset name.
  real(R4P),         intent(in), target   :: x(*)       !< Values, any rank (sequence association).
  integer(I8P),      intent(in)           :: dims(:)    !< Shape, Fortran order.
  logical,           intent(in), optional :: appendable !< Chunked, unlimited along the last dimension.
  integer(I4P),      intent(in), optional :: deflate    !< Deflate level (1-9), implies chunking.
  integer(I4P),      intent(in), optional :: comm       !< MPI communicator: every rank writes its rows (appendable, collective).
  integer(I8P),      intent(in), optional :: chunk_bytes !< Bytes of a chunk at most (default 1 MiB), at least 1.
  integer(I4P)                            :: error      !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_write(loc_id, name, H5T_NATIVE_FLOAT_g, 4_I8P, dims, c_loc(x), appendable, deflate, comm, chunk_bytes)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_write_R4P

  function h5_dataset_write_I8P(loc_id, name, x, dims, appendable, deflate, unsigned, comm, chunk_bytes) result(error)
  !< Create a dataset of `I8P` values (HDF5 native int64, or uint64 with `unsigned=.true.`), shape `dims` in Fortran order.
  integer(hid_kind), intent(in)           :: loc_id     !< Parent (file or group).
  character(*),      intent(in)           :: name       !< Dataset name.
  integer(I8P),      intent(in), target   :: x(*)       !< Values, any rank (sequence association).
  integer(I8P),      intent(in)           :: dims(:)    !< Shape, Fortran order.
  logical,           intent(in), optional :: appendable !< Chunked, unlimited along the last dimension.
  integer(I4P),      intent(in), optional :: deflate    !< Deflate level (1-9), implies chunking.
  logical,           intent(in), optional :: unsigned   !< Store the bits as the unsigned type of the same width.
  integer(I4P),      intent(in), optional :: comm       !< MPI communicator: every rank writes its rows (appendable, collective).
  integer(I8P),      intent(in), optional :: chunk_bytes !< Bytes of a chunk at most (default 1 MiB), at least 1.
  integer(I4P)                            :: error      !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_write(loc_id, name, merge(H5T_NATIVE_UINT64_g, H5T_NATIVE_INT64_g, is_true(unsigned)), 8_I8P, dims, &
                        c_loc(x), appendable, deflate, comm, chunk_bytes)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_write_I8P

  function h5_dataset_write_I4P(loc_id, name, x, dims, appendable, deflate, unsigned, comm, chunk_bytes) result(error)
  !< Create a dataset of `I4P` values (HDF5 native int32, or uint32 with `unsigned=.true.`), shape `dims` in Fortran order.
  integer(hid_kind), intent(in)           :: loc_id     !< Parent (file or group).
  character(*),      intent(in)           :: name       !< Dataset name.
  integer(I4P),      intent(in), target   :: x(*)       !< Values, any rank (sequence association).
  integer(I8P),      intent(in)           :: dims(:)    !< Shape, Fortran order.
  logical,           intent(in), optional :: appendable !< Chunked, unlimited along the last dimension.
  integer(I4P),      intent(in), optional :: deflate    !< Deflate level (1-9), implies chunking.
  logical,           intent(in), optional :: unsigned   !< Store the bits as the unsigned type of the same width.
  integer(I4P),      intent(in), optional :: comm       !< MPI communicator: every rank writes its rows (appendable, collective).
  integer(I8P),      intent(in), optional :: chunk_bytes !< Bytes of a chunk at most (default 1 MiB), at least 1.
  integer(I4P)                            :: error      !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_write(loc_id, name, merge(H5T_NATIVE_UINT32_g, H5T_NATIVE_INT32_g, is_true(unsigned)), 4_I8P, dims, &
                        c_loc(x), appendable, deflate, comm, chunk_bytes)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_write_I4P

  function h5_dataset_write_I2P(loc_id, name, x, dims, appendable, deflate, unsigned, comm, chunk_bytes) result(error)
  !< Create a dataset of `I2P` values (HDF5 native int16, or uint16 with `unsigned=.true.`), shape `dims` in Fortran order.
  integer(hid_kind), intent(in)           :: loc_id     !< Parent (file or group).
  character(*),      intent(in)           :: name       !< Dataset name.
  integer(I2P),      intent(in), target   :: x(*)       !< Values, any rank (sequence association).
  integer(I8P),      intent(in)           :: dims(:)    !< Shape, Fortran order.
  logical,           intent(in), optional :: appendable !< Chunked, unlimited along the last dimension.
  integer(I4P),      intent(in), optional :: deflate    !< Deflate level (1-9), implies chunking.
  logical,           intent(in), optional :: unsigned   !< Store the bits as the unsigned type of the same width.
  integer(I4P),      intent(in), optional :: comm       !< MPI communicator: every rank writes its rows (appendable, collective).
  integer(I8P),      intent(in), optional :: chunk_bytes !< Bytes of a chunk at most (default 1 MiB), at least 1.
  integer(I4P)                            :: error      !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_write(loc_id, name, merge(H5T_NATIVE_UINT16_g, H5T_NATIVE_INT16_g, is_true(unsigned)), 2_I8P, dims, &
                        c_loc(x), appendable, deflate, comm, chunk_bytes)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_write_I2P

  function h5_dataset_write_I1P(loc_id, name, x, dims, appendable, deflate, unsigned, comm, chunk_bytes) result(error)
  !< Create a dataset of `I1P` values (HDF5 native int8, or uint8 with `unsigned=.true.`), shape `dims` in Fortran order.
  integer(hid_kind), intent(in)           :: loc_id     !< Parent (file or group).
  character(*),      intent(in)           :: name       !< Dataset name.
  integer(I1P),      intent(in), target   :: x(*)       !< Values, any rank (sequence association).
  integer(I8P),      intent(in)           :: dims(:)    !< Shape, Fortran order.
  logical,           intent(in), optional :: appendable !< Chunked, unlimited along the last dimension.
  integer(I4P),      intent(in), optional :: deflate    !< Deflate level (1-9), implies chunking.
  logical,           intent(in), optional :: unsigned   !< Store the bits as the unsigned type of the same width.
  integer(I4P),      intent(in), optional :: comm       !< MPI communicator: every rank writes its rows (appendable, collective).
  integer(I8P),      intent(in), optional :: chunk_bytes !< Bytes of a chunk at most (default 1 MiB), at least 1.
  integer(I4P)                            :: error      !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_write(loc_id, name, merge(H5T_NATIVE_UINT8_g, H5T_NATIVE_INT8_g, is_true(unsigned)), 1_I8P, dims, &
                        c_loc(x), appendable, deflate, comm, chunk_bytes)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_write_I1P

  function h5_dataset_slab_R8P(loc_id, name, x, dims, mem_dims, mem_start, file_start, count, create, append, &
                               chunk_bytes) result(error)
  !< Write the block `count` of the values `x` (R8P; shape `mem_dims`, the block at `mem_start`) into the dataset `name`,
  !< at `file_start` (Fortran order, from 0); with `create`, the dataset is created first, shape `dims` (in a parallel file
  !< by every rank: collective; the blocks are then written by each rank alone). An empty block writes nothing.
  integer(hid_kind), intent(in)           :: loc_id        !< Parent (file or group).
  character(*),      intent(in)           :: name          !< Dataset name.
  real(R8P),        intent(in), target   :: x(*)          !< Values, any rank (sequence association).
  integer(I8P),      intent(in)           :: dims(:)       !< Shape of the dataset, Fortran order.
  integer(I8P),      intent(in)           :: mem_dims(:)   !< Shape of the values, Fortran order.
  integer(I8P),      intent(in)           :: mem_start(:)  !< Start of the block in the values, from 0.
  integer(I8P),      intent(in)           :: file_start(:) !< Start of the block in the dataset, from 0.
  integer(I8P),      intent(in)           :: count(:)      !< Shape of the block.
  logical,           intent(in)           :: create        !< Create the dataset first.
  logical,           intent(in), optional :: append        !< Grow along the last dimension (see `dataset_slab`).
  integer(I8P),      intent(in), optional :: chunk_bytes   !< Bytes of a chunk at most, with append (default 1 MiB).
  integer(I4P)                            :: error         !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_slab(loc_id, name, H5T_NATIVE_DOUBLE_g, dims, mem_dims, mem_start, file_start, count, &
                       c_loc(x), create, append, chunk_bytes)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_slab_R8P

  function h5_dataset_slab_R4P(loc_id, name, x, dims, mem_dims, mem_start, file_start, count, create, append, &
                               chunk_bytes) result(error)
  !< Write the block `count` of the values `x` (R4P; shape `mem_dims`, the block at `mem_start`) into the dataset `name`,
  !< at `file_start` (Fortran order, from 0); with `create`, the dataset is created first, shape `dims` (in a parallel file
  !< by every rank: collective; the blocks are then written by each rank alone). An empty block writes nothing.
  integer(hid_kind), intent(in)           :: loc_id        !< Parent (file or group).
  character(*),      intent(in)           :: name          !< Dataset name.
  real(R4P),        intent(in), target   :: x(*)          !< Values, any rank (sequence association).
  integer(I8P),      intent(in)           :: dims(:)       !< Shape of the dataset, Fortran order.
  integer(I8P),      intent(in)           :: mem_dims(:)   !< Shape of the values, Fortran order.
  integer(I8P),      intent(in)           :: mem_start(:)  !< Start of the block in the values, from 0.
  integer(I8P),      intent(in)           :: file_start(:) !< Start of the block in the dataset, from 0.
  integer(I8P),      intent(in)           :: count(:)      !< Shape of the block.
  logical,           intent(in)           :: create        !< Create the dataset first.
  logical,           intent(in), optional :: append        !< Grow along the last dimension (see `dataset_slab`).
  integer(I8P),      intent(in), optional :: chunk_bytes   !< Bytes of a chunk at most, with append (default 1 MiB).
  integer(I4P)                            :: error         !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_slab(loc_id, name, H5T_NATIVE_FLOAT_g, dims, mem_dims, mem_start, file_start, count, &
                       c_loc(x), create, append, chunk_bytes)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_slab_R4P

  function h5_dataset_slab_I8P(loc_id, name, x, dims, mem_dims, mem_start, file_start, count, create, unsigned, &
                               append, chunk_bytes) result(error)
  !< Write the block `count` of the values `x` (I8P; shape `mem_dims`, the block at `mem_start`) into the dataset `name`,
  !< at `file_start` (Fortran order, from 0); with `create`, the dataset is created first, shape `dims` (in a parallel file
  !< by every rank: collective; the blocks are then written by each rank alone). An empty block writes nothing.
  integer(hid_kind), intent(in)           :: loc_id        !< Parent (file or group).
  character(*),      intent(in)           :: name          !< Dataset name.
  integer(I8P),     intent(in), target   :: x(*)          !< Values, any rank (sequence association).
  integer(I8P),      intent(in)           :: dims(:)       !< Shape of the dataset, Fortran order.
  integer(I8P),      intent(in)           :: mem_dims(:)   !< Shape of the values, Fortran order.
  integer(I8P),      intent(in)           :: mem_start(:)  !< Start of the block in the values, from 0.
  integer(I8P),      intent(in)           :: file_start(:) !< Start of the block in the dataset, from 0.
  integer(I8P),      intent(in)           :: count(:)      !< Shape of the block.
  logical,           intent(in)           :: create        !< Create the dataset first.
  logical,           intent(in), optional :: append        !< Grow along the last dimension (see `dataset_slab`).
  integer(I8P),      intent(in), optional :: chunk_bytes   !< Bytes of a chunk at most, with append (default 1 MiB).
  logical,           intent(in), optional :: unsigned      !< Store the bits as the unsigned type of their width.
  integer(I4P)                            :: error         !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_slab(loc_id, name, merge(H5T_NATIVE_UINT64_g, H5T_NATIVE_INT64_g, is_true(unsigned)), dims, mem_dims, mem_start, &
                       file_start, count, &
                       c_loc(x), create, append, chunk_bytes)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_slab_I8P

  function h5_dataset_slab_I4P(loc_id, name, x, dims, mem_dims, mem_start, file_start, count, create, unsigned, &
                               append, chunk_bytes) result(error)
  !< Write the block `count` of the values `x` (I4P; shape `mem_dims`, the block at `mem_start`) into the dataset `name`,
  !< at `file_start` (Fortran order, from 0); with `create`, the dataset is created first, shape `dims` (in a parallel file
  !< by every rank: collective; the blocks are then written by each rank alone). An empty block writes nothing.
  integer(hid_kind), intent(in)           :: loc_id        !< Parent (file or group).
  character(*),      intent(in)           :: name          !< Dataset name.
  integer(I4P),     intent(in), target   :: x(*)          !< Values, any rank (sequence association).
  integer(I8P),      intent(in)           :: dims(:)       !< Shape of the dataset, Fortran order.
  integer(I8P),      intent(in)           :: mem_dims(:)   !< Shape of the values, Fortran order.
  integer(I8P),      intent(in)           :: mem_start(:)  !< Start of the block in the values, from 0.
  integer(I8P),      intent(in)           :: file_start(:) !< Start of the block in the dataset, from 0.
  integer(I8P),      intent(in)           :: count(:)      !< Shape of the block.
  logical,           intent(in)           :: create        !< Create the dataset first.
  logical,           intent(in), optional :: append        !< Grow along the last dimension (see `dataset_slab`).
  integer(I8P),      intent(in), optional :: chunk_bytes   !< Bytes of a chunk at most, with append (default 1 MiB).
  logical,           intent(in), optional :: unsigned      !< Store the bits as the unsigned type of their width.
  integer(I4P)                            :: error         !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_slab(loc_id, name, merge(H5T_NATIVE_UINT32_g, H5T_NATIVE_INT32_g, is_true(unsigned)), dims, mem_dims, mem_start, &
                       file_start, count, &
                       c_loc(x), create, append, chunk_bytes)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_slab_I4P

  function h5_dataset_slab_I2P(loc_id, name, x, dims, mem_dims, mem_start, file_start, count, create, unsigned, &
                               append, chunk_bytes) result(error)
  !< Write the block `count` of the values `x` (I2P; shape `mem_dims`, the block at `mem_start`) into the dataset `name`,
  !< at `file_start` (Fortran order, from 0); with `create`, the dataset is created first, shape `dims` (in a parallel file
  !< by every rank: collective; the blocks are then written by each rank alone). An empty block writes nothing.
  integer(hid_kind), intent(in)           :: loc_id        !< Parent (file or group).
  character(*),      intent(in)           :: name          !< Dataset name.
  integer(I2P),     intent(in), target   :: x(*)          !< Values, any rank (sequence association).
  integer(I8P),      intent(in)           :: dims(:)       !< Shape of the dataset, Fortran order.
  integer(I8P),      intent(in)           :: mem_dims(:)   !< Shape of the values, Fortran order.
  integer(I8P),      intent(in)           :: mem_start(:)  !< Start of the block in the values, from 0.
  integer(I8P),      intent(in)           :: file_start(:) !< Start of the block in the dataset, from 0.
  integer(I8P),      intent(in)           :: count(:)      !< Shape of the block.
  logical,           intent(in)           :: create        !< Create the dataset first.
  logical,           intent(in), optional :: append        !< Grow along the last dimension (see `dataset_slab`).
  integer(I8P),      intent(in), optional :: chunk_bytes   !< Bytes of a chunk at most, with append (default 1 MiB).
  logical,           intent(in), optional :: unsigned      !< Store the bits as the unsigned type of their width.
  integer(I4P)                            :: error         !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_slab(loc_id, name, merge(H5T_NATIVE_UINT16_g, H5T_NATIVE_INT16_g, is_true(unsigned)), dims, mem_dims, mem_start, &
                       file_start, count, &
                       c_loc(x), create, append, chunk_bytes)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_slab_I2P

  function h5_dataset_slab_I1P(loc_id, name, x, dims, mem_dims, mem_start, file_start, count, create, unsigned, &
                               append, chunk_bytes) result(error)
  !< Write the block `count` of the values `x` (I1P; shape `mem_dims`, the block at `mem_start`) into the dataset `name`,
  !< at `file_start` (Fortran order, from 0); with `create`, the dataset is created first, shape `dims` (in a parallel file
  !< by every rank: collective; the blocks are then written by each rank alone). An empty block writes nothing.
  integer(hid_kind), intent(in)           :: loc_id        !< Parent (file or group).
  character(*),      intent(in)           :: name          !< Dataset name.
  integer(I1P),     intent(in), target   :: x(*)          !< Values, any rank (sequence association).
  integer(I8P),      intent(in)           :: dims(:)       !< Shape of the dataset, Fortran order.
  integer(I8P),      intent(in)           :: mem_dims(:)   !< Shape of the values, Fortran order.
  integer(I8P),      intent(in)           :: mem_start(:)  !< Start of the block in the values, from 0.
  integer(I8P),      intent(in)           :: file_start(:) !< Start of the block in the dataset, from 0.
  integer(I8P),      intent(in)           :: count(:)      !< Shape of the block.
  logical,           intent(in)           :: create        !< Create the dataset first.
  logical,           intent(in), optional :: append        !< Grow along the last dimension (see `dataset_slab`).
  integer(I8P),      intent(in), optional :: chunk_bytes   !< Bytes of a chunk at most, with append (default 1 MiB).
  logical,           intent(in), optional :: unsigned      !< Store the bits as the unsigned type of their width.
  integer(I4P)                            :: error         !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_slab(loc_id, name, merge(H5T_NATIVE_UINT8_g, H5T_NATIVE_INT8_g, is_true(unsigned)), dims, mem_dims, mem_start, &
                       file_start, count, &
                       c_loc(x), create, append, chunk_bytes)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_slab_I1P

  pure function h5_name_invalid(name) result(reason)
  !< Why `name` cannot be the name of a VTKHDF array or block, empty if it can: VTKHDF names cannot contain `/` (the
  !< separator of HDF5 paths) nor `.` (the VTKHDF specification, Limitations), and cannot be empty. HDF5 sets no limit
  !< to the length of a name (measured: 200000 characters accepted).
  character(*), intent(in)      :: name   !< Name.
  character(len=:), allocatable :: reason !< Why it is refused, empty if it is not.

  reason = ''
  if (len_trim(name) == 0) then
    reason = 'is empty'
  elseif (index(name, '/') > 0) then
    reason = "contains '/' (VTKHDF names contain neither '/' nor '.')"
  elseif (index(name, '.') > 0) then
    reason = "contains '.' (VTKHDF names contain neither '/' nor '.')"
  endif
  endfunction h5_name_invalid

  function h5_mpi_max(comm, n) result(nmax)
  !< The maximum of `n` over the ranks of `comm` (a Fortran MPI communicator, collective); `n` without MPI.
  integer(I4P), intent(in) :: comm !< MPI communicator (Fortran handle).
  integer(I8P), intent(in) :: n    !< Value of this rank.
  integer(I8P)             :: nmax !< Maximum over the ranks.
#ifdef VTKFORTRAN_USE_MPI
  type(MPI_Comm)           :: c    !< Communicator.

  c%MPI_VAL = comm
  call MPI_Allreduce(n, nmax, 1, MPI_INTEGER8, MPI_MAX, c)
#else
  nmax = n + 0_I8P * comm
#endif
  endfunction h5_mpi_max

  function h5_mpi_rank(comm) result(rank)
  !< The rank of this process in `comm` (a Fortran MPI communicator); 0 without MPI.
  integer(I4P), intent(in) :: comm !< MPI communicator (Fortran handle).
  integer(I4P)             :: rank !< Rank.
#ifdef VTKFORTRAN_USE_MPI
  type(MPI_Comm)           :: c    !< Communicator.
  integer                  :: r    !< Rank.

  c%MPI_VAL = comm
  call MPI_Comm_rank(c, r)
  rank = int(r, I4P)
#else
  rank = 0_I4P * comm
#endif
  endfunction h5_mpi_rank

  function h5_mpi_allgather(comm, values, gathered) result(error)
  !< The values of every rank of `comm` (a Fortran MPI communicator), `gathered(:, r)` those of the rank `r - 1`; the
  !< values of this rank alone without MPI (and an error).
  integer(I4P),              intent(in)  :: comm          !< MPI communicator (Fortran handle).
  integer(I8P),              intent(in)  :: values(:)     !< Values of this rank.
  integer(I8P), allocatable, intent(out) :: gathered(:,:) !< Values of every rank.
  integer(I4P)                           :: error         !< Error status.
#ifdef VTKFORTRAN_USE_MPI
  type(MPI_Comm)                         :: c             !< Communicator.
  integer                                :: size_         !< Ranks.

  c%MPI_VAL = comm
  call MPI_Comm_size(c, size_)
  allocate(gathered(size(values), size_))
  call MPI_Allgather(values, size(values), MPI_INTEGER8, gathered, size(values), MPI_INTEGER8, c)
  error = 0_I4P
#else
  allocate(gathered(size(values), 1))
  gathered(:, 1) = values
  error = 1_I4P + 0_I4P * comm
#endif
  endfunction h5_mpi_allgather

  function h5_dataset_append_R8P(loc_id, name, x, dims, comm) result(error)
  !< Append `R8P` values to an appendable dataset; `dims` is the shape of the values appended, Fortran order.
  integer(hid_kind), intent(in)         :: loc_id  !< Parent (file or group).
  character(*),      intent(in)         :: name    !< Dataset name.
  real(R8P),         intent(in), target   :: x(*)       !< Values appended, any rank.
  integer(I8P),      intent(in)           :: dims(:) !< Shape of the values appended, Fortran order.
  integer(I4P),      intent(in), optional :: comm    !< MPI communicator: every rank appends its values, in rank order.
  integer(I4P)                          :: error   !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_append(loc_id, name, H5T_NATIVE_DOUBLE_g, dims, c_loc(x), comm)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_append_R8P

  function h5_dataset_append_R4P(loc_id, name, x, dims, comm) result(error)
  !< Append `R4P` values to an appendable dataset; `dims` is the shape of the values appended, Fortran order.
  integer(hid_kind), intent(in)         :: loc_id  !< Parent (file or group).
  character(*),      intent(in)         :: name    !< Dataset name.
  real(R4P),         intent(in), target   :: x(*)       !< Values appended, any rank.
  integer(I8P),      intent(in)           :: dims(:) !< Shape of the values appended, Fortran order.
  integer(I4P),      intent(in), optional :: comm    !< MPI communicator: every rank appends its values, in rank order.
  integer(I4P)                          :: error   !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_append(loc_id, name, H5T_NATIVE_FLOAT_g, dims, c_loc(x), comm)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_append_R4P

  function h5_dataset_append_I8P(loc_id, name, x, dims, unsigned, comm) result(error)
  !< Append `I8P` values to an appendable dataset; `dims` is the shape of the values appended, Fortran order.
  integer(hid_kind), intent(in)           :: loc_id   !< Parent (file or group).
  character(*),      intent(in)           :: name     !< Dataset name.
  integer(I8P),      intent(in), target   :: x(*)       !< Values appended, any rank.
  integer(I8P),      intent(in)           :: dims(:)  !< Shape of the values appended, Fortran order.
  integer(I4P),      intent(in), optional :: comm    !< MPI communicator: every rank appends its values, in rank order.
  logical,           intent(in), optional :: unsigned !< The values are the bits of the unsigned type of the same width.
  integer(I4P)                            :: error    !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_append(loc_id, name, merge(H5T_NATIVE_UINT64_g, H5T_NATIVE_INT64_g, is_true(unsigned)), dims, c_loc(x), comm)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_append_I8P

  function h5_dataset_append_I4P(loc_id, name, x, dims, unsigned, comm) result(error)
  !< Append `I4P` values to an appendable dataset; `dims` is the shape of the values appended, Fortran order.
  integer(hid_kind), intent(in)           :: loc_id   !< Parent (file or group).
  character(*),      intent(in)           :: name     !< Dataset name.
  integer(I4P),      intent(in), target   :: x(*)       !< Values appended, any rank.
  integer(I8P),      intent(in)           :: dims(:)  !< Shape of the values appended, Fortran order.
  integer(I4P),      intent(in), optional :: comm    !< MPI communicator: every rank appends its values, in rank order.
  logical,           intent(in), optional :: unsigned !< The values are the bits of the unsigned type of the same width.
  integer(I4P)                            :: error    !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_append(loc_id, name, merge(H5T_NATIVE_UINT32_g, H5T_NATIVE_INT32_g, is_true(unsigned)), dims, c_loc(x), comm)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_append_I4P

  function h5_dataset_append_I2P(loc_id, name, x, dims, unsigned, comm) result(error)
  !< Append `I2P` values to an appendable dataset; `dims` is the shape of the values appended, Fortran order.
  integer(hid_kind), intent(in)           :: loc_id   !< Parent (file or group).
  character(*),      intent(in)           :: name     !< Dataset name.
  integer(I2P),      intent(in), target   :: x(*)       !< Values appended, any rank.
  integer(I8P),      intent(in)           :: dims(:)  !< Shape of the values appended, Fortran order.
  integer(I4P),      intent(in), optional :: comm    !< MPI communicator: every rank appends its values, in rank order.
  logical,           intent(in), optional :: unsigned !< The values are the bits of the unsigned type of the same width.
  integer(I4P)                            :: error    !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_append(loc_id, name, merge(H5T_NATIVE_UINT16_g, H5T_NATIVE_INT16_g, is_true(unsigned)), dims, c_loc(x), comm)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_append_I2P

  function h5_dataset_append_I1P(loc_id, name, x, dims, unsigned, comm) result(error)
  !< Append `I1P` values to an appendable dataset; `dims` is the shape of the values appended, Fortran order.
  integer(hid_kind), intent(in)           :: loc_id   !< Parent (file or group).
  character(*),      intent(in)           :: name     !< Dataset name.
  integer(I1P),      intent(in), target   :: x(*)       !< Values appended, any rank.
  integer(I8P),      intent(in)           :: dims(:)  !< Shape of the values appended, Fortran order.
  integer(I4P),      intent(in), optional :: comm    !< MPI communicator: every rank appends its values, in rank order.
  logical,           intent(in), optional :: unsigned !< The values are the bits of the unsigned type of the same width.
  integer(I4P)                            :: error    !< Error status.

#ifdef VTKFORTRAN_USE_HDF5
  error = dataset_append(loc_id, name, merge(H5T_NATIVE_UINT8_g, H5T_NATIVE_INT8_g, is_true(unsigned)), dims, c_loc(x), comm)
#else
  error = 1_I4P
#endif
  endfunction h5_dataset_append_I1P

  function h5_dataset_read_R8P(loc_id, name, x) result(error)
  !< Read all the values of a dataset as `R8P`, flattened in the order of its elements (the shape: `h5_dataset_dims`).
  integer(hid_kind),      intent(in)  :: loc_id !< Parent (file or group).
  character(*),           intent(in)  :: name   !< Dataset name (or path).
  real(R8P), allocatable, intent(out) :: x(:)   !< Values.
  integer(I4P)                        :: error  !< Error status.
  real(R8P), allocatable, target      :: buffer(:) !< Values read.
  integer(I8P)                        :: n      !< Number of values.

  error = dataset_size(loc_id, name, n)
  allocate(buffer(n))
#ifdef VTKFORTRAN_USE_HDF5
  if (error == 0) error = dataset_read(loc_id, name, H5T_NATIVE_DOUBLE_g, c_loc(buffer))
#endif
  call move_alloc(buffer, x)
  endfunction h5_dataset_read_R8P

  function h5_dataset_read_R4P(loc_id, name, x) result(error)
  !< Read all the values of a dataset as `R4P`, flattened in the order of its elements (the shape: `h5_dataset_dims`).
  integer(hid_kind),      intent(in)  :: loc_id !< Parent (file or group).
  character(*),           intent(in)  :: name   !< Dataset name (or path).
  real(R4P), allocatable, intent(out) :: x(:)   !< Values.
  integer(I4P)                        :: error  !< Error status.
  real(R4P), allocatable, target      :: buffer(:) !< Values read.
  integer(I8P)                        :: n      !< Number of values.

  error = dataset_size(loc_id, name, n)
  allocate(buffer(n))
#ifdef VTKFORTRAN_USE_HDF5
  if (error == 0) error = dataset_read(loc_id, name, H5T_NATIVE_FLOAT_g, c_loc(buffer))
#endif
  call move_alloc(buffer, x)
  endfunction h5_dataset_read_R4P

  function h5_dataset_read_I8P(loc_id, name, x) result(error)
  !< Read all the values of a dataset as `I8P`, flattened in the order of its elements (the shape: `h5_dataset_dims`).
  integer(hid_kind),         intent(in)  :: loc_id !< Parent (file or group).
  character(*),              intent(in)  :: name   !< Dataset name (or path).
  integer(I8P), allocatable, intent(out) :: x(:)   !< Values.
  integer(I4P)                           :: error  !< Error status.
  integer(I8P), allocatable, target      :: buffer(:) !< Values read.
  integer(I8P)                           :: n      !< Number of values.

  error = dataset_size(loc_id, name, n)
  allocate(buffer(n))
#ifdef VTKFORTRAN_USE_HDF5
  if (error == 0) error = dataset_read(loc_id, name, H5T_NATIVE_INT64_g, c_loc(buffer))
#endif
  call move_alloc(buffer, x)
  endfunction h5_dataset_read_I8P

  function h5_dataset_read_I4P(loc_id, name, x) result(error)
  !< Read all the values of a dataset as `I4P`, flattened in the order of its elements (the shape: `h5_dataset_dims`).
  integer(hid_kind),         intent(in)  :: loc_id !< Parent (file or group).
  character(*),              intent(in)  :: name   !< Dataset name (or path).
  integer(I4P), allocatable, intent(out) :: x(:)   !< Values.
  integer(I4P)                           :: error  !< Error status.
  integer(I4P), allocatable, target      :: buffer(:) !< Values read.
  integer(I8P)                           :: n      !< Number of values.

  error = dataset_size(loc_id, name, n)
  allocate(buffer(n))
#ifdef VTKFORTRAN_USE_HDF5
  if (error == 0) error = dataset_read(loc_id, name, H5T_NATIVE_INT32_g, c_loc(buffer))
#endif
  call move_alloc(buffer, x)
  endfunction h5_dataset_read_I4P

  function h5_dataset_read_I2P(loc_id, name, x) result(error)
  !< Read all the values of a dataset as `I2P`, flattened in the order of its elements (the shape: `h5_dataset_dims`).
  integer(hid_kind),         intent(in)  :: loc_id !< Parent (file or group).
  character(*),              intent(in)  :: name   !< Dataset name (or path).
  integer(I2P), allocatable, intent(out) :: x(:)   !< Values.
  integer(I4P)                           :: error  !< Error status.
  integer(I2P), allocatable, target      :: buffer(:) !< Values read.
  integer(I8P)                           :: n      !< Number of values.

  error = dataset_size(loc_id, name, n)
  allocate(buffer(n))
#ifdef VTKFORTRAN_USE_HDF5
  if (error == 0) error = dataset_read(loc_id, name, H5T_NATIVE_INT16_g, c_loc(buffer))
#endif
  call move_alloc(buffer, x)
  endfunction h5_dataset_read_I2P

  function h5_dataset_read_I1P(loc_id, name, x) result(error)
  !< Read all the values of a dataset as `I1P`, flattened in the order of its elements (the shape: `h5_dataset_dims`).
  integer(hid_kind),         intent(in)  :: loc_id !< Parent (file or group).
  character(*),              intent(in)  :: name   !< Dataset name (or path).
  integer(I1P), allocatable, intent(out) :: x(:)   !< Values.
  integer(I4P)                           :: error  !< Error status.
  integer(I1P), allocatable, target      :: buffer(:) !< Values read.
  integer(I8P)                           :: n      !< Number of values.

  error = dataset_size(loc_id, name, n)
  allocate(buffer(n))
#ifdef VTKFORTRAN_USE_HDF5
  if (error == 0) error = dataset_read(loc_id, name, H5T_NATIVE_INT8_g, c_loc(buffer))
#endif
  call move_alloc(buffer, x)
  endfunction h5_dataset_read_I1P

  ! private procedures
  pure function is_true(flag) result(is)
  !< Return the value of an optional logical, false when absent.
  logical, intent(in), optional :: flag !< Optional flag.
  logical                       :: is   !< Its value.

  is = .false.
  if (present(flag)) is = flag
  endfunction is_true

  pure function c_dims(dims) result(reversed)
  !< Return a shape in C order (HDF5) from the Fortran order.
  integer(I8P), intent(in) :: dims(:)               !< Shape, Fortran order.
  integer(c_long_long)     :: reversed(size(dims))  !< Shape, C order.
  integer                  :: d                     !< Counter.

  do d=1, size(dims)
    reversed(d) = int(dims(size(dims) - d + 1), c_long_long)
  enddo
  endfunction c_dims

  function dataset_size(loc_id, name, n) result(error)
  !< Return the number of values of a dataset.
  integer(hid_kind), intent(in)  :: loc_id  !< Parent (file or group).
  character(*),      intent(in)  :: name    !< Dataset name (or path).
  integer(I8P),      intent(out) :: n       !< Number of values.
  integer(I4P)                   :: error   !< Error status.
  integer(I8P), allocatable      :: dims(:) !< Shape.

  n = 0_I8P
  error = h5_dataset_dims(loc_id, name, dims)
  if (error == 0) n = product(dims)
  endfunction dataset_size

#ifdef VTKFORTRAN_USE_HDF5
  function attribute_write(loc_id, name, memtype, n, buffer) result(error)
  !< Write a rank-1 numeric attribute of `n` values.
  integer(hid_kind),  intent(in) :: loc_id  !< Object (file, group or dataset).
  character(*),       intent(in) :: name    !< Attribute name.
  integer(c_int64_t), intent(in) :: memtype !< Type of the values in memory (and in the file).
  integer(I8P),       intent(in) :: n       !< Number of values.
  type(c_ptr),        intent(in) :: buffer  !< Values.
  integer(I4P)                   :: error   !< Error status.
  integer(c_int64_t)             :: space   !< Dataspace.
  integer(c_int64_t)             :: attr    !< Attribute.

  error = 1_I4P
  if (H5Aexists(loc_id, trim(name)//c_null_char) > 0) then ! overwritten
    if (H5Adelete(loc_id, trim(name)//c_null_char) < 0) return
  endif
  space = H5Screate_simple(1_c_int, [int(n, c_long_long)], c_null_ptr)
  if (space < 0) return
  attr = H5Acreate2(loc_id, trim(name)//c_null_char, memtype, space, H5P_DEFAULT, H5P_DEFAULT)
  if (attr >= 0) then
    if (H5Awrite(attr, memtype, buffer) >= 0) error = 0_I4P
    if (H5Aclose(attr) < 0) error = 1_I4P
  endif
  if (H5Sclose(space) < 0) error = 1_I4P
  endfunction attribute_write

  function attribute_size(attr) result(n)
  !< Return the number of values of an attribute.
  integer(c_int64_t), intent(in) :: attr  !< Attribute.
  integer(I8P)                   :: n     !< Number of values.
  integer(c_int64_t)             :: space !< Dataspace.
  integer(c_int)                 :: rank  !< Rank.
  integer(c_long_long)           :: dims(32) !< Shape.

  n = 0_I8P
  space = H5Aget_space(attr)
  if (space < 0) return
  rank = H5Sget_simple_extent_ndims(space)
  if (rank == 0) then
    n = 1_I8P
  elseif (rank > 0) then
    if (H5Sget_simple_extent_dims(space, dims, c_null_ptr) >= 0) n = product(int(dims(1:rank), I8P))
  endif
  if (H5Sclose(space) < 0) n = 0_I8P
  endfunction attribute_size

  function dataset_dims(dset, dims) result(error)
  !< Return the shape of an open dataset, Fortran order.
  integer(c_int64_t),        intent(in)  :: dset     !< Dataset.
  integer(I8P), allocatable, intent(out) :: dims(:)  !< Shape, Fortran order.
  integer(I4P)                           :: error    !< Error status.
  integer(c_int64_t)                     :: space    !< Dataspace.
  integer(c_int)                         :: rank     !< Rank.
  integer(c_long_long)                   :: cdims(32) !< Shape, C order.
  integer                                :: d        !< Counter.

  error = 1_I4P
  space = H5Dget_space(dset)
  if (space < 0) return
  rank = H5Sget_simple_extent_ndims(space)
  if (rank >= 0) then
    if (H5Sget_simple_extent_dims(space, cdims, c_null_ptr) >= 0) then
      allocate(dims(rank))
      do d=1, rank
        dims(d) = int(cdims(rank - d + 1), I8P)
      enddo
      error = 0_I4P
    endif
  endif
  if (H5Sclose(space) < 0) error = 1_I4P
  endfunction dataset_dims

  function dataset_write(loc_id, name, memtype, bytes, dims, buffer, appendable, deflate, comm, chunk_bytes) result(error)
  !< Create a dataset of the type `memtype` (stored as it is), shape `dims` (Fortran order), and write its values.
  !<
  !< With `comm` (an appendable dataset of a parallel file) the ranks create the dataset empty, its chunks sized for the
  !< rows of all of them, then each one appends its rows after the ranks before (collective).
  !<
  !< A chunked dataset (appendable or deflated) is chunked by whole rows (its last Fortran dimension): as many as fit in
  !< `chunk_bytes` (default `DEFAULT_CHUNK_BYTES`), at least one, at most the rows written now (all the ranks' with `comm`).
  !< With `comm` the largest `chunk_bytes` of the ranks is used: the chunk shape is the same on every rank.
  integer(hid_kind),  intent(in)           :: loc_id     !< Parent (file or group).
  character(*),       intent(in)           :: name       !< Dataset name.
  integer(c_int64_t), intent(in)           :: memtype    !< Type of the values in memory (and in the file).
  integer(I8P),       intent(in)           :: bytes      !< Bytes of a value.
  integer(I8P),       intent(in)           :: dims(:)    !< Shape, Fortran order.
  type(c_ptr),        intent(in)           :: buffer     !< Values.
  logical,            intent(in), optional :: appendable !< Chunked, unlimited along the last Fortran dimension.
  integer(I4P),       intent(in), optional :: deflate    !< Deflate level (1-9), implies chunking.
  integer(I4P),       intent(in), optional :: comm       !< MPI communicator (Fortran handle).
  integer(I8P),       intent(in), optional :: chunk_bytes !< Bytes of a chunk at most (default 1 MiB), at least 1.
  integer(I4P)                             :: error      !< Error status.
  integer(I8P)                             :: cbytes     !< Bytes of a chunk at most.
  integer(I8P)                             :: first      !< Rows of the ranks before this one.
  integer(I8P)                             :: total      !< Rows of all the ranks.
  integer(I8P)                             :: fbytes     !< Bytes of a value in the file.
  integer(c_int64_t)                       :: ftype      !< Type of the values in the file.
  integer(c_long_long)                     :: cdims(size(dims)) !< Shape, C order.
  integer(c_long_long), target             :: maxdims(size(dims)) !< Maximum shape, C order.
  integer(c_long_long)                     :: chunk(size(dims)) !< Chunk shape, C order.
  integer(c_int64_t)                       :: space      !< Dataspace.
  integer(c_int64_t)                       :: dcpl       !< Dataset creation properties.
  integer(c_int64_t)                       :: dset       !< Dataset.
  logical                                  :: chunked    !< The dataset is chunked.
  logical                                  :: ok         !< The creation properties are set.
  integer(I4P)                             :: level      !< Deflate level, 0 for none.

  error = 1_I4P
  if (size(dims) < 1 .or. bytes < 1) return
  cbytes = DEFAULT_CHUNK_BYTES
  if (present(chunk_bytes)) cbytes = chunk_bytes
  cdims = c_dims(dims)
  total = dims(size(dims))
  ftype = memtype
  fbytes = bytes
  if (present(comm)) then ! created empty by every rank, its chunks sized for the rows of all, of the widest kind given
    if (.not.is_true(appendable)) return
    if (mpi_rows(comm, dims(size(dims)), first, total) /= 0_I4P) return
    cbytes = mpi_maximum(comm, cbytes) ! one chunk shape on every rank (the dataset is created collectively)
    cdims(1) = 0_c_long_long
    fbytes = mpi_maximum(comm, bytes)
    ftype = widest_type(memtype, fbytes)
  endif
  if (cbytes < 1_I8P) return ! with comm, the same on every rank
  level = 0_I4P ; if (present(deflate)) level = max(0_I4P, min(9_I4P, deflate))
  chunked = is_true(appendable) .or. level > 0
  maxdims = cdims
  if (is_true(appendable)) maxdims(1) = H5S_UNLIMITED
  if (is_true(appendable)) then
    space = H5Screate_simple(int(size(dims), c_int), cdims, c_loc(maxdims))
  else
    space = H5Screate_simple(int(size(dims), c_int), cdims, c_null_ptr)
  endif
  if (space < 0) return
  dcpl = H5P_DEFAULT
  ok = .true.
  if (chunked) then
    dcpl = H5Pcreate(H5P_CLS_DATASET_CREATE_ID_g)
    chunk = max(1_c_long_long, cdims)
    ! whole rows (slowest dimension) up to cbytes: a rank-3 grid is chunked by planes, not as one chunk
    chunk(1) = max(1_c_long_long, min(int(total, c_long_long), int(cbytes / (fbytes * product(chunk(2:))), c_long_long)))
    ok = dcpl >= 0
    if (ok) ok = H5Pset_chunk(dcpl, int(size(dims), c_int), chunk) >= 0
    if (ok .and. level > 0) ok = H5Pset_deflate(dcpl, int(level, c_int)) >= 0
  endif
  dset = -1
  if (ok) dset = H5Dcreate2(loc_id, trim(name)//c_null_char, ftype, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)
  if (dset >= 0) then
    if (product(dims) == 0_I8P .or. present(comm)) then
      error = 0_I4P
    elseif (H5Dwrite(dset, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer) >= 0) then
      error = 0_I4P
    endif
    if (H5Dclose(dset) < 0) error = 1_I4P
  endif
  if (dcpl > 0) then
    if (H5Pclose(dcpl) < 0) error = 1_I4P
  endif
  if (H5Sclose(space) < 0) error = 1_I4P
  if (present(comm) .and. error == 0_I4P) error = dataset_append(loc_id, name, memtype, dims, buffer, comm)
  endfunction dataset_write

  function widest_type(memtype, bytes) result(ftype)
  !< The type of the class and sign of `memtype` (real, signed or unsigned integer) with `bytes` bytes, or `memtype`.
  !<
  !< The type of a dataset created by the ranks of a communicator: the ranks may give the same values with different kinds
  !< (the ids of an empty PolyData block are I8P), the dataset gets the widest one.
  integer(c_int64_t), intent(in) :: memtype !< Type of the values in memory.
  integer(I8P),       intent(in) :: bytes   !< Bytes of a value in the file.
  integer(c_int64_t)             :: ftype   !< Type of the values in the file.
  integer(c_int64_t)             :: reals(4), signed(4), unsigned_(4) !< Types by bytes: 1, 2, 4, 8.
  integer                        :: w       !< Index of the width.

  ftype = memtype
  reals = [-1_c_int64_t, -1_c_int64_t, H5T_NATIVE_FLOAT_g, H5T_NATIVE_DOUBLE_g]
  signed = [H5T_NATIVE_INT8_g, H5T_NATIVE_INT16_g, H5T_NATIVE_INT32_g, H5T_NATIVE_INT64_g]
  unsigned_ = [H5T_NATIVE_UINT8_g, H5T_NATIVE_UINT16_g, H5T_NATIVE_UINT32_g, H5T_NATIVE_UINT64_g]
  select case(bytes)
  case(1_I8P) ; w = 1
  case(2_I8P) ; w = 2
  case(4_I8P) ; w = 3
  case(8_I8P) ; w = 4
  case default ; return
  endselect
  if (any(reals == memtype)) then
    if (reals(w) >= 0_c_int64_t) ftype = reals(w)
  elseif (any(signed == memtype)) then
    ftype = signed(w)
  elseif (any(unsigned_ == memtype)) then
    ftype = unsigned_(w)
  endif
  endfunction widest_type

  function mpi_maximum(comm, n) result(nmax)
  !< The maximum of `n` over the ranks of `comm` (a Fortran MPI communicator); `n` without MPI.
  integer(I4P), intent(in) :: comm  !< MPI communicator (Fortran handle).
  integer(I8P), intent(in) :: n     !< Value of the rank.
  integer(I8P)             :: nmax  !< Maximum over the ranks.
#ifdef VTKFORTRAN_USE_MPI
  type(MPI_Comm)           :: c     !< Communicator.

  c%MPI_VAL = comm
  call MPI_Allreduce(n, nmax, 1, MPI_INTEGER8, MPI_MAX, c)
#else
  nmax = n + 0_I8P * comm
#endif
  endfunction mpi_maximum

  function mpi_rows(comm, n, first, total) result(error)
  !< The rows appended by the ranks of `comm` before this one (`first`) and by all of them (`total`), this one appending `n`.
  integer(I4P), intent(in)  :: comm   !< MPI communicator (Fortran handle).
  integer(I8P), intent(in)  :: n      !< Rows of this rank.
  integer(I8P), intent(out) :: first  !< Rows of the ranks before.
  integer(I8P), intent(out) :: total  !< Rows of all the ranks.
  integer(I4P)              :: error  !< Error status.
#ifdef VTKFORTRAN_USE_MPI
  type(MPI_Comm)            :: c      !< Communicator.
  integer                   :: rank   !< Rank.
  integer                   :: size_  !< Ranks.
  integer(I8P), allocatable :: rows(:) !< Rows of each rank.

  c%MPI_VAL = comm
  call MPI_Comm_rank(c, rank)
  call MPI_Comm_size(c, size_)
  allocate(rows(0:size_-1))
  call MPI_Allgather(n, 1, MPI_INTEGER8, rows, 1, MPI_INTEGER8, c)
  first = sum(rows(0:rank-1))
  total = sum(rows)
  error = 0_I4P
#else
  first = 0_I8P ; total = n + 0_I8P * comm
  error = 1_I4P
#endif
  endfunction mpi_rows

  function dataset_append(loc_id, name, memtype, dims, buffer, comm) result(error)
  !< Append values (shape `dims`, Fortran order) to an appendable dataset, along its last Fortran dimension; the other
  !< dimensions must match the dataset's.
  integer(hid_kind),  intent(in) :: loc_id    !< Parent (file or group).
  character(*),       intent(in) :: name      !< Dataset name.
  integer(c_int64_t), intent(in) :: memtype   !< Type of the values in memory.
  integer(I8P),       intent(in) :: dims(:)   !< Shape of the values appended, Fortran order.
  type(c_ptr),        intent(in) :: buffer    !< Values appended.
  integer(I4P), intent(in), optional :: comm  !< MPI communicator: the ranks append their values in rank order (collective).
  integer(I4P)                   :: error     !< Error status.
  integer(I8P)                   :: first     !< Rows of the ranks before this one.
  integer(I8P)                   :: total     !< Rows of all the ranks.
  integer(c_int64_t)             :: dset      !< Dataset.
  integer(c_int64_t)             :: filespace !< Dataspace of the dataset, extended.
  integer(c_int64_t)             :: memspace  !< Dataspace of the values appended.
  integer(I8P), allocatable      :: old(:)    !< Current shape, Fortran order.
  integer(c_long_long)           :: cnew(size(dims))   !< New shape, C order.
  integer(c_long_long)           :: cstart(size(dims)) !< Start of the appended values, C order.
  integer(c_long_long)           :: ccount(size(dims)) !< Shape of the appended values, C order.
  integer(c_int64_t)             :: dxpl      !< Data transfer properties (collective with comm).
  integer(c_int8_t), target      :: none(1)   !< A buffer for a rank without rows (nothing is read from it).
  integer(c_int64_t)             :: ftype     !< Type of the dataset (with comm).
  integer(c_size_t)              :: fsize, msize !< Bytes of a value in the file, in memory.
  integer(c_int8_t), allocatable, target :: converted(:) !< The values converted to the type of the dataset.
  integer(c_int8_t), pointer     :: bytes(:)  !< The values, as bytes.

  error = 1_I4P
  first = 0_I8P
  total = dims(size(dims))
  if (present(comm)) then ! collective: every rank extends by the rows of all, and writes its own after the ranks before
    error = mpi_rows(comm, dims(size(dims)), first, total)
    if (error /= 0_I4P) return
    error = 1_I4P
  elseif (product(dims) == 0_I8P) then
    error = 0_I4P
    return
  endif
  dset = H5Dopen2(loc_id, trim(name)//c_null_char, H5P_DEFAULT)
  if (dset < 0) return
  if (dataset_dims(dset, old) /= 0) allocate(old(0)) ! no short-circuit in Fortran: old is checked apart
  if (size(old) == size(dims)) then
    if (all(old(1:size(dims)-1) == dims(1:size(dims)-1))) then
      ccount = c_dims(dims)
      cnew = c_dims(old)
      cstart = 0_c_long_long
      cstart(1) = cnew(1) + int(first, c_long_long)
      cnew(1) = cnew(1) + int(total, c_long_long)
      if (H5Dset_extent(dset, cnew) < 0) then
        continue
      elseif (present(comm)) then ! collective: every rank writes (a deflated dataset needs it), its rows or none
        dxpl = -1_c_int64_t
#ifdef VTKFORTRAN_USE_MPI
        dxpl = vtkfortran_h5_dxpl_mpio_collective()
#endif
        filespace = H5Dget_space(dset)
        memspace = H5Screate_simple(int(size(dims), c_int), max(ccount, 1_c_long_long), c_null_ptr)
        ! the values written in the type of the dataset (the widest kind of the ranks), converted here when they are of
        ! another: a collective write with a type conversion, even of an empty selection, falls back to independent
        ! writes, which a deflated dataset refuses
        ftype = H5Dget_type(dset)
        if (filespace >= 0 .and. memspace >= 0 .and. dxpl >= 0 .and. ftype >= 0) then
          if (ccount(1) == 0_c_long_long) then ! a rank with no rows: it extends with the others, selects nothing
            if (H5Sselect_none(filespace) >= 0 .and. H5Sselect_none(memspace) >= 0) then
              if (H5Dwrite(dset, ftype, memspace, filespace, dxpl, c_loc(none)) >= 0) error = 0_I4P
            endif
          elseif (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, cstart, c_null_ptr, ccount, c_null_ptr) >= 0) then
            if (H5Tequal(memtype, ftype) > 0) then
              if (H5Dwrite(dset, memtype, memspace, filespace, dxpl, buffer) >= 0) error = 0_I4P
            else
              fsize = H5Tget_size(ftype) ; msize = H5Tget_size(memtype)
              allocate(converted(max(fsize, msize) * product(ccount)))
              call c_f_pointer(buffer, bytes, [msize * product(ccount)])
              converted(1:msize * product(ccount)) = bytes
              if (H5Tconvert(memtype, ftype, int(product(ccount), c_size_t), c_loc(converted), c_null_ptr, &
                             H5P_DEFAULT) >= 0) then
                if (H5Dwrite(dset, ftype, memspace, filespace, dxpl, c_loc(converted)) >= 0) error = 0_I4P
              endif
            endif
          endif
        endif
        if (ftype >= 0) then
          if (H5Tclose(ftype) < 0) error = 1_I4P
        endif
        if (memspace >= 0) then
          if (H5Sclose(memspace) < 0) error = 1_I4P
        endif
        if (filespace >= 0) then
          if (H5Sclose(filespace) < 0) error = 1_I4P
        endif
        if (dxpl >= 0) then
          if (H5Pclose(dxpl) < 0) error = 1_I4P
        endif
      elseif (ccount(1) == 0_c_long_long) then ! nothing to append
        error = 0_I4P
      else
        filespace = H5Dget_space(dset)
        memspace = H5Screate_simple(int(size(dims), c_int), ccount, c_null_ptr)
        if (filespace >= 0 .and. memspace >= 0) then
          if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, cstart, c_null_ptr, ccount, c_null_ptr) >= 0) then
            if (H5Dwrite(dset, memtype, memspace, filespace, H5P_DEFAULT, buffer) >= 0) error = 0_I4P
          endif
        endif
        if (memspace >= 0) then
          if (H5Sclose(memspace) < 0) error = 1_I4P
        endif
        if (filespace >= 0) then
          if (H5Sclose(filespace) < 0) error = 1_I4P
        endif
      endif
    endif
  endif
  if (H5Dclose(dset) < 0) error = 1_I4P
  endfunction dataset_append

  function dataset_slab(loc_id, name, memtype, dims, mem_dims, mem_start, file_start, count, buffer, create, append, &
                        chunk_bytes) result(error)
  !< Write the block `count` of the values (shape `mem_dims`, the block at `mem_start`) into the dataset `name` at
  !< `file_start` (Fortran order, from 0), creating it first (shape `dims`, type `memtype`) with `create`.
  !<
  !< With `append` the dataset grows along its last Fortran dimension (a time series written by the ranks of a parallel
  !< file): created (`create`) appendable, chunked as `dataset_write` chunks it (whole rows up to `chunk_bytes`, at most
  !< `dims(last)`), or else extended by `dims(last)` rows (collective in a parallel file), the block written after the
  !< rows it had (`file_start(last)` counted from them).
  integer(hid_kind),  intent(in) :: loc_id        !< Parent (file or group).
  character(*),       intent(in) :: name          !< Dataset name.
  integer(c_int64_t), intent(in) :: memtype       !< Type of the values in memory (and in the file).
  integer(I8P),       intent(in) :: dims(:)       !< Shape of the dataset, Fortran order.
  integer(I8P),       intent(in) :: mem_dims(:)   !< Shape of the values, Fortran order.
  integer(I8P),       intent(in) :: mem_start(:)  !< Start of the block in the values.
  integer(I8P),       intent(in) :: file_start(:) !< Start of the block in the dataset.
  integer(I8P),       intent(in) :: count(:)      !< Shape of the block.
  type(c_ptr),        intent(in) :: buffer        !< Values.
  logical,            intent(in) :: create        !< Create the dataset first.
  logical,  optional, intent(in) :: append        !< Grow the dataset along its last dimension.
  integer(I8P), optional, intent(in) :: chunk_bytes !< Bytes of a chunk at most (default 1 MiB), with append.
  integer(I4P)                   :: error         !< Error status.
  integer(c_int64_t)             :: dset          !< Dataset.
  integer(c_int64_t)             :: space         !< Dataspace of the dataset.
  integer(c_int64_t)             :: memspace      !< Dataspace of the values.
  integer(c_int64_t)             :: dcpl          !< Dataset creation properties.
  integer(c_long_long)           :: cdims(size(dims)) !< Shape, C order.
  integer(c_long_long), target   :: maxdims(size(dims)) !< Maximum shape, C order.
  integer(c_long_long)           :: chunk(size(dims)) !< Chunk shape, C order.
  integer(I8P), allocatable      :: old(:)        !< Shape before the extension, Fortran order.
  integer(I8P)                   :: start(size(dims)) !< Start of the block in the dataset.
  integer(I8P)                   :: cbytes        !< Bytes of a chunk at most.
  logical                        :: is_append     !< Grow the dataset.

  error = 1_I4P
  if (size(dims) < 1 .or. size(mem_dims) /= size(dims) .or. size(mem_start) /= size(dims) .or. &
      size(file_start) /= size(dims) .or. size(count) /= size(dims)) return
  is_append = is_true(append)
  cbytes = DEFAULT_CHUNK_BYTES ; if (present(chunk_bytes)) cbytes = chunk_bytes
  start = file_start
  if (create .and. is_append) then ! appendable, chunked by whole rows
    if (cbytes < 1_I8P) return
    cdims = c_dims(dims)
    maxdims = cdims ; maxdims(1) = H5S_UNLIMITED
    chunk = max(1_c_long_long, cdims)
    chunk(1) = max(1_c_long_long, min(cdims(1), int(cbytes / (int(H5Tget_size(memtype), I8P) * product(chunk(2:))), &
                                                    c_long_long)))
    space = H5Screate_simple(int(size(dims), c_int), cdims, c_loc(maxdims))
    if (space < 0) return
    dcpl = H5Pcreate(H5P_CLS_DATASET_CREATE_ID_g)
    dset = -1
    if (dcpl >= 0) then
      if (H5Pset_chunk(dcpl, int(size(dims), c_int), chunk) >= 0) &
        dset = H5Dcreate2(loc_id, trim(name)//c_null_char, memtype, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)
      if (H5Pclose(dcpl) < 0) dset = -1
    endif
    if (H5Sclose(space) < 0) dset = -1
  elseif (create) then
    space = H5Screate_simple(int(size(dims), c_int), c_dims(dims), c_null_ptr)
    if (space < 0) return
    dset = H5Dcreate2(loc_id, trim(name)//c_null_char, memtype, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)
    if (H5Sclose(space) < 0) dset = -1
  else
    dset = H5Dopen2(loc_id, trim(name)//c_null_char, H5P_DEFAULT)
    if (dset >= 0 .and. is_append) then ! extended by dims(last) rows, the block after the rows it had
      if (dataset_dims(dset, old) /= 0_I4P) allocate(old(0))
      if (size(old) /= size(dims)) then
        if (H5Dclose(dset) < 0) error = 1_I4P
        return
      endif
      start(size(dims)) = start(size(dims)) + old(size(dims))
      old(size(dims)) = old(size(dims)) + dims(size(dims))
      if (H5Dset_extent(dset, c_dims(old)) < 0) then
        if (H5Dclose(dset) < 0) error = 1_I4P
        return
      endif
    endif
  endif
  if (dset < 0) return
  if (product(count) == 0_I8P) then ! an empty block: nothing to write
    error = 0_I4P
  else
    space = H5Dget_space(dset)
    memspace = H5Screate_simple(int(size(dims), c_int), c_dims(mem_dims), c_null_ptr)
    if (space >= 0 .and. memspace >= 0) then
      if (H5Sselect_hyperslab(space, H5S_SELECT_SET, c_dims(start), c_null_ptr, c_dims(count), c_null_ptr) >= 0 .and. &
          H5Sselect_hyperslab(memspace, H5S_SELECT_SET, c_dims(mem_start), c_null_ptr, c_dims(count), c_null_ptr) >= 0) then
        if (H5Dwrite(dset, memtype, memspace, space, H5P_DEFAULT, buffer) >= 0) error = 0_I4P
      endif
    endif
    if (memspace >= 0) then
      if (H5Sclose(memspace) < 0) error = 1_I4P
    endif
    if (space >= 0) then
      if (H5Sclose(space) < 0) error = 1_I4P
    endif
  endif
  if (H5Dclose(dset) < 0) error = 1_I4P
  endfunction dataset_slab

  function dataset_read(loc_id, name, memtype, buffer) result(error)
  !< Read all the values of a dataset, converted to the type `memtype`.
  integer(hid_kind),  intent(in) :: loc_id  !< Parent (file or group).
  character(*),       intent(in) :: name    !< Dataset name (or path).
  integer(c_int64_t), intent(in) :: memtype !< Type of the values in memory.
  type(c_ptr),        intent(in) :: buffer  !< Values read.
  integer(I4P)                   :: error   !< Error status.
  integer(c_int64_t)             :: dset    !< Dataset.

  error = 1_I4P
  dset = H5Dopen2(loc_id, trim(name)//c_null_char, H5P_DEFAULT)
  if (dset < 0) return
  if (H5Dread(dset, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer) >= 0) error = 0_I4P
  if (H5Dclose(dset) < 0) error = 1_I4P
  endfunction dataset_read
#endif
endmodule vtk_fortran_hdf5
