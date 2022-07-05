!< KISS library for packing heterogeneous data into single (homogeneous) packed one.
!
module befor64_pack_data_m
!< KISS library for packing heterogeneous data into single (homogeneous) packed one.
use penf

implicit none
private
public :: pack_data

interface pack_data
  !< Pack different kinds of data into single I1P array.
  !<
  !< This is useful for encoding different (heterogeneous) kinds variables into a single (homogeneous) stream of bits.
  !< @note This procedure exploits the `transfer` builtin function, that from the standard (2003+) is defined as
  !< `TRANSFER(SOURCE, MOLD [, SIZE])`. Data object having a physical representation identical to that of `SOURCE` but with the type
  !< and type parameters of `MOLD`. The result is of the same type and type parameters as `MOLD`.
  !< If `MOLD` is an array and `SIZE` is absent, the result is an array and of rank one. Its size is as small as possible such
  !< that its physical representation is not shorter than that of `SOURCE`.
  !<
  !< Presently, the following combinations are available:
  !<
  !<* [ ] Arrays-Arrays:
  !<    * [X] real(any)-real(any);
  !<    * [X] real(any)-integer(any);
  !<    * [X] integer(any)-integer(any);
  !<    * [X] integer(any)-real(any);
  !<    * [ ] real(any)-character;
  !<    * [ ] character-real(any);
  !<    * [ ] integer(any)-character;
  !<    * [ ] character-integer(any);
  !<* [ ] Scalars-Scalars:
  !<    * [ ] real(any)-real(any);
  !<    * [ ] real(any)-integer(any);
  !<    * [ ] integer(any)-integer(any);
  !<    * [ ] integer(any)-real(any);
  !<    * [ ] real(any)-character;
  !<    * [ ] character-real(any);
  !<    * [ ] integer(any)-character;
  !<    * [ ] character-integer(any);
  !<
  !<### Examples of usage
  !<
  !<#### Packing two real arrays, one with kind R8P and one with R4P
  !<```ortran
  !<real(R8P)::                 array_r8(1:12)
  !<real(R4P)::                 array_r4(-1:5)
  !<integer(I1P), allocatable:: rpack
  !<...
  !<call pack_data(a1=array_r8,a2=array_r4,packed=rpack)
  !<```
  !<#### Packing two arrays, one real with kind R4P and one integer with I4P
  !<```ortran
  !<real(R4P)::                 array_r4(2)
  !<integer(I4P)::              array_i4(0:2)
  !<integer(I1P), allocatable:: rpack
  !<...
  !<call pack_data(a1=array_r4,a2=array_i4,packed=rpack)
  !<```
  module procedure pack_data_R8_R4, pack_data_R8_I8, pack_data_R8_I4, pack_data_R8_I2, pack_data_R8_I1, &
                   pack_data_R4_R8, pack_data_R4_I8, pack_data_R4_I4, pack_data_R4_I2, pack_data_R4_I1, &
                   pack_data_I8_R8, pack_data_I8_R4, pack_data_I8_I4, pack_data_I8_I2, pack_data_I8_I1, &
                   pack_data_I4_R8, pack_data_I4_R4, pack_data_I4_I8, pack_data_I4_I2, pack_data_I4_I1, &
                   pack_data_I2_R8, pack_data_I2_R4, pack_data_I2_I8, pack_data_I2_I4, pack_data_I2_I1, &
                   pack_data_I1_R8, pack_data_I1_R4, pack_data_I1_I8, pack_data_I1_I4, pack_data_I1_I2
endinterface

contains
   pure subroutine pack_data_R8_R4(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R8P)                 :: a1(1)
   !< real(R4P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   real(R8P),                 intent(in)    :: a1(1:)    !< Firs data stream.
   real(R4P),                 intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_R8_R4

   pure subroutine pack_data_R8_I8(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R8P)                 :: a1(1)
   !< integer(I8P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   real(R8P),                 intent(in)    :: a1(1:)    !< First data stream.
   integer(I8P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_R8_I8

   pure subroutine pack_data_R8_I4(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R8P)                 :: a1(1)
   !< integer(I4P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   real(R8P),                 intent(in)    :: a1(1:)    !< First data stream.
   integer(I4P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_R8_I4

   pure subroutine pack_data_R8_I2(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R8P)                 :: a1(1)
   !< integer(I2P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   real(R8P),                 intent(in)    :: a1(1:)    !< First data stream.
   integer(I2P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_R8_I2

   pure subroutine pack_data_R8_I1(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R8P)                 :: a1(1)
   !< integer(I1P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   real(R8P),                 intent(in)    :: a1(1:)    !< First data stream.
   integer(I1P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_R8_I1

   pure subroutine pack_data_R4_R8(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R4P)                 :: a1(1)
   !< real(R8P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   real(R4P),                 intent(in)    :: a1(1:)    !< Firs data stream.
   real(R8P),                 intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_R4_R8

   pure subroutine pack_data_R4_I8(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R4P)                 :: a1(1)
   !< integer(I8P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   real(R4P),                 intent(in)    :: a1(1:)    !< First data stream.
   integer(I8P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_R4_I8

   pure subroutine pack_data_R4_I4(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R4P)                 :: a1(1)
   !< integer(I4P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   real(R4P),                 intent(in)    :: a1(1:)    !< First data stream.
   integer(I4P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_R4_I4

   pure subroutine pack_data_R4_I2(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R4P)                 :: a1(1)
   !< integer(I2P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   real(R4P),                 intent(in)    :: a1(1:)    !< First data stream.
   integer(I2P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_R4_I2

   pure subroutine pack_data_R4_I1(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< real(R4P)                 :: a1(1)
   !< integer(I1P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   real(R4P),                 intent(in)    :: a1(1:)    !< First data stream.
   integer(I1P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_R4_I1

   pure subroutine pack_data_I8_R8(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P)              :: a1(1)
   !< real(R8P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   integer(I8P),              intent(in)    :: a1(1:)    !< First data stream.
   real(R8P),                 intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I8_R8

   pure subroutine pack_data_I8_R4(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P)              :: a1(1)
   !< real(R4P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   integer(I8P),              intent(in)    :: a1(1:)    !< First data stream.
   real(R4P),                 intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I8_R4

   pure subroutine pack_data_I8_I4(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P)              :: a1(1)
   !< integer(I4P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   integer(I8P),              intent(in)    :: a1(1:)    !< First data stream.
   integer(I4P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I8_I4

   pure subroutine pack_data_I8_I2(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P)              :: a1(1)
   !< integer(I2P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   integer(I8P),              intent(in)    :: a1(1:)    !< First data stream.
   integer(I2P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I8_I2

   pure subroutine pack_data_I8_I1(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P)              :: a1(1)
   !< integer(I1P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(9)
   !<```
   !=> 1 <<<
   integer(I8P),              intent(in)    :: a1(1:)    !< First data stream.
   integer(I1P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I8_I1

   pure subroutine pack_data_I4_R8(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I4P)              :: a1(1)
   !< real(R8P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   integer(I4P),              intent(in)    :: a1(1:)    !< First data stream.
   real(R8P),                 intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I4_R8

   pure subroutine pack_data_I4_R4(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I4P)              :: a1(1)
   !< real(R4P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   integer(I4P),              intent(in)    :: a1(1:)    !< First data stream.
   real(R4P),                 intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I4_R4

   pure subroutine pack_data_I4_I8(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I4P)              :: a1(1)
   !< integer(I8P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   integer(I4P),              intent(in)    :: a1(1:)    !< First data stream.
   integer(I8P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I4_I8

   pure subroutine pack_data_I4_I2(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I4P)              :: a1(1)
   !< integer(I2P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   integer(I4P),              intent(in)    :: a1(1:)    !< First data stream.
   integer(I2P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I4_I2

   pure subroutine pack_data_I4_I1(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I4P)              :: a1(1)
   !< integer(I1P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(5)
   !<```
   !=> 1 <<<
   integer(I4P),              intent(in)    :: a1(1:)    !< First data stream.
   integer(I1P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I4_I1

   pure subroutine pack_data_I2_R8(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I2P)              :: a1(1)
   !< real(R8P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   integer(I2P),              intent(in)    :: a1(1:)    !< First data stream.
   real(R8P),                 intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I2_R8

   pure subroutine pack_data_I2_R4(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I2P)              :: a1(1)
   !< real(R4P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   integer(I2P),              intent(in)    :: a1(1:)    !< First data stream.
   real(R4P),                 intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I2_R4

   pure subroutine pack_data_I2_I8(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I2P)              :: a1(1)
   !< integer(I8P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(3)
   !<```
   !=> 1 <<<
   integer(I2P),              intent(in)    :: a1(1:)    !< First data stream.
   integer(I8P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I2_I8

   pure subroutine pack_data_I2_I4(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I2P)              :: a1(1)
   !< integer(I4P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(3)
   !<```
   !=> 1 <<<
   integer(I2P),              intent(in)    :: a1(1:)    !< First data stream.
   integer(I4P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I2_I4

   pure subroutine pack_data_I2_I1(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I2P)              :: a1(1)
   !< integer(I1P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(3)
   !<```
   !=> 1 <<<
   integer(I2P),              intent(in)    :: a1(1:)    !< First data stream.
   integer(I1P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I2_I1

   pure subroutine pack_data_I1_R8(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I1P)              :: a1(1)
   !< real(R8P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   integer(I1P),              intent(in)    :: a1(1:)    !< First data stream.
   real(R8P),                 intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I1_R8

   pure subroutine pack_data_I1_R4(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I1P)              :: a1(1)
   !< real(R4P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   integer(I1P),              intent(in)    :: a1(1:)    !< First data stream.
   real(R4P),                 intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I1_R4

   pure subroutine pack_data_I1_I8(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I1P)              :: a1(1)
   !< integer(I8P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(2)
   !<```
   !=> 1 <<<
   integer(I1P),              intent(in)    :: a1(1:)    !< First data stream.
   integer(I8P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I1_I8

   pure subroutine pack_data_I1_I4(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I1P)              :: a1(1)
   !< integer(I4P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(2)
   !<```
   !=> 1 <<<
   integer(I1P),              intent(in)    :: a1(1:)    !< First data stream.
   integer(I4P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I1_I4

   pure subroutine pack_data_I1_I2(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I1P)              :: a1(1)
   !< integer(I2P)              :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(2)
   !<```
   !=> 1 <<<
   integer(I1P),              intent(in)    :: a1(1:)    !< First data stream.
   integer(I2P),              intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.

   p1 = transfer(a1,p1)
   p2 = transfer(a2,p2)
   packed = [p1,p2]
   endsubroutine pack_data_I1_I2
endmodule befor64_pack_data_m
