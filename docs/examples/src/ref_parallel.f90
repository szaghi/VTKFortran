!run ref_parallel ref_parallel
!run ref_parallel-series cat ref_series.vtm
!run ref_parallel-pvd cat ref_simulation.pvd
program ref_parallel
!< The examples of the reference page "Parallel and composite files": the five parallel headers with their pieces, multi-block
!< files, a time series, a volatile file. Every header is then read back and checked against its pieces.
use penf, only : I1P, I4P, R8P
use vtk_fortran, only : pvtk_file, vtk_file
implicit none

call pvts
call pvtr
call pvtu
call pvti
call pvtp
call vtm
call vtm_nested
call vtm_scratch
call pvd
call volatile
call check([character(16) :: 'ref.pvts', 'ref.pvtr', 'ref.pvtu', 'ref.pvti', 'ref.pvtp'])
contains
  subroutine pvts
  !region pvts
  use vtk_fortran, only : pvtk_file
  use penf,        only : I4P

  type(pvtk_file)         :: a_pvtk_file
  integer(I4P), parameter :: nx1=0, nx2=9, ny1=0, ny2=5, nz1=0, nz2=5 ! whole extent
  integer(I4P), parameter :: nx1_p(2) = [0, 4]                        ! extents of the pieces along x:
  integer(I4P), parameter :: nx2_p(2) = [4, 9]                        ! adjacent pieces share the plane i=4
  integer(I4P)            :: error

  ! --- each rank writes its own piece: a complete .vts file (a few lines of the program, see the cookbook) ---
  call write_piece(filename='ref_part_1.vts', i1=nx1_p(1), i2=nx2_p(1))
  call write_piece(filename='ref_part_2.vts', i1=nx1_p(2), i2=nx2_p(2))

  ! --- one rank writes the parallel header ---
  error = a_pvtk_file%initialize(filename='ref.pvts', mesh_topology='PStructuredGrid', mesh_kind='Float64', &
                                 nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='pressure', data_type='Float64', number_of_components=1)
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_1.vts', &
                                                    nx1=nx1_p(1), nx2=nx2_p(1), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_2.vts', &
                                                    nx1=nx1_p(2), nx2=nx2_p(2), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = a_pvtk_file%finalize()
  !endregion pvts
  endsubroutine pvts

  subroutine write_piece(filename, i1, i2)
  !< Write the piece i1...i2 along x of the structured grid 0:9 x 0:5 x 0:5, with its node array "pressure".
  character(*), intent(in) :: filename
  integer(I4P), intent(in) :: i1, i2
  real(R8P)                :: x(i1:i2,0:5,0:5), y(i1:i2,0:5,0:5), z(i1:i2,0:5,0:5)
  type(vtk_file)           :: a_vtk_file
  integer(I4P)             :: i, j, k, error

  do k=0, 5 ; do j=0, 5 ; do i=i1, i2
    x(i,j,k) = i ; y(i,j,k) = j ; z(i,j,k) = k
  enddo ; enddo ; enddo
  error = a_vtk_file%initialize(format='raw', filename=filename, mesh_topology='StructuredGrid', &
                                nx1=0, nx2=9, ny1=0, ny2=5, nz1=0, nz2=5)
  error = a_vtk_file%xml_writer%write_piece(nx1=i1, nx2=i2, ny1=0, ny2=5, nz1=0, nz2=5)
  error = a_vtk_file%xml_writer%write_geo(n=size(x), x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='pressure', x=x, one_component=.true.)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_piece

  subroutine pvtr
  type(pvtk_file) :: a_pvtk_file
  type(vtk_file)  :: a_vtk_file
  integer(I4P)    :: p, error

  do p=1, 2   ! the pieces: points 0-2 and 2-4 along x
    error = a_vtk_file%initialize(format='raw', filename='ref_part_'//achar(48+p)//'.vtr', mesh_topology='RectilinearGrid', &
                                  nx1=0, nx2=4, ny1=0, ny2=2, nz1=0, nz2=2)
    error = a_vtk_file%xml_writer%write_piece(nx1=2*p-2, nx2=2*p, ny1=0, ny2=2, nz1=0, nz2=2)
    error = a_vtk_file%xml_writer%write_geo(x=[real(2*p-2, R8P), real(2*p-1, R8P), real(2*p, R8P)], &
                                            y=[0._R8P, 1._R8P, 2._R8P], z=[0._R8P, 1._R8P, 2._R8P])
    error = a_vtk_file%xml_writer%write_piece()
    error = a_vtk_file%finalize()
  enddo
  !region pvtr
  error = a_pvtk_file%initialize(filename='ref.pvtr', mesh_topology='PRectilinearGrid', mesh_kind='Float64', &
                                 nx1=0, nx2=4, ny1=0, ny2=2, nz1=0, nz2=2)
  ! ... PPointData / PCellData as for PVTS ...
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_1.vtr', nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=2)
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_2.vtr', nx1=2, nx2=4, ny1=0, ny2=2, nz1=0, nz2=2)
  error = a_pvtk_file%finalize()
  !endregion pvtr
  endsubroutine pvtr

  subroutine pvtu
  type(vtk_file) :: a_vtk_file
  integer(I4P)   :: p, error

  do p=1, 2   ! the pieces: one tetrahedron each, with local point ids 0-3
    error = a_vtk_file%initialize(format='raw', filename='ref_part_'//achar(48+p)//'.vtu', mesh_topology='UnstructuredGrid')
    error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
    error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P] + p, &
                                            y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])
    error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0,1,2,3], offset=[4], cell_type=[10_I1P])
    error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
    error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=[300._R8P, 310._R8P, 320._R8P, 330._R8P])
    error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
    error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
    error = a_vtk_file%xml_writer%write_dataarray(data_name='part', x=[p])
    error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
    error = a_vtk_file%xml_writer%write_piece()
    error = a_vtk_file%finalize()
  enddo
  block
  !region pvtu
  use vtk_fortran, only : pvtk_file
  use penf,        only : I4P

  type(pvtk_file) :: a_pvtk_file
  integer(I4P)    :: error

  ! --- after every rank has written its own ref_part_N.vtu ---
  error = a_pvtk_file%initialize(filename='ref.pvtu', mesh_topology='PUnstructuredGrid', mesh_kind='Float64')
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='temperature', data_type='Float64', &
                                                          number_of_components=1)
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='part', data_type='Int32', number_of_components=1)
  error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_1.vtu')
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_2.vtu')
  error = a_pvtk_file%finalize()
  !endregion pvtu
  endblock
  endsubroutine pvtu

  subroutine pvti
  type(pvtk_file) :: a_pvtk_file
  type(vtk_file)  :: a_vtk_file
  real(R8P)       :: phi(0:15,0:15,0:7)
  integer(I4P)    :: p, i1, i2, error

  phi = 1
  do p=1, 2   ! the pieces: points 0-8 and 8-15 along x, the same origin and spacing
    i1 = 8*(p-1) ; i2 = min(8*p, 15)
    error = a_vtk_file%initialize(format='raw', filename='ref_part_'//achar(48+p)//'.vti', mesh_topology='ImageData', &
                                  nx1=0, nx2=15, ny1=0, ny2=15, nz1=0, nz2=7, &
                                  origin=[0._R8P, 0._R8P, 0._R8P], spacing=[0.1_R8P, 0.1_R8P, 0.2_R8P])
    error = a_vtk_file%xml_writer%write_piece(nx1=i1, nx2=i2, ny1=0, ny2=15, nz1=0, nz2=7)
    error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
    error = a_vtk_file%xml_writer%write_dataarray(data_name='phi', x=phi(i1:i2,:,:), one_component=.true.)
    error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
    error = a_vtk_file%xml_writer%write_piece()
    error = a_vtk_file%finalize()
  enddo
  !region pvti
  error = a_pvtk_file%initialize(filename='ref.pvti', mesh_topology='PImageData',          &
                                 nx1=0, nx2=15, ny1=0, ny2=15, nz1=0, nz2=7,              &
                                 origin=[0._R8P, 0._R8P, 0._R8P], spacing=[0.1_R8P, 0.1_R8P, 0.2_R8P])
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='phi', data_type='Float64', number_of_components=1)
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_1.vti', nx1=0, nx2=8,  ny1=0, ny2=15, nz1=0, nz2=7)
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_2.vti', nx1=8, nx2=15, ny1=0, ny2=15, nz1=0, nz2=7)
  error = a_pvtk_file%finalize()
  !endregion pvti
  endsubroutine pvti

  subroutine pvtp
  type(pvtk_file) :: a_pvtk_file
  type(vtk_file)  :: a_vtk_file
  integer(I4P)    :: p, error

  do p=1, 2   ! the pieces: one triangle each
    error = a_vtk_file%initialize(format='raw', filename='ref_part_'//achar(48+p)//'.vtp', mesh_topology='PolyData')
    error = a_vtk_file%xml_writer%write_piece(np=3, nverts=0, nlines=0, nstrips=0, npolys=1)
    error = a_vtk_file%xml_writer%write_geo(np=3, nc=1, x=[0._R8P, 1._R8P, 0._R8P] + p, y=[0._R8P, 0._R8P, 1._R8P], &
                                            z=[0._R8P, 0._R8P, 0._R8P])
    error = a_vtk_file%xml_writer%write_polydata_cells(polys_connectivity=[0,1,2], polys_offset=[3])
    error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
    error = a_vtk_file%xml_writer%write_dataarray(data_name='id', x=[p])
    error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
    error = a_vtk_file%xml_writer%write_piece()
    error = a_vtk_file%finalize()
  enddo
  !region pvtp
  error = a_pvtk_file%initialize(filename='ref.pvtp', mesh_topology='PPolyData', mesh_kind='Float64')
  error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='id', data_type='Int32', number_of_components=1)
  error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_1.vtp')
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_2.vtp')
  error = a_pvtk_file%finalize()
  !endregion pvtp
  endsubroutine pvtp

  subroutine vtm
  !region vtm
  use vtk_fortran, only : vtm_file, vtk_file
  use penf,        only : I4P, R8P

  type(vtk_file)  :: a_vtk_file
  type(vtm_file)  :: a_vtm_file
  character(16)   :: filenames(4)
  real(R8P)       :: x(0:3), y(0:2), z(0:2)
  integer(I4P)    :: f, error

  filenames = ['ref_block_1.vtr', 'ref_block_2.vtr', 'ref_block_3.vtr', 'ref_block_4.vtr']
  y = [0, 1, 2] ; z = [0, 1, 2]
  ! write each block as an independent file
  do f=1, size(filenames, dim=1)
    x = [0, 1, 2, 3] + 4._R8P*(f - 1)
    error = a_vtk_file%initialize(format='raw', filename=trim(filenames(f)), mesh_topology='RectilinearGrid', &
                                  nx1=0, nx2=3, ny1=0, ny2=2, nz1=0, nz2=2)
    error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=3, ny1=0, ny2=2, nz1=0, nz2=2)
    error = a_vtk_file%xml_writer%write_geo(x=x, y=y, z=z)
    error = a_vtk_file%xml_writer%write_piece()
    error = a_vtk_file%finalize()
  enddo

  ! assemble the multi-block file
  error = a_vtm_file%initialize(filename='ref.vtm')
  error = a_vtm_file%write_block(filenames=[filenames(1), filenames(2)], names=['1','2'], name='first block')
  error = a_vtm_file%write_block(filenames=[filenames(3), filenames(4)], names=['3','4'], name='second block')
  error = a_vtm_file%finalize()
  !endregion vtm
  endsubroutine vtm

  subroutine vtm_nested
  use vtk_fortran, only : vtm_file
  type(vtm_file) :: a_vtm_file
  integer(I4P)   :: error

  !region vtm-nested
  error = a_vtm_file%initialize(filename='ref_machine.vtm')
  error = a_vtm_file%write_block(action='open', name='assembly')                                  ! Block 0
  error = a_vtm_file%write_block(filenames=['a.vtu', 'b.vtu'], names=['a', 'b'], action='write')   !   DataSet 0, 1
  error = a_vtm_file%write_block(filenames=['b1.vtu', 'b2.vtu'], name='bolts')                     !   Block 2: DataSet 0, 1
  error = a_vtm_file%write_block(action='open', name='sub-assembly')                              !   Block 3
  error = a_vtm_file%write_block(filenames=['n.vtu'], name='nuts')                                 !     Block 0: DataSet 0
  error = a_vtm_file%write_block(action='close')                                                  !   (end of Block 3)
  error = a_vtm_file%write_block(action='close')                                                  ! (end of Block 0)
  error = a_vtm_file%finalize()
  !endregion vtm-nested
  endsubroutine vtm_nested

  subroutine vtm_scratch
  use vtk_fortran, only : vtm_file
  use penf,        only : str
  type(vtm_file) :: a_vtm_file
  integer(I4P)   :: step, error

  !region vtm-scratch
  error = a_vtm_file%initialize(filename='ref_series.vtm', scratch_units_number=2)       ! 2 blocks
  error = a_vtm_file%write_block(scratch=1, action='open', name='fluid domain')         ! the name of each block
  error = a_vtm_file%write_block(scratch=2, action='open', name='solid')
  do step=1, 3   ! the files of the blocks are known one at a time, interleaved
    error = a_vtm_file%write_block(scratch=1, action='write', filename='fluid/step_'//trim(str(step, .true.))//'.vtu')
    error = a_vtm_file%write_block(scratch=2, action='write', filename='solid/step_'//trim(str(step, .true.))//'.vtu')
  enddo
  error = a_vtm_file%finalize()                                                         ! writes the blocks
  !endregion vtm-scratch
  endsubroutine vtm_scratch

  subroutine pvd
  !region pvd
  use vtk_fortran, only : pvd_file
  use penf,        only : I4P, R8P, strz

  type(pvd_file)          :: a_pvd_file
  integer(I4P), parameter :: nsteps = 3
  integer(I4P)            :: step, error
  real(R8P)               :: time

  error = a_pvd_file%initialize(filename='ref_simulation.pvd')
  do step=0, nsteps
    time = step / 10._R8P
    ! ... advance the solution to time, write ref_simulation_NNNN.vtu with a vtk_file ...
    error = a_pvd_file%write_dataset(filename='ref_simulation_'//trim(strz(step, 4))//'.vtu', timestep=time)
  enddo
  error = a_pvd_file%finalize()
  !endregion pvd
  endsubroutine pvd

  subroutine volatile
  !region volatile
  use vtk_fortran, only : vtk_file, write_xml_volatile
  use penf,        only : I4P, R8P

  type(vtk_file)                :: a_vtk_file
  character(len=:), allocatable :: xml_volatile
  integer(I4P)                  :: error

  ! on a process without access to the file system
  error = a_vtk_file%initialize(format='binary', filename='ref_volatile.vtr', mesh_topology='RectilinearGrid', &
                                is_volatile=.true., nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=1)
  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=1)
  error = a_vtk_file%xml_writer%write_geo(x=[0._R8P, 1._R8P], y=[0._R8P, 1._R8P], z=[0._R8P, 1._R8P])
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  call a_vtk_file%get_xml_volatile(xml_volatile) ! the whole file, as a string
  call a_vtk_file%free                           ! free the memory of the volatile file
  ! ... send xml_volatile to the master ...

  ! on the master
  error = write_xml_volatile(xml_volatile=xml_volatile, filename='ref_volatile.vtr')
  !endregion volatile
  endsubroutine volatile

  subroutine check(files)
  !< Read every parallel header back and check it against its pieces: the program stops with an error on a mismatch.
  character(*), intent(in)      :: files(:)
  type(pvtk_file)               :: a_pvtk_file
  character(len=:), allocatable :: message
  integer(I4P)                  :: f, error

  do f=1, size(files)
    error = a_pvtk_file%initialize(filename=trim(files(f)), action='read')
    if (error == 0) error = a_pvtk_file%xml_reader%check_pieces(message=message)
    if (error /= 0) then
      print '(A,I0)', trim(files(f))//': error ', error
      error stop
    endif
    print '(A)', trim(files(f))//': pieces checked'
    error = a_pvtk_file%finalize()
  enddo
  endsubroutine check
endprogram ref_parallel
