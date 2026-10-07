!as heat
!run heat_7 heat
!render heat_7 heat.vtm array=temperature clip=Z,0.47 crinkle=1 overlay=probes.vtp points=16 zoom=1.15
program heat
!< Tutorial, chapter 7: an assembly of the pieces of chapter 6 and of a set of probes, in a multi-block file.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file, vtm_file
implicit none
integer(I4P), parameter :: probes=8
real(R8P),    parameter :: px(probes)=[0.2_R8P, 0.35_R8P, 0.5_R8P, 0.65_R8P, 0.8_R8P, 0.35_R8P, 0.7_R8P, 0.5_R8P]
real(R8P),    parameter :: py(probes)=[0.2_R8P, 0.2_R8P, 0.2_R8P, 0.2_R8P, 0.2_R8P, 0.4_R8P, 0.65_R8P, 0.8_R8P]
real(R8P),    parameter :: pz(probes)=0.5_R8P
real(R8P)               :: tp(probes)
type(vtk_file)          :: a_vtk_file
type(vtm_file)          :: assembly
integer(I4P),     allocatable :: level(:)
character(len=:), allocatable :: kind(:), name(:), file(:)
integer(I4P)            :: e, error

! the temperature at the probes, read from the pieces written by chapter 6
tp = probe(px, py, pz)

!region probes
! the probes: a polydata of points, one vertex each
error = a_vtk_file%initialize(format='ascii', filename='probes.vtp', mesh_topology='PolyData')
error = a_vtk_file%xml_writer%write_piece(np=probes, nverts=probes, nlines=0, nstrips=0, npolys=0)
error = a_vtk_file%xml_writer%write_geo(np=probes, nc=probes, x=px, y=py, z=pz)
error = a_vtk_file%xml_writer%write_polydata_cells(verts_connectivity=[(e - 1, e=1, probes)], verts_offset=[(e, e=1, probes)])
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=tp)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
!endregion probes

!region assembly
error = assembly%initialize(filename='heat.vtm')
error = assembly%write_block(action='open', name='solver')
error = assembly%write_block(filenames=['heat_1.vtu', 'heat_2.vtu', 'heat_3.vtu', 'heat_4.vtu'], &
                             names=['piece-1', 'piece-2', 'piece-3', 'piece-4'], name='domain')
error = assembly%write_block(filenames=['probes.vtp'], names=['probes'], name='sensors')
error = assembly%write_block(action='close')
error = assembly%finalize()
!endregion assembly

!region tree
! read the assembly back: its blocks and datasets, depth first
error = assembly%initialize(filename='heat.vtm', action='read')
error = assembly%get_entries(level=level, kind=kind, name=name, file=file)
do e=1, size(level)
  print '(A,A,1X,A,1X,A)', repeat('  ', level(e) - 1), trim(kind(e)), trim(name(e)), trim(file(e))
enddo
error = assembly%finalize()
!endregion tree
contains
  impure elemental function probe(x, y, z) result(t)
  !< The temperature at the point nearest to (x,y,z), read from the piece that owns it.
  real(R8P), intent(in)  :: x, y, z
  real(R8P)              :: t
  real(R8P), allocatable :: qx(:), qy(:), qz(:), temperature(:)
  type(vtk_file)         :: piece
  character(len=10)      :: filename
  integer(I4P)           :: p, error

  ! the pieces split the cube along x in 4 equal parts: the owner of x
  p = min(4, int(x*4) + 1)
  write(filename, '(A,I1,A)') 'heat_', p, '.vtu'
  error = piece%initialize(filename=filename, action='read')
  error = piece%xml_reader%read_geo(x=qx, y=qy, z=qz)
  error = piece%xml_reader%read_dataarray(location='node', data_name='temperature', x=temperature)
  error = piece%finalize()
  t = temperature(minloc((qx - x)**2 + (qy - y)**2 + (qz - z)**2, dim=1))
  endfunction probe
endprogram heat
