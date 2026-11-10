!< VTK_Fortran test: write and read files concurrently from OpenMP threads.
program vtk_fortran_write_openmp
!< VTK_Fortran test: write and read files concurrently from OpenMP threads.
!<
!< Each file keeps its state in its own `vtk_file`: files written by different threads at the same time must be the same, byte
!< for byte, as the same files written one after the other, in every format (the appended ones use a scratch file each).
!< Then each thread reads its own file back. The library is built without OpenMP, as a user links it; CMake builds this test
!< with OpenMP. Built without it (FoBiS modes), the directives are comments and the test runs serially.
!$ use omp_lib
use penf
use vtk_fortran, only : vtk_file

implicit none
integer(I4P), parameter :: nfiles=8                                                        !< Files written concurrently.
integer(I4P), parameter :: nc=2000                                                         !< Cells of each file.
character(15), parameter :: formats(4)=[character(15) :: 'ascii', 'binary', 'raw', 'binary-appended'] !< Formats, in turn.
logical                  :: written(nfiles)                                                !< Files written alike.
logical                  :: read_back(nfiles)                                              !< Files read back.
integer(I4P)             :: f                                                              !< Counter.
integer(I4P)             :: threads                                                        !< Threads available.

threads = 1
!$ threads = omp_get_max_threads()
! serially: the reference files, and the initialization of PENF and BeFoR64 (done by the first initialize)
do f=1, nfiles
  call write_file(f, 'vtkfortran_write_openmp_serial_'//trim(str(f, .true.))//'.vtu')
enddo
! concurrently: the same files, then each one read back by a thread
!$omp parallel do schedule(static, 1)
do f=1, nfiles
  call write_file(f, 'vtkfortran_write_openmp_parallel_'//trim(str(f, .true.))//'.vtu')
enddo
!$omp end parallel do
do f=1, nfiles
  written(f) = same_bytes('vtkfortran_write_openmp_serial_'//trim(str(f, .true.))//'.vtu', &
                          'vtkfortran_write_openmp_parallel_'//trim(str(f, .true.))//'.vtu')
enddo
!$omp parallel do schedule(static, 1)
do f=1, nfiles
  read_back(f) = is_read_back(f, 'vtkfortran_write_openmp_parallel_'//trim(str(f, .true.))//'.vtu')
enddo
!$omp end parallel do

print '(A,I0,A)', 'threads: ', threads, merge(' (built with OpenMP)   ', ' (built without OpenMP)', threads > 1 .or. omp_built())
print '(A,8L2)', 'written alike:', written
print '(A,8L2)', 'read back:    ', read_back
print "(A,L1)", new_line('a')//'Are all tests passed? ', all(written) .and. all(read_back)
if (.not.(all(written) .and. all(read_back))) error stop 'some tests failed'
stop
contains
  subroutine write_file(f, filename)
  !< Write the file f: a strip of nc tetrahedra, its point data depending on f, in the format of f.
  integer(I4P), intent(in) :: f        !< File number.
  character(*), intent(in) :: filename !< File name.
  type(vtk_file)           :: a_vtk_file
  real(R8P)                :: x(nc+3), y(nc+3), z(nc+3), v(nc+3)
  integer(I4P)             :: connect(4*nc), offset(nc), p, error
  integer(I1P)             :: cell_type(nc)

  do p=1, nc + 3
    x(p) = p ; y(p) = mod(p, 2) ; z(p) = mod(p, 3) ; v(p) = f * 1000 + p
  enddo
  do p=1, nc
    connect(4*p-3:4*p) = [p - 1, p, p + 1, p + 2]
  enddo
  offset = [(4*p, p=1, nc)]
  cell_type = 10_I1P
  error = a_vtk_file%initialize(format=trim(formats(mod(f - 1, 4) + 1)), filename=filename, mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=nc+3, nc=nc)
  error = a_vtk_file%xml_writer%write_geo(np=nc+3, nc=nc, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='v', x=v)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_file

  function is_read_back(f, filename) result(is_ok)
  !< Read the point data of the file f back and check its values.
  integer(I4P), intent(in) :: f        !< File number.
  character(*), intent(in) :: filename !< File name.
  logical                  :: is_ok    !< Check result.
  type(vtk_file)           :: a_vtk_file
  real(R8P), allocatable   :: v(:)
  integer(I4P)             :: p, error

  is_ok = .false.
  error = a_vtk_file%initialize(filename=filename, action='read')
  if (error == 0) error = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='v', x=v)
  if (error == 0) is_ok = size(v) == nc + 3 .and. all(v == [(real(f * 1000 + p, R8P), p=1, nc + 3)])
  error = a_vtk_file%finalize()
  endfunction is_read_back

  function same_bytes(file1, file2) result(is_same)
  !< Compare the bytes of two files.
  character(*), intent(in)      :: file1, file2 !< File names.
  logical                       :: is_same      !< Comparison result.
  character(len=:), allocatable :: b1, b2
  integer(I4P)                  :: s1, s2, u

  inquire(file=file1, size=s1) ; inquire(file=file2, size=s2)
  is_same = s1 == s2 .and. s1 > 0
  if (.not.is_same) return
  allocate(character(len=s1) :: b1, b2)
  open(newunit=u, file=file1, access='stream', form='unformatted', action='read') ; read(u) b1 ; close(u)
  open(newunit=u, file=file2, access='stream', form='unformatted', action='read') ; read(u) b2 ; close(u)
  is_same = b1 == b2
  endfunction same_bytes

  logical function omp_built()
  !< Return true when compiled with OpenMP.
  omp_built = .false.
  !$ omp_built = .true.
  endfunction omp_built
endprogram vtk_fortran_write_openmp
