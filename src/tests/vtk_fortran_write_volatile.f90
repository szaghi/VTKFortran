!< VTK_Fortran test: write *volatile* file.
program vtk_fortran_write_volatile
!< VTK_Fortran test: write *volatile* file.

!< Test the writing of *volatile* XML file for enabling some parallel environments where only one process
!< (master process) has access to the filestyem, whereas other processes cannot.
use penf
use vtk_fortran, only : vtk_file, write_xml_volatile

implicit none
character(:), allocatable :: xml_volatile_file_1 !< XML volatile file 1.
character(:), allocatable :: xml_volatile_file_2 !< XML volatile file 2.
integer(I4P)              :: error               !< Status error.

! suppose that two slave processes create their own files, but they being not allowed to access to filesystem they creates
! "volatile" files in the form of string containing the XML-coded files
call write_slave(x_offset=0._R8P, y_offset=0._R8P, z_offset=0._R8P, xml_volatile = xml_volatile_file_1)
call write_slave(x_offset=6._R8P, y_offset=0._R8P, z_offset=0._R8P, xml_volatile = xml_volatile_file_2)

! now the master process can collect volatile files from slave processes and save them to filestystem
error = write_xml_volatile(xml_volatile=xml_volatile_file_1, filename="write_volatile_slave_1.vtr")
error = write_xml_volatile(xml_volatile=xml_volatile_file_2, filename="write_volatile_slave_2.vtr")

call write_check(filename="write_volatile_slave_1_check.vtr")
contains
   subroutine write_slave(x_offset, y_offset, z_offset, xml_volatile)
   !< Slave writer: creates *volatile* file, not a real one. This is what a slave process should do, create a volatile file (a
   !< string containing the XML file). Note that it is the same of what a standard process does for creating a real file except for
   !< the flag `is_volatile=.true.` and for the last two methods invoked for retreiving the volatile XML file and to free the
   !< memory.
   real(R8P),    intent(in)               :: x_offset     !< Offset along x axis.
   real(R8P),    intent(in)               :: y_offset     !< Offset along y axis.
   real(R8P),    intent(in)               :: z_offset     !< Offset along z axis.
   character(:), intent(out), allocatable :: xml_volatile !< XML volatile file.
   type(vtk_file)                         :: a_vtk_file   !< A VTK filei: temporary file to create the volatile string.
   ! dimensions of files
   integer(I4P), parameter :: nx1=0_I4P                              !< X lower bound extent.
   integer(I4P), parameter :: nx2=16_I4P                             !< X upper bound extent.
   integer(I4P), parameter :: ny1=0_I4P                              !< Y lower bound extent.
   integer(I4P), parameter :: ny2=16_I4P                             !< Y upper bound extent.
   integer(I4P), parameter :: nz1=0_I4P                              !< Z lower bound extent.
   integer(I4P), parameter :: nz2=16_I4P                             !< Z upper bound extent.
   integer(I4P), parameter :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1) !< Number of elements.
   ! data files
   real(R8P)    :: x(nx1:nx2) !< X coordinates.
   real(R8P)    :: y(ny1:ny2) !< Y coordinates.
   real(R8P)    :: z(nz1:nz2) !< Z coordinates.
   integer(I4P) :: v(1:nn)    !< Variable defined at coordinates.
   ! auxiliaries
   integer(I4P) :: error !< Status error.
   integer(I4P) :: i     !< Counter.
   integer(I4P) :: j     !< Counter.
   integer(I4P) :: k     !< Counter.
   integer(I4P) :: n     !< Counter.
   real(R8P)    :: x1    !< X lower extent.
   real(R8P)    :: x2    !< X upper extent.
   real(R8P)    :: y1    !< Y lower extent.
   real(R8P)    :: y2    !< Y upper extent.
   real(R8P)    :: z1    !< Z lower extent.
   real(R8P)    :: z2    !< Z upper extent.

   x1= -3._R8P   + x_offset
   x2=  3._R8P   + x_offset
   y1= -2._R8P   + y_offset
   y2=  0.25_R8P + y_offset
   z1= -2._R8P   + z_offset
   z2=  0.16_R8P + z_offset
   n = 0
   do k=nz1,nz2
      do j=ny1,ny2
         do i=nx1,nx2
            n = n + 1
            v(n) = i*j*k
         enddo
      enddo
   enddo
   do i=nx1,nx2
      x(i) = x1 + (i - 1) * (x2 - x1)/real(nx2 - nx1, kind=R8P)
   enddo
   do j=ny1,ny2
      y(j) = y1 + (j - 1) * (y2 - y1)/real(ny2 - ny1, kind=R8P)
   enddo
   do k=nz1,nz2
      z(k) = z1 + (k - 1) * (z2 - z1)/real(nz2 - nz1, kind=R8P)
   enddo
   error = a_vtk_file%initialize(format='binary', filename='volatile.vtr', mesh_topology='RectilinearGrid', &
                                 is_volatile=.true., nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
   error = a_vtk_file%xml_writer%write_fielddata(action='open')
   error = a_vtk_file%xml_writer%write_fielddata(x=0._R8P, data_name='TIME')
   error = a_vtk_file%xml_writer%write_fielddata(x=1_I8P, data_name='CYCLE')
   error = a_vtk_file%xml_writer%write_fielddata(action='close')
   error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
   error = a_vtk_file%xml_writer%write_geo(x=x, y=y, z=z)
   error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
   error = a_vtk_file%xml_writer%write_dataarray(data_name='cell_value', x=v)
   error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
   error = a_vtk_file%xml_writer%write_piece()
   error = a_vtk_file%finalize()
   ! new methods: to get the volatile file encoded into a character string and to free the memory after the file string has been
   ! retrieved
   call a_vtk_file%get_xml_volatile(xml_volatile)
   call a_vtk_file%free
   endsubroutine write_slave

   subroutine write_check(filename)
   !< Check writer: creates a real file for checking the volatile writer.
   character(*), intent(in) :: filename   !< XML file name.
   type(vtk_file)           :: a_vtk_file !< A VTK filei: temporary file to create the volatile string.
   ! dimensions of files
   integer(I4P), parameter :: nx1=0_I4P                              !< X lower bound extent.
   integer(I4P), parameter :: nx2=16_I4P                             !< X upper bound extent.
   integer(I4P), parameter :: ny1=0_I4P                              !< Y lower bound extent.
   integer(I4P), parameter :: ny2=16_I4P                             !< Y upper bound extent.
   integer(I4P), parameter :: nz1=0_I4P                              !< Z lower bound extent.
   integer(I4P), parameter :: nz2=16_I4P                             !< Z upper bound extent.
   integer(I4P), parameter :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1) !< Number of elements.
   ! data files
   real(R8P)    :: x(nx1:nx2) !< X coordinates.
   real(R8P)    :: y(ny1:ny2) !< Y coordinates.
   real(R8P)    :: z(nz1:nz2) !< Z coordinates.
   integer(I4P) :: v(1:nn)    !< Variable defined at coordinates.
   ! auxiliaries
   integer(I4P) :: error !< Status error.
   integer(I4P) :: i     !< Counter.
   integer(I4P) :: j     !< Counter.
   integer(I4P) :: k     !< Counter.
   integer(I4P) :: n     !< Counter.
   real(R8P)    :: x1    !< X lower extent.
   real(R8P)    :: x2    !< X upper extent.
   real(R8P)    :: y1    !< Y lower extent.
   real(R8P)    :: y2    !< Y upper extent.
   real(R8P)    :: z1    !< Z lower extent.
   real(R8P)    :: z2    !< Z upper extent.

   x1= -3._R8P
   x2=  3._R8P
   y1= -2._R8P
   y2=  0.25_R8P
   z1= -2._R8P
   z2=  0.16_R8P
   n = 0
   do k=nz1,nz2
      do j=ny1,ny2
         do i=nx1,nx2
            n = n + 1
            v(n) = i*j*k
         enddo
      enddo
   enddo
   do i=nx1,nx2
      x(i) = x1 + (i - 1) * (x2 - x1)/real(nx2 - nx1, kind=R8P)
   enddo
   do j=ny1,ny2
      y(j) = y1 + (j - 1) * (y2 - y1)/real(ny2 - ny1, kind=R8P)
   enddo
   do k=nz1,nz2
      z(k) = z1 + (k - 1) * (z2 - z1)/real(nz2 - nz1, kind=R8P)
   enddo
   error = a_vtk_file%initialize(format='binary', filename=filename, mesh_topology='RectilinearGrid', &
                                 is_volatile=.false., nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
   error = a_vtk_file%xml_writer%write_fielddata(action='open')
   error = a_vtk_file%xml_writer%write_fielddata(x=0._R8P, data_name='TIME')
   error = a_vtk_file%xml_writer%write_fielddata(x=1_I8P, data_name='CYCLE')
   error = a_vtk_file%xml_writer%write_fielddata(action='close')
   error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
   error = a_vtk_file%xml_writer%write_geo(x=x, y=y, z=z)
   error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
   error = a_vtk_file%xml_writer%write_dataarray(data_name='cell_value', x=v)
   error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
   error = a_vtk_file%xml_writer%write_piece()
   error = a_vtk_file%finalize()
   call a_vtk_file%free
   endsubroutine write_check
endprogram vtk_fortran_write_volatile
