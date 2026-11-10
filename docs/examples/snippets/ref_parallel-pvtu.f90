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
