type(pvtk_file)               :: a_pvtk_file
character(len=:), allocatable :: sources(:), names(:), message
integer(I4P)                  :: error

error = a_pvtk_file%initialize(filename='ref.pvtu', action='read')
error = a_pvtk_file%xml_reader%get_sources(sources)                              ! files of the pieces
error = a_pvtk_file%xml_reader%get_dataarray_names(location='node', names=names) ! declared point arrays
error = a_pvtk_file%xml_reader%check_pieces(message=message)                     ! 0, or 7 and the first mismatch
error = a_pvtk_file%finalize()
