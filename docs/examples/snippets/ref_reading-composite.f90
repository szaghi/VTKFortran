type(vtm_file)                :: a_vtm_file
type(pvd_file)                :: a_pvd_file
integer(I4P),     allocatable :: level(:), index(:)
character(len=:), allocatable :: kind(:), name(:), file(:)
real(R8P),        allocatable :: timestep(:)
integer(I4P)                  :: error

error = a_vtm_file%initialize(filename='ref_machine.vtm', action='read')
error = a_vtm_file%get_entries(level=level, kind=kind, index=index, name=name, file=file)
error = a_vtm_file%finalize()

error = a_pvd_file%initialize(filename='ref_simulation.pvd', action='read')
error = a_pvd_file%get_datasets(timestep=timestep, file=file)  ! also part, group, name
error = a_pvd_file%finalize()
