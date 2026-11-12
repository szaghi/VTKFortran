/* VTKFortran: the MPI file access and collective transfers of a parallel HDF5 file (VTKHDF written by the ranks of a
 * communicator).
 *
 * The HDF5 C API takes an MPI_Comm, the Fortran caller has a Fortran handle (mpi_f08 `MPI_Comm%MPI_VAL`): MPI_Comm_f2c
 * converts it, and only C can call it. Built only with VTKFORTRAN_USE_MPI (and a parallel HDF5): the other builds leave
 * src/mpi/ out, so they need no C compiler. */
#ifdef VTKFORTRAN_USE_MPI
#include <mpi.h>
#include <hdf5.h>

/* A file access property list for MPI-IO on the communicator `fcomm` (Fortran handle); negative on error. */
hid_t vtkfortran_h5_fapl_mpio(MPI_Fint fcomm) {
  hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);
  if (fapl < 0) return -1;
  if (H5Pset_fapl_mpio(fapl, MPI_Comm_f2c(fcomm), MPI_INFO_NULL) < 0) {
    H5Pclose(fapl);
    return -1;
  }
  return fapl;
}

/* A data transfer property list for collective MPI-IO transfers (every rank calls H5Dwrite, a rank without values with
 * an empty selection): parallel HDF5 writes a deflated (filtered) dataset only so; negative on error. */
hid_t vtkfortran_h5_dxpl_mpio_collective(void) {
  hid_t dxpl = H5Pcreate(H5P_DATASET_XFER);
  if (dxpl < 0) return -1;
  if (H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE) < 0) {
    H5Pclose(dxpl);
    return -1;
  }
  return dxpl;
}
#else
typedef int vtkfortran_mpi_disabled; /* ISO C forbids an empty translation unit */
#endif
