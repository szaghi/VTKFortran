    ! Finalize method of VTK file class.
    generic                        :: finalize => &
                                      finalize_write !< Finalize file.
    procedure, pass(self), private :: finalize_write !< Finalize file (exporter).
