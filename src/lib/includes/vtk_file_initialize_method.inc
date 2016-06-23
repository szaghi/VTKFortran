    ! Initialize method of VTK file class.
    generic                        :: initialize => &
                                      initialize_write !< Initialize file.
    procedure, pass(self), private :: initialize_write !< Initialize file (exporter).
