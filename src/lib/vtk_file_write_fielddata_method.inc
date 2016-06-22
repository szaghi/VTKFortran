!< Write fielddata method of VTK file class.
    generic                        :: write_fielddata =>          &
                                      write_fielddata1_rank0_R8P, &
                                      write_fielddata1_rank0_R4P, &
                                      write_fielddata1_rank0_I8P, &
                                      write_fielddata1_rank0_I4P, &
                                      write_fielddata1_rank0_I2P, &
                                      write_fielddata1_rank0_I1P, &
                                      write_fielddata_tag        !< Write FieldData tag.
    procedure, pass(self), private :: write_fielddata1_rank0_R8P !< Write FieldData tag (data 1, rank 0, R8P).
    procedure, pass(self), private :: write_fielddata1_rank0_R4P !< Write FieldData tag (data 1, rank 0, R4P).
    procedure, pass(self), private :: write_fielddata1_rank0_I8P !< Write FieldData tag (data 1, rank 0, I8P).
    procedure, pass(self), private :: write_fielddata1_rank0_I4P !< Write FieldData tag (data 1, rank 0, I4P).
    procedure, pass(self), private :: write_fielddata1_rank0_I2P !< Write FieldData tag (data 1, rank 0, I2P).
    procedure, pass(self), private :: write_fielddata1_rank0_I1P !< Write FieldData tag (data 1, rank 0, I1P).
    procedure, pass(self), private :: write_fielddata_tag        !< Write FieldData tag.
