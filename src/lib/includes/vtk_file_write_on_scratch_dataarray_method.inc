    ! Write on scratch dataarray method of VTK file class.
    generic, private               :: write_on_scratch_dataarray =>      &
                                      write_on_scratch_dataarray1_rank1, &
                                      write_on_scratch_dataarray1_rank2, &
                                      write_on_scratch_dataarray1_rank3, &
                                      write_on_scratch_dataarray1_rank4, &
                                      write_on_scratch_dataarray3_rank1_R8P, &
                                      write_on_scratch_dataarray3_rank1_R4P, &
                                      write_on_scratch_dataarray3_rank1_I8P, &
                                      write_on_scratch_dataarray3_rank1_I4P, &
                                      write_on_scratch_dataarray3_rank1_I2P, &
                                      write_on_scratch_dataarray3_rank1_I1P, &
                                      write_on_scratch_dataarray3_rank2_R8P, &
                                      write_on_scratch_dataarray3_rank2_R4P, &
                                      write_on_scratch_dataarray3_rank2_I8P, &
                                      write_on_scratch_dataarray3_rank2_I4P, &
                                      write_on_scratch_dataarray3_rank2_I2P, &
                                      write_on_scratch_dataarray3_rank2_I1P, &
                                      write_on_scratch_dataarray3_rank3_R8P, &
                                      write_on_scratch_dataarray3_rank3_R4P, &
                                      write_on_scratch_dataarray3_rank3_I8P, &
                                      write_on_scratch_dataarray3_rank3_I4P, &
                                      write_on_scratch_dataarray3_rank3_I2P, &
                                      write_on_scratch_dataarray3_rank3_I1P !< Write dataarray.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank1 !< Write dataarray, data 1 rank 1.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank2 !< Write dataarray, data 1 rank 2.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank3 !< Write dataarray, data 1 rank 3.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank4 !< Write dataarray, data 1 rank 4.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_R8P !< Write dataarray, comp 3 rank 1, R8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_R4P !< Write dataarray, comp 3 rank 1, R4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_I8P !< Write dataarray, comp 3 rank 1, I8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_I4P !< Write dataarray, comp 3 rank 1, I4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_I2P !< Write dataarray, comp 3 rank 1, I2P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_I1P !< Write dataarray, comp 3 rank 1, I1P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_R8P !< Write dataarray, comp 3 rank 2, R8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_R4P !< Write dataarray, comp 3 rank 2, R4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_I8P !< Write dataarray, comp 3 rank 2, I8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_I4P !< Write dataarray, comp 3 rank 2, I4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_I2P !< Write dataarray, comp 3 rank 2, I2P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_I1P !< Write dataarray, comp 3 rank 2, I1P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_R8P !< Write dataarray, comp 3 rank 3, R8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_R4P !< Write dataarray, comp 3 rank 3, R4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_I8P !< Write dataarray, comp 3 rank 3, I8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_I4P !< Write dataarray, comp 3 rank 3, I4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_I2P !< Write dataarray, comp 3 rank 3, I2P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_I1P !< Write dataarray, comp 3 rank 3, I1P.
