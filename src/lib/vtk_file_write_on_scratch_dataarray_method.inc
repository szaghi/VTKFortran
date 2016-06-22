    ! Write on scratch dataarray method of VTK file class.
    generic, private               :: write_on_scratch_dataarray =>      &
                                      write_on_scratch_dataarray1_rank1, &
                                      write_on_scratch_dataarray1_rank2, &
                                      write_on_scratch_dataarray1_rank3, &
                                      write_on_scratch_dataarray1_rank4, &
                                      write_on_scratch_dataarray3_rank1, &
                                      write_on_scratch_dataarray3_rank2, &
                                      write_on_scratch_dataarray3_rank3 !< Write dataarray.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank1 !< Write dataarray, data 1 rank 1.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank2 !< Write dataarray, data 1 rank 2.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank3 !< Write dataarray, data 1 rank 3.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank4 !< Write dataarray, data 1 rank 4.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1 !< Write dataarray, comp 3 rank 1.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2 !< Write dataarray, comp 3 rank 2.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3 !< Write dataarray, comp 3 rank 3.
