!< Write data method of VTK file class.
    generic                        :: write_dataarray =>            &
                                      write_dataarray_location_tag, &
                                      write_dataarray1_rank1_R8P,   &
                                      write_dataarray1_rank1_R4P,   &
                                      write_dataarray1_rank1_I8P,   &
                                      write_dataarray1_rank1_I4P,   &
                                      write_dataarray1_rank1_I2P,   &
                                      write_dataarray1_rank1_I1P,   &
                                      write_dataarray1_rank2_R8P,   &
                                      write_dataarray1_rank2_R4P,   &
                                      write_dataarray1_rank2_I8P,   &
                                      write_dataarray1_rank2_I4P,   &
                                      write_dataarray1_rank2_I2P,   &
                                      write_dataarray1_rank2_I1P,   &
                                      write_dataarray1_rank3_R8P,   &
                                      write_dataarray1_rank3_R4P,   &
                                      write_dataarray1_rank3_I8P,   &
                                      write_dataarray1_rank3_I4P,   &
                                      write_dataarray1_rank3_I2P,   &
                                      write_dataarray1_rank3_I1P,   &
                                      write_dataarray1_rank4_R8P,   &
                                      write_dataarray1_rank4_R4P,   &
                                      write_dataarray1_rank4_I8P,   &
                                      write_dataarray1_rank4_I4P,   &
                                      write_dataarray1_rank4_I2P,   &
                                      write_dataarray1_rank4_I1P,   &
                                      write_dataarray3_rank1_R8P,   &
                                      write_dataarray3_rank1_R4P,   &
                                      write_dataarray3_rank1_I8P,   &
                                      write_dataarray3_rank1_I4P,   &
                                      write_dataarray3_rank1_I2P,   &
                                      write_dataarray3_rank1_I1P,   &
                                      write_dataarray3_rank3_R8P,   &
                                      write_dataarray3_rank3_R4P,   &
                                      write_dataarray3_rank3_I8P,   &
                                      write_dataarray3_rank3_I4P,   &
                                      write_dataarray3_rank3_I2P,   &
                                      write_dataarray3_rank3_I1P   !< Write data (array).
    procedure, pass(self), private :: write_dataarray_location_tag !< Write `<[/]PointData>` or `<[/]CellData>` open/close tag.
    procedure, pass(self), private :: write_dataarray1_rank1_R8P   !< Write DataArray (data 1, rank 1, R8P).
    procedure, pass(self), private :: write_dataarray1_rank1_R4P   !< Write DataArray (data 1, rank 1, R4P).
    procedure, pass(self), private :: write_dataarray1_rank1_I8P   !< Write DataArray (data 1, rank 1, I8P).
    procedure, pass(self), private :: write_dataarray1_rank1_I4P   !< Write DataArray (data 1, rank 1, I4P).
    procedure, pass(self), private :: write_dataarray1_rank1_I2P   !< Write DataArray (data 1, rank 1, I2P).
    procedure, pass(self), private :: write_dataarray1_rank1_I1P   !< Write DataArray (data 1, rank 1, I1P).
    procedure, pass(self), private :: write_dataarray1_rank2_R8P   !< Write DataArray (data 1, rank 2, R8P).
    procedure, pass(self), private :: write_dataarray1_rank2_R4P   !< Write DataArray (data 1, rank 2, R4P).
    procedure, pass(self), private :: write_dataarray1_rank2_I8P   !< Write DataArray (data 1, rank 2, I8P).
    procedure, pass(self), private :: write_dataarray1_rank2_I4P   !< Write DataArray (data 1, rank 2, I4P).
    procedure, pass(self), private :: write_dataarray1_rank2_I2P   !< Write DataArray (data 1, rank 2, I2P).
    procedure, pass(self), private :: write_dataarray1_rank2_I1P   !< Write DataArray (data 1, rank 2, I1P).
    procedure, pass(self), private :: write_dataarray1_rank3_R8P   !< Write DataArray (data 1, rank 3, R8P).
    procedure, pass(self), private :: write_dataarray1_rank3_R4P   !< Write DataArray (data 1, rank 3, R4P).
    procedure, pass(self), private :: write_dataarray1_rank3_I8P   !< Write DataArray (data 1, rank 3, I8P).
    procedure, pass(self), private :: write_dataarray1_rank3_I4P   !< Write DataArray (data 1, rank 3, I4P).
    procedure, pass(self), private :: write_dataarray1_rank3_I2P   !< Write DataArray (data 1, rank 3, I2P).
    procedure, pass(self), private :: write_dataarray1_rank3_I1P   !< Write DataArray (data 1, rank 3, I1P).
    procedure, pass(self), private :: write_dataarray1_rank4_R8P   !< Write DataArray (data 1, rank 4, R8P).
    procedure, pass(self), private :: write_dataarray1_rank4_R4P   !< Write DataArray (data 1, rank 4, R4P).
    procedure, pass(self), private :: write_dataarray1_rank4_I8P   !< Write DataArray (data 1, rank 4, I8P).
    procedure, pass(self), private :: write_dataarray1_rank4_I4P   !< Write DataArray (data 1, rank 4, I4P).
    procedure, pass(self), private :: write_dataarray1_rank4_I2P   !< Write DataArray (data 1, rank 4, I2P).
    procedure, pass(self), private :: write_dataarray1_rank4_I1P   !< Write DataArray (data 1, rank 4, I1P).
    procedure, pass(self), private :: write_dataarray3_rank1_R8P   !< Write DataArray (data 3, rank 1, R8P).
    procedure, pass(self), private :: write_dataarray3_rank1_R4P   !< Write DataArray (data 3, rank 1, R4P).
    procedure, pass(self), private :: write_dataarray3_rank1_I8P   !< Write DataArray (data 3, rank 1, I8P).
    procedure, pass(self), private :: write_dataarray3_rank1_I4P   !< Write DataArray (data 3, rank 1, I4P).
    procedure, pass(self), private :: write_dataarray3_rank1_I2P   !< Write DataArray (data 3, rank 1, I2P).
    procedure, pass(self), private :: write_dataarray3_rank1_I1P   !< Write DataArray (data 3, rank 1, I1P).
    procedure, pass(self), private :: write_dataarray3_rank3_R8P   !< Write DataArray (data 3, rank 3, R8P).
    procedure, pass(self), private :: write_dataarray3_rank3_R4P   !< Write DataArray (data 3, rank 3, R4P).
    procedure, pass(self), private :: write_dataarray3_rank3_I8P   !< Write DataArray (data 3, rank 3, I8P).
    procedure, pass(self), private :: write_dataarray3_rank3_I4P   !< Write DataArray (data 3, rank 3, I4P).
    procedure, pass(self), private :: write_dataarray3_rank3_I2P   !< Write DataArray (data 3, rank 3, I2P).
    procedure, pass(self), private :: write_dataarray3_rank3_I1P   !< Write DataArray (data 3, rank 3, I1P).
    procedure, pass(self), private :: write_dataarray_tag          !< Write `<DataArray...>...</DataArray>` tag.
    procedure, pass(self), private :: write_dataarray_tag_appended !< Write `<DataArray.../>` appended tag.
    procedure, pass(self), private :: write_dataarray_appended     !< Write `<AppendedData...>...</AppendedData>` tag.
