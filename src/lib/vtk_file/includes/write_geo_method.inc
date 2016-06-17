!< Write geo method of VTK file class.
    generic                        :: write_geo =>                     &
                                      write_geo_strg_data1_rank2_R8P,  &
                                      write_geo_strg_data1_rank2_R4P,  &
                                      write_geo_strg_data1_rank4_R8P,  &
                                      write_geo_strg_data1_rank4_R4P,  &
                                      write_geo_strg_data3_rank1_R8P,  &
                                      write_geo_strg_data3_rank1_R4P,  &
                                      write_geo_strg_data3_rank3_R8P,  &
                                      write_geo_strg_data3_rank3_R4P !< Write mesh.
    procedure, pass(self), private :: write_geo_strg_data1_rank2_R8P !< Write **StructuredGrid** mesh (data 1, rank 2, R8P).
    procedure, pass(self), private :: write_geo_strg_data1_rank2_R4P !< Write **StructuredGrid** mesh (data 1, rank 2, R4P).
    procedure, pass(self), private :: write_geo_strg_data1_rank4_R8P !< Write **StructuredGrid** mesh (data 1, rank 4, R8P).
    procedure, pass(self), private :: write_geo_strg_data1_rank4_R4P !< Write **StructuredGrid** mesh (data 1, rank 4, R4P).
    procedure, pass(self), private :: write_geo_strg_data3_rank1_R8P !< Write **StructuredGrid** mesh (data 3, rank 1, R8P).
    procedure, pass(self), private :: write_geo_strg_data3_rank1_R4P !< Write **StructuredGrid** mesh (data 3, rank 1, R4P).
    procedure, pass(self), private :: write_geo_strg_data3_rank3_R8P !< Write **StructuredGrid** mesh (data 3, rank 3, R8P).
    procedure, pass(self), private :: write_geo_strg_data3_rank3_R4P !< Write **StructuredGrid** mesh (data 3, rank 3, R4P).
