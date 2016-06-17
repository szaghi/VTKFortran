!< Encode base64 method of VTK file class.
    generic, private           :: encode_base64_dataarray =>          &
                                  encode_base64_dataarray1_rank1_R8P, &
                                  encode_base64_dataarray1_rank1_R4P, &
                                  encode_base64_dataarray1_rank1_I8P, &
                                  encode_base64_dataarray1_rank1_I4P, &
                                  encode_base64_dataarray1_rank1_I2P, &
                                  encode_base64_dataarray1_rank1_I1P, &
                                  encode_base64_dataarray1_rank2_R8P, &
                                  encode_base64_dataarray1_rank2_R4P, &
                                  encode_base64_dataarray1_rank2_I8P, &
                                  encode_base64_dataarray1_rank2_I4P, &
                                  encode_base64_dataarray1_rank2_I2P, &
                                  encode_base64_dataarray1_rank2_I1P, &
                                  encode_base64_dataarray1_rank3_R8P, &
                                  encode_base64_dataarray1_rank3_R4P, &
                                  encode_base64_dataarray1_rank3_I8P, &
                                  encode_base64_dataarray1_rank3_I4P, &
                                  encode_base64_dataarray1_rank3_I2P, &
                                  encode_base64_dataarray1_rank3_I1P, &
                                  encode_base64_dataarray1_rank4_R8P, &
                                  encode_base64_dataarray1_rank4_R4P, &
                                  encode_base64_dataarray1_rank4_I8P, &
                                  encode_base64_dataarray1_rank4_I4P, &
                                  encode_base64_dataarray1_rank4_I2P, &
                                  encode_base64_dataarray1_rank4_I1P, &
                                  encode_base64_dataarray3_rank1_R8P, &
                                  encode_base64_dataarray3_rank1_I8P, &
                                  encode_base64_dataarray3_rank1_I4P, &
                                  encode_base64_dataarray3_rank1_I2P, &
                                  encode_base64_dataarray3_rank1_I1P, &
                                  encode_base64_dataarray3_rank1_R4P, &
                                  encode_base64_dataarray3_rank3_R8P, &
                                  encode_base64_dataarray3_rank3_R4P, &
                                  encode_base64_dataarray3_rank3_I8P, &
                                  encode_base64_dataarray3_rank3_I4P, &
                                  encode_base64_dataarray3_rank3_I2P, &
                                  encode_base64_dataarray3_rank3_I1P !< Encode Base64 dataarray.
    procedure, nopass, private :: encode_base64_dataarray1_rank1_R8P !< Encode (Base64) dataarray, 1 data of rank 1 (R8P).
    procedure, nopass, private :: encode_base64_dataarray1_rank1_R4P !< Encode (Base64) dataarray, 1 data of rank 1 (R4P).
    procedure, nopass, private :: encode_base64_dataarray1_rank1_I8P !< Encode (Base64) dataarray, 1 data of rank 1 (I8P).
    procedure, nopass, private :: encode_base64_dataarray1_rank1_I4P !< Encode (Base64) dataarray, 1 data of rank 1 (I4P).
    procedure, nopass, private :: encode_base64_dataarray1_rank1_I2P !< Encode (Base64) dataarray, 1 data of rank 1 (I2P).
    procedure, nopass, private :: encode_base64_dataarray1_rank1_I1P !< Encode (Base64) dataarray, 1 data of rank 1 (I1P).
    procedure, nopass, private :: encode_base64_dataarray1_rank2_R8P !< Encode (Base64) dataarray, 1 data of rank 2 (R8P).
    procedure, nopass, private :: encode_base64_dataarray1_rank2_R4P !< Encode (Base64) dataarray, 1 data of rank 2 (R4P).
    procedure, nopass, private :: encode_base64_dataarray1_rank2_I8P !< Encode (Base64) dataarray, 1 data of rank 2 (I8P).
    procedure, nopass, private :: encode_base64_dataarray1_rank2_I4P !< Encode (Base64) dataarray, 1 data of rank 2 (I4P).
    procedure, nopass, private :: encode_base64_dataarray1_rank2_I2P !< Encode (Base64) dataarray, 1 data of rank 2 (I2P).
    procedure, nopass, private :: encode_base64_dataarray1_rank2_I1P !< Encode (Base64) dataarray, 1 data of rank 2 (I1P).
    procedure, nopass, private :: encode_base64_dataarray1_rank3_R8P !< Encode (Base64) dataarray, 1 data of rank 3 (R8P).
    procedure, nopass, private :: encode_base64_dataarray1_rank3_R4P !< Encode (Base64) dataarray, 1 data of rank 3 (R4P).
    procedure, nopass, private :: encode_base64_dataarray1_rank3_I8P !< Encode (Base64) dataarray, 1 data of rank 3 (I8P).
    procedure, nopass, private :: encode_base64_dataarray1_rank3_I4P !< Encode (Base64) dataarray, 1 data of rank 3 (I4P).
    procedure, nopass, private :: encode_base64_dataarray1_rank3_I2P !< Encode (Base64) dataarray, 1 data of rank 3 (I2P).
    procedure, nopass, private :: encode_base64_dataarray1_rank3_I1P !< Encode (Base64) dataarray, 1 data of rank 3 (I1P).
    procedure, nopass, private :: encode_base64_dataarray1_rank4_R8P !< Encode (Base64) dataarray, 1 data of rank 4 (R8P).
    procedure, nopass, private :: encode_base64_dataarray1_rank4_R4P !< Encode (Base64) dataarray, 1 data of rank 4 (R4P).
    procedure, nopass, private :: encode_base64_dataarray1_rank4_I8P !< Encode (Base64) dataarray, 1 data of rank 4 (I8P).
    procedure, nopass, private :: encode_base64_dataarray1_rank4_I4P !< Encode (Base64) dataarray, 1 data of rank 4 (I4P).
    procedure, nopass, private :: encode_base64_dataarray1_rank4_I2P !< Encode (Base64) dataarray, 1 data of rank 4 (I2P).
    procedure, nopass, private :: encode_base64_dataarray1_rank4_I1P !< Encode (Base64) dataarray, 1 data of rank 4 (I1P).
    procedure, nopass, private :: encode_base64_dataarray3_rank1_R8P !< Encode (Base64) dataarray, 3 data of rank 1 (R8P).
    procedure, nopass, private :: encode_base64_dataarray3_rank1_R4P !< Encode (Base64) dataarray, 3 data of rank 1 (R4P).
    procedure, nopass, private :: encode_base64_dataarray3_rank1_I8P !< Encode (Base64) dataarray, 3 data of rank 1 (I8P).
    procedure, nopass, private :: encode_base64_dataarray3_rank1_I4P !< Encode (Base64) dataarray, 3 data of rank 1 (I4P).
    procedure, nopass, private :: encode_base64_dataarray3_rank1_I2P !< Encode (Base64) dataarray, 3 data of rank 1 (I2P).
    procedure, nopass, private :: encode_base64_dataarray3_rank1_I1P !< Encode (Base64) dataarray, 3 data of rank 1 (I1P).
    procedure, nopass, private :: encode_base64_dataarray3_rank3_R8P !< Encode (Base64) dataarray, 3 data of rank 3 (R8P).
    procedure, nopass, private :: encode_base64_dataarray3_rank3_R4P !< Encode (Base64) dataarray, 3 data of rank 3 (R4P).
    procedure, nopass, private :: encode_base64_dataarray3_rank3_I8P !< Encode (Base64) dataarray, 3 data of rank 3 (I8P).
    procedure, nopass, private :: encode_base64_dataarray3_rank3_I4P !< Encode (Base64) dataarray, 3 data of rank 3 (I4P).
    procedure, nopass, private :: encode_base64_dataarray3_rank3_I2P !< Encode (Base64) dataarray, 3 data of rank 3 (I2P).
    procedure, nopass, private :: encode_base64_dataarray3_rank3_I1P !< Encode (Base64) dataarray, 3 data of rank 3 (I1P).
