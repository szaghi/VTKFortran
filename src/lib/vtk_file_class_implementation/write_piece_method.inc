    ! Write piece method of VTK file class.
    generic                        :: write_piece =>         &
                                      write_piece_start_tag, &
                                      write_piece_end_tag   !< Write Piece start/end tag.
    procedure, pass(self), private :: write_piece_start_tag !< Write `<Piece ...>` start tag.
    procedure, pass(self), private :: write_piece_end_tag   !< Write `</Piece>` end tag.
