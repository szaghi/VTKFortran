    ! Write-tag-related methods of VTK file class.
    procedure, pass(self), private :: write_self_closing_tag       !< Write `<tag_name.../>` self closing tag.
    procedure, pass(self), private :: write_tag                    !< Write `<tag_name...>...</tag_name>` tag.
    procedure, pass(self), private :: write_start_tag              !< Write `<tag_name...>` start tag.
    procedure, pass(self), private :: write_end_tag                !< Write `</tag_name>` end tag.
    procedure, pass(self), private :: write_header_tag             !< Write header tag.
    procedure, pass(self), private :: write_topology_tag           !< Write topology tag.
    procedure, pass(self), private :: write_dataarray_tag          !< Write `<DataArray...>...</DataArray>` tag.
    procedure, pass(self), private :: write_dataarray_tag_appended !< Write `<DataArray.../>` appended tag.
    procedure, pass(self), private :: write_dataarray_appended     !< Write `<AppendedData...>...</AppendedData>` tag.
