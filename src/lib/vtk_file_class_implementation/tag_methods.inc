    ! Tag-related methods of VTK file class.
    procedure, pass(self), private :: self_closing_tag !< Return `<tag_name.../>` self closing tag.
    procedure, pass(self), private :: tag              !< Return `<tag_name...>...</tag_name>` tag.
    procedure, pass(self), private :: start_tag        !< Return `<tag_name...>` start tag.
    procedure, pass(self), private :: end_tag          !< Return `</tag_name>` end tag.
