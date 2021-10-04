class zcl_text2tab_serializer definition
  public
  final
  create public .

  public section.

    types:
      ty_decimal_sep type c length 1.

    types:
      tt_fields_list type standard table of abap_compname with default key.

    types:
      ts_fields_list type sorted table of abap_compname with unique key table_line.

    constants c_crlf like cl_abap_char_utilities=>cr_lf value cl_abap_char_utilities=>cr_lf. "#EC NOTEXT
    constants c_lf like cl_abap_char_utilities=>newline value cl_abap_char_utilities=>newline. "#EC NOTEXT
    constants c_tab like cl_abap_char_utilities=>horizontal_tab value cl_abap_char_utilities=>horizontal_tab. "#EC NOTEXT

    methods serialize
      importing
        !i_data type any
        !i_header_only type abap_bool default abap_false
        !i_fields_only type tt_fields_list optional
      returning
        value(r_string) type string
      raising
        zcx_text2tab_error .
    methods serialize_header_description
      importing
        !i_data type any
        !i_lang type sy-langu default sy-langu
        !i_fields_only type tt_fields_list optional
      returning
        value(r_string) type string
      raising
        zcx_text2tab_error .

    " CONSTRUCTION
    class-methods create
      importing
        !i_decimal_sep type ty_decimal_sep optional
        !i_date_format type zcl_text2tab_parser=>ty_date_format optional
        !i_max_frac_digits type i optional
        !i_use_lf type abap_bool default abap_false
        !i_add_header_descr type sy-langu optional " DEPRECATIED, MAY CHANGE SOON
      returning
        value(ro_serializer) type ref to zcl_text2tab_serializer
      raising
        zcx_text2tab_error .

  protected section.
  private section.

    data mv_decimal_sep type ty_decimal_sep .
    data mv_date_format type zcl_text2tab_parser=>ty_date_format .
    data mv_line_sep type string .
    data mv_max_frac_digits type i .
    data mv_current_field type string .
    data mv_line_index type i .
    data mv_add_header_descr type sy-langu.

    methods serialize_field
      importing
        !is_component type zcl_text2tab_utils=>ty_comp_descr
        !i_value type any
      returning
        value(r_out) type string
      raising
        zcx_text2tab_error .
    class-methods apply_conv_exit
      importing
        !i_in type any
        !i_convexit type abap_editmask
      returning
        value(r_out) type string
      exceptions
        conv_failed .
    class-methods serialize_date
      importing
        !i_date type datum
        !iv_date_format type zcl_text2tab_parser=>ty_date_format
      returning
        value(r_out) type string .
    class-methods validate_components
      importing
        !id_struc type ref to cl_abap_structdescr
      raising
        zcx_text2tab_error .
    class-methods serialize_header
      importing
        !it_components type zcl_text2tab_utils=>tt_comp_descr
        !i_fields_only type ts_fields_list optional
        !iv_add_header_tech type abap_bool default abap_true
        !iv_add_header_descr type abap_bool default abap_false
      changing
        !ct_lines type string_table .
    methods serialize_data
      importing
        !it_components type zcl_text2tab_utils=>tt_comp_descr
        !i_data type any table
        !i_fields_only type ts_fields_list optional
      changing
        !ct_lines type string_table
      raising
        zcx_text2tab_error .
    class-methods detect_type
      importing
        !i_data type any
      exporting
        !e_struc_type type ref to cl_abap_structdescr
        !e_data_kind like cl_abap_typedescr=>kind_struct
      raising
        zcx_text2tab_error .
ENDCLASS.



CLASS ZCL_TEXT2TAB_SERIALIZER IMPLEMENTATION.


  method apply_conv_exit.

    data l_fm_name type rs38l_fnam value 'CONVERSION_EXIT_XXXXX_OUTPUT'.

    replace first occurrence of 'XXXXX' in l_fm_name with i_convexit.
    if zcl_text2tab_utils=>function_exists( l_fm_name ) = abap_false.
      return.
    endif.

    data l_tmp type c length 40. " Potential bug, but string is padded at the end
    call function l_fm_name
      exporting
        input  = i_in
      importing
        output = l_tmp
      exceptions
        others = 1.

    if sy-subrc <> 0.
      raise conv_failed.
    endif.

    r_out = l_tmp.

  endmethod.  "apply_conv_exit


  method create.
    create object ro_serializer.

    if i_use_lf = abap_true.
      ro_serializer->mv_line_sep = c_lf.
    else.
      ro_serializer->mv_line_sep = c_crlf.
    endif.

    " Defaults
    ro_serializer->mv_decimal_sep     = '.'.
    ro_serializer->mv_date_format     = 'DMY.'.
    ro_serializer->mv_max_frac_digits = 5.

    " Not empty param and not empty decimal separator
    if i_decimal_sep ca '.,'.
      ro_serializer->mv_decimal_sep = i_decimal_sep.
    endif.

    if i_max_frac_digits is supplied and i_max_frac_digits between 0 and 22. " 22 ? constant from FLTP_CHAR_CONVERSION
      ro_serializer->mv_max_frac_digits = i_max_frac_digits.
    endif.

    " Not empty param and not empty decimal separator
    if i_date_format is not initial.
      zcl_text2tab_utils=>validate_date_format_spec( i_date_format ).
      ro_serializer->mv_date_format = i_date_format.
    endif.

    ro_serializer->mv_add_header_descr = i_add_header_descr.

  endmethod.


  method detect_type.

    data ld_type type ref to cl_abap_typedescr.
    data ld_table type ref to cl_abap_tabledescr.

    ld_type = cl_abap_typedescr=>describe_by_data( i_data ).
    if not ld_type->kind ca 'ST'. " Structure or table
      zcx_text2tab_error=>raise(
        msg  = 'i_data must be a Structure or a table'
        code = 'ST' ). "#EC NOTEXT
    endif.

    if ld_type->kind = cl_abap_typedescr=>kind_struct.
      e_struc_type ?= ld_type.
    else.
      ld_table     ?= ld_type.
      e_struc_type ?= ld_table->get_table_line_type( ).
    endif.

    e_data_kind = ld_type->kind.

  endmethod.


  method serialize.

    " Detect types
    data ld_struc type ref to cl_abap_structdescr.
    data lv_data_kind like cl_abap_typedescr=>kind_struct.

    detect_type(
      exporting
        i_data = i_data
      importing
        e_struc_type = ld_struc
        e_data_kind  = lv_data_kind ).
    validate_components( ld_struc ).

    " Get components
    data lt_fields_only_sorted type ts_fields_list.
    data lt_components type zcl_text2tab_utils=>tt_comp_descr.

    lt_fields_only_sorted = i_fields_only.
    lt_components = zcl_text2tab_utils=>describe_struct(
      i_struc              = ld_struc
      i_with_descr_in_lang = mv_add_header_descr ).

    " Unify containers
    data lr_datatab type ref to data.
    field-symbols <data> type any table.

    if lv_data_kind = cl_abap_typedescr=>kind_struct.
      lr_datatab = zcl_text2tab_utils=>create_standard_table_of( ld_struc ).
      assign lr_datatab->* to <data>.
      insert i_data into table <data>.
    else.
      assign i_data to <data>.
    endif.

    " serialize header / collect in string table
    data lt_lines type string_table.
    serialize_header(
      exporting
        it_components      = lt_components
        i_fields_only      = lt_fields_only_sorted
        iv_add_header_tech = abap_true
        iv_add_header_descr = boolc( mv_add_header_descr is not initial )
      changing
        ct_lines = lt_lines ).

    " serialize data
    if i_header_only = abap_false.
      serialize_data(
        exporting
          it_components = lt_components
          i_data        = <data>
          i_fields_only = lt_fields_only_sorted
        changing
          ct_lines = lt_lines ).
    endif.

    r_string = concat_lines_of(
      table = lt_lines
      sep   = mv_line_sep ).

  endmethod.


  method serialize_data.
    data lt_fields type string_table.
    data lv_buf type string.
    data lv_limit_fields type abap_bool.

    field-symbols <record> type any.
    field-symbols <field>  type any.
    field-symbols <c> like line of it_components.

    lv_limit_fields = boolc( lines( i_fields_only ) > 0 ).

    " Serialization loop
    loop at i_data assigning <record>.
      mv_line_index = sy-tabix.
      clear lt_fields.
      loop at it_components assigning <c>.
        assign component sy-tabix of structure <record> to <field>.
        assert sy-subrc = 0.
        if lv_limit_fields = abap_true.
          read table i_fields_only transporting no fields with key table_line = <c>-name.
          check sy-subrc = 0. " continue if not found
        endif.
        mv_current_field = <c>-name.
        lv_buf = serialize_field(
          is_component = <c>
          i_value      = <field> ).
        append lv_buf to lt_fields.
      endloop.
      lv_buf = concat_lines_of( table = lt_fields sep = c_tab ).
      append lv_buf to ct_lines.
    endloop.

  endmethod.


  method serialize_date.
    data:
          l_iter    type i,
          l_part    type c,
          l_sep     type c.

    if i_date is initial or i_date = ''. " 00.. is initial, '' is not !
      return. " Empty date -> empty string
    endif.
    l_sep = iv_date_format+3(1).

    do 3 times.
      l_iter = sy-index - 1.
      l_part = iv_date_format+l_iter(1).

      case l_part.
        when 'D'.
          r_out = r_out && i_date+6(2).
        when 'M'.
          r_out = r_out && i_date+4(2).
        when 'Y'.
          r_out = r_out && i_date+0(4).
        when others.
          " assuming date format was validated ...
      endcase.

      if l_sep is not initial and l_iter < 2.
        r_out = r_out && l_sep.
      endif.
    enddo.
  endmethod.


  method serialize_field.
    data:
          l_tmp type c length 40.

    case is_component-type_kind.

      when cl_abap_typedescr=>typekind_char. " Char + Alpha
        if is_component-edit_mask is initial.
          r_out = i_value.
        else.
          apply_conv_exit(
            exporting
              i_in       = i_value
              i_convexit = is_component-edit_mask
            receiving
              r_out = r_out
            exceptions others = 1 ).
          if sy-subrc is not initial.
            zcx_text2tab_error=>raise(
              msg      = |convexit failed for "{ i_value }"|
              location = |{ mv_current_field }@{ mv_line_index }|
              code     = 'CF' ). "#EC NOTEXT
          endif.
        endif.

      when cl_abap_typedescr=>typekind_date. " Date
        r_out = serialize_date(
          i_date         = i_value
          iv_date_format = mv_date_format ).

      when cl_abap_typedescr=>typekind_int
        or cl_abap_typedescr=>typekind_int1
        or cl_abap_typedescr=>typekind_int2. " Integer
        l_tmp = i_value.
        shift l_tmp left deleting leading space. " In case it is the last one
        r_out = l_tmp.

      when cl_abap_typedescr=>typekind_packed. " Amount
        l_tmp = i_value.
        shift l_tmp left deleting leading space. " strange space at the end if move to string
        if mv_decimal_sep <> '.'.
          replace first occurrence of '.' in l_tmp with mv_decimal_sep.
        endif.
        replace first occurrence of '-' in l_tmp with ''.
        if sy-subrc = 0.
          concatenate '-' l_tmp into l_tmp.
        endif.
        r_out = l_tmp.

      when cl_abap_typedescr=>typekind_float. " Float
        " Hmmmm ... improve ?
        write i_value to l_tmp right-justified exponent 0 decimals mv_max_frac_digits.
        shift l_tmp right deleting trailing '0'.
        replace first occurrence of ',' in l_tmp with '.'. " Can be ',' depending on user fmt ?
        shift l_tmp right deleting trailing '.'. " In case it is the last one
        shift l_tmp left deleting leading space. " In case it is the last one
        if mv_decimal_sep <> '.'.
          replace first occurrence of '.' in l_tmp with mv_decimal_sep.
        endif.
        r_out = l_tmp.

      when others.
        r_out = i_value.

    endcase.

  endmethod.


  method serialize_header.
    data lt_fields type string_table.
    data lt_fields_descr type string_table.
    data lv_limit_fields type abap_bool.
    data lv_buf type string.
    field-symbols <c> like line of it_components.

    lv_limit_fields = boolc( lines( i_fields_only ) > 0 ).

    loop at it_components assigning <c>.
      if lv_limit_fields = abap_true.
        read table i_fields_only transporting no fields with key table_line = <c>-name.
        check sy-subrc = 0. " continue if not found
      endif.
      append <c>-name to lt_fields.
      if iv_add_header_descr = abap_true.
        append <c>-description to lt_fields_descr.
      endif.
    endloop.

    if iv_add_header_descr = abap_true.
      lv_buf = concat_lines_of(
        table = lt_fields_descr
        sep   = c_tab ).
      append lv_buf to ct_lines.
    endif.

    if iv_add_header_tech = abap_true.
      lv_buf = concat_lines_of(
        table = lt_fields
        sep   = c_tab ).
      append lv_buf to ct_lines.
    endif.

  endmethod.


  method serialize_header_description.

    " Detect types
    data ld_struc type ref to cl_abap_structdescr.

    detect_type(
      exporting
        i_data = i_data
      importing
        e_struc_type = ld_struc ).
    validate_components( ld_struc ).

    " Get components
    data lt_fields_only_sorted type ts_fields_list.
    data lt_components type zcl_text2tab_utils=>tt_comp_descr.

    lt_fields_only_sorted = i_fields_only.
    lt_components = zcl_text2tab_utils=>describe_struct(
      i_struc              = ld_struc
      i_with_descr_in_lang = i_lang ).

    " serialize header / collect in string table
    data lt_lines type string_table.
    serialize_header(
      exporting
        it_components      = lt_components
        i_fields_only      = lt_fields_only_sorted
        iv_add_header_tech = abap_false
        iv_add_header_descr = abap_true
      changing
        ct_lines = lt_lines ).

    r_string = concat_lines_of(
      table = lt_lines
      sep   = mv_line_sep ).

  endmethod.


  method validate_components.
    " check if all components are elementary
    field-symbols <c> like line of id_struc->components.
    loop at id_struc->components assigning <c>.
      if id_struc->get_component_type( <c>-name )->kind <> cl_abap_typedescr=>kind_elem.
        zcx_text2tab_error=>raise(
          msg  = 'i_data line should contain only fields of elementary types'
          code = 'ET' ). "#EC NOTEXT
      endif.
    endloop.

  endmethod.
ENDCLASS.
