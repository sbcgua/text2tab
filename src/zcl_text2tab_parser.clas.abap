class zcl_text2tab_parser definition
  public
  final
  create public .

  public section.
    type-pools abap .

    types:
      ty_amount_format type c length 2.
    types:
      ty_date_format type c length 4.
    types:
      tt_field_map type standard table of i with default key.

    constants c_tab like cl_abap_char_utilities=>horizontal_tab value cl_abap_char_utilities=>horizontal_tab. "#EC NOTEXT
    constants c_crlf like cl_abap_char_utilities=>cr_lf value cl_abap_char_utilities=>cr_lf. "#EC NOTEXT
    constants c_lf like cl_abap_char_utilities=>newline value cl_abap_char_utilities=>newline. "#EC NOTEXT

    class-methods version
      returning
        value(r_version) type string .
    class-methods check_version_fits
      importing
        !i_required_version type string
      returning
        value(r_fits) type abap_bool .
    class-methods create
      importing
        !i_pattern type any         " target structure or table
        !i_ignore_nonflat type abap_bool default abap_false
        !i_amount_format  type ty_amount_format optional
        !i_date_format    type ty_date_format optional
        !i_begin_comment  type zif_text2tab=>ty_begin_comment optional
        !i_deep_provider  type ref to zif_text2tab_deep_provider optional
      returning
        value(ro_parser) type ref to zcl_text2tab_parser
      raising
        zcx_text2tab_error .
    class-methods create_typeless
      returning
        value(ro_parser) type ref to zcl_text2tab_parser
      raising
        zcx_text2tab_error .
    methods parse
      importing
        !i_data type string
        !i_strict type abap_bool default abap_true
        !i_corresponding type abap_bool default abap_false
        !i_has_head type abap_bool default abap_true
        !i_rename_fields type any optional
      exporting
        !e_container type any
        !e_head_fields type string_table
      raising
        zcx_text2tab_error .
    methods ignore_conv_exit " Beta ! May be subject to change
      importing
        !i_convexit type abap_editmask
      returning
        value(ro_parser) type ref to zcl_text2tab_parser.

  protected section.
  private section.

    data mv_amount_format type ty_amount_format .
    data mv_date_format type ty_date_format .
    data mo_struc_descr type ref to cl_abap_structdescr .
    data mv_current_field type string .
    data mv_line_index type i .
    data mv_is_typeless type abap_bool .
    data mv_begin_comment type zif_text2tab=>ty_begin_comment .
    data mt_ignore_exits type sorted table of abap_editmask with unique key table_line.

    data mt_components type zcl_text2tab_utils=>tt_comp_descr .
    data mi_deep_provider type ref to zif_text2tab_deep_provider .

    methods parse_typefull
      importing
        !i_data type string
        !i_strict type abap_bool default abap_true
        !i_corresponding type abap_bool default abap_false
        !i_has_head type abap_bool default abap_true
        !i_rename_map type zcl_text2tab_utils=>th_field_name_map
      exporting
        !e_container type any
        !e_head_fields type string_table
      raising
        zcx_text2tab_error .
    methods parse_typeless
      importing
        !i_data type string
        !i_rename_map type zcl_text2tab_utils=>th_field_name_map
      exporting
        !e_container type ref to data
        !e_head_fields type string_table
      raising
        zcx_text2tab_error .
    methods parse_head_line
      importing
        !i_strict type abap_bool
        !i_corresponding type abap_bool
        !i_rename_map type zcl_text2tab_utils=>th_field_name_map
      changing
        !ct_data type string_table
        !ct_map type tt_field_map
        !ct_head_fields type string_table
      raising
        zcx_text2tab_error .
    methods map_head_structure
      importing
        !i_header type string
        !i_strict type abap_bool
        !i_corresponding type abap_bool
        !i_rename_map type zcl_text2tab_utils=>th_field_name_map
      exporting
        !et_map type tt_field_map
        !et_head_fields type string_table
      raising
        zcx_text2tab_error .
    methods parse_data
      importing
        !it_data type string_table
        !it_map type tt_field_map
      exporting
        !e_container type any
      raising
        zcx_text2tab_error .
    methods parse_line
      importing
        !i_dataline type string
        !it_map type tt_field_map
      exporting
        !es_container type any
      raising
        zcx_text2tab_error .
    methods parse_field
      importing
        !is_component type zcl_text2tab_utils=>ty_comp_descr
        !i_value type string
      exporting
        !e_field type any
      raising
        zcx_text2tab_error .
    methods parse_float
      importing
        !i_value type string
        !i_decimals type abap_compdescr-decimals
      exporting
        !e_field type any
      raising
        zcx_text2tab_error .
    methods parse_date
      importing
        !i_value type string
      returning
        value(r_date) type d
      raising
        zcx_text2tab_error .
    methods parse_time
      importing
        !i_value type string
      returning
        value(r_time) type t
      raising
        zcx_text2tab_error .
    methods apply_conv_exit
      importing
        !i_value type string
        !i_convexit type abap_editmask
      exporting
        !e_field type any
      raising
        zcx_text2tab_error .
    methods raise_error
      importing
        !i_msg type string
        !i_code type zcx_text2tab_error=>ty_rc optional
      raising
        zcx_text2tab_error .
ENDCLASS.



CLASS ZCL_TEXT2TAB_PARSER IMPLEMENTATION.


  method apply_conv_exit.

    data l_fm_name type tfdir-funcname value 'CONVERSION_EXIT_XXXXX_INPUT'.
    data l_message type string.

    if lines( mt_ignore_exits ) > 0.
      read table mt_ignore_exits with key table_line = i_convexit transporting no fields.
      if sy-subrc = 0.
        e_field = i_value.
        return.
      endif.
    endif.

    replace first occurrence of 'XXXXX' in l_fm_name with i_convexit.
    if zcl_text2tab_utils=>function_exists( l_fm_name ) = abap_false.
      raise_error( i_msg = 'Conversion exit not found' i_code = 'EM' ). "#EC NOTEXT
    endif.

    call function l_fm_name
      exporting
        input  = i_value
      importing
        output = e_field
      exceptions
        others = 1.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into l_message.
      raise_error( i_msg = l_message i_code = 'EF' ). "#EC NOTEXT
    endif.

  endmethod.


  method check_version_fits.

    r_fits = zcl_text2tab_utils=>check_version_fits(
      i_current_version  = zif_text2tab=>version
      i_required_version = i_required_version ).

  endmethod.


  method create.

    create object ro_parser.

    ro_parser->mv_amount_format  = ' ,'.   " Defaults
    ro_parser->mv_date_format    = 'DMY.'. " Defaults
    ro_parser->mo_struc_descr    = zcl_text2tab_utils=>get_safe_struc_descr( i_pattern ).
    ro_parser->mi_deep_provider  = i_deep_provider.
    ro_parser->mt_components     = zcl_text2tab_utils=>describe_struct(
      i_struc          = ro_parser->mo_struc_descr
      i_is_deep        = boolc( i_deep_provider is bound )
      i_ignore_nonflat = i_ignore_nonflat ).

    " Not empty param and not empty decimal separator
    if not ( i_amount_format is initial or i_amount_format+1(1) is initial ).
      ro_parser->mv_amount_format = i_amount_format.
    endif.

    " Not empty param and not empty decimal separator
    if i_date_format is not initial.
      zcl_text2tab_utils=>validate_date_format_spec( i_date_format ).
      ro_parser->mv_date_format = i_date_format.
    endif.

    ro_parser->mv_begin_comment = i_begin_comment.

  endmethod.


  method create_typeless.
    create object ro_parser.
    ro_parser->mv_is_typeless = abap_true.
  endmethod.


  method ignore_conv_exit.
    insert i_convexit into table mt_ignore_exits.
    ro_parser = me.
  endmethod.


  method map_head_structure.
    data:
          l_field_cnt  type i,
          l_mandt_cnt  type i,
          l_tab_cnt    type i,
          lt_dupcheck  type string_table.

    clear: et_map, et_head_fields.
    assert not ( i_strict = abap_true and mv_is_typeless = abap_true ).
    assert not ( i_strict = abap_true and i_corresponding = abap_true ).

    field-symbols <field> type string.

    split i_header at c_tab into table et_head_fields.
    l_field_cnt = lines( et_head_fields ).

    " Check if the line ends with TAB
    find all occurrences of c_tab in i_header match count l_tab_cnt.
    if l_tab_cnt = l_field_cnt. " Line ends with TAB, last empty field is not added to table, see help for 'split'
      raise_error( i_msg = 'Empty field at the end' i_code = 'EE' ).   "#EC NOTEXT
    endif.

    " Compare number of fields, check structure similarity
    if i_strict = abap_true.
      read table mt_components with key name = 'MANDT' transporting no fields.
      if sy-subrc is initial. " Found in structure components
        read table et_head_fields with key table_line = 'MANDT' transporting no fields.
        if sy-subrc is not initial. " But not found in the file
          l_mandt_cnt = 1. " MANDT field may be skipped
        endif.
      endif.

      if l_field_cnt + l_mandt_cnt <> lines( mt_components ).
        raise_error( i_msg = 'Different columns number' i_code = 'CN' ).   "#EC NOTEXT
      endif.
    endif.

    " Check duplicate field names in incoming structure
    lt_dupcheck = et_head_fields.
    sort lt_dupcheck.
    delete adjacent duplicates from lt_dupcheck.
    if lines( lt_dupcheck ) <> l_field_cnt.
      raise_error( i_msg = 'Duplicate field names found' i_code = 'DN' ).   "#EC NOTEXT
    endif.

    " Check duplicates after rename
    data ls_rename like line of i_rename_map.
    if i_rename_map is not initial.
      loop at et_head_fields assigning <field>.
        read table i_rename_map with key from = <field> into ls_rename.
        check sy-subrc is initial.
        <field> = ls_rename-to.
      endloop.

      lt_dupcheck = et_head_fields.
      sort lt_dupcheck.
      delete adjacent duplicates from lt_dupcheck.
      if lines( lt_dupcheck ) <> l_field_cnt.
        raise_error( i_msg = 'Duplicate field names found after rename' i_code = 'DR' ).   "#EC NOTEXT
      endif.
    endif.

    " Compare columns names and make map
    field-symbols <component> like line of mt_components.
    loop at et_head_fields assigning <field>.
      if <field> is initial. " Check empty fields
        raise_error( i_msg = 'Empty field name found' i_code = 'EN' ).   "#EC NOTEXT
      endif.
      " ~ following CL_ABAP_STRUCTDESCR->CHECK_COMPONENT_TABLE, non-strict mode characters included
      if strlen( <field> ) > abap_max_comp_name_ln or <field> cn 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789#$%&*-/;<=>?@^{|}'.
        raise_error( i_msg = 'Incorrect field name (long or special chars used)' i_code = 'WE' ). "#EC NOTEXT
      endif.
      if mv_is_typeless = abap_false.
        read table mt_components with key name = <field> assigning <component>.
        if sy-subrc is initial.
          if <component>-ignore = abap_false.
            append sy-tabix to et_map.
          else.
            raise_error( i_msg = |Cannot map to ignored field { <field> }| i_code = 'IG' ). "#EC NOTEXT
          endif.
        else.
          if i_corresponding = abap_true.
            append -1 to et_map. " Skip this field later
          else.
            raise_error( i_msg = |Field { <field> } not found in structure| i_code = 'MC' ). "#EC NOTEXT
          endif.
        endif.
      else.
        append sy-tabix to et_map. " direct map
      endif.
    endloop.

  endmethod.


  method parse.

    data lt_rename_map type zcl_text2tab_utils=>th_field_name_map.
    lt_rename_map = zcl_text2tab_utils=>build_rename_map( i_rename_fields ).

    if mv_is_typeless = abap_true.
      if cl_abap_typedescr=>describe_by_data( e_container )->type_kind <> cl_abap_typedescr=>typekind_dref.
        raise_error( i_msg = 'Typeless parsing require dref as the container'  i_code = 'DR' ). "#EC NOTEXT
      endif.

      parse_typeless(
        exporting
          i_data       = i_data
          i_rename_map = lt_rename_map
        importing
          e_container   = e_container
          e_head_fields = e_head_fields ).
    else.
      parse_typefull(
        exporting
          i_data       = i_data
          i_has_head   = i_has_head
          i_strict     = i_strict
          i_rename_map = lt_rename_map
          i_corresponding = i_corresponding
        importing
          e_container   = e_container
          e_head_fields = e_head_fields ).
    endif.

  endmethod.


  method parse_data.

    data:
          l_container_kind like cl_abap_typedescr=>kind,
          ref_tab_line     type ref to data.

    field-symbols:
                   <dataline> type string,
                   <table>    type any table,
                   <record>   type any.

    clear e_container.

    " Identify container type and Create temp container record
    l_container_kind = cl_abap_typedescr=>describe_by_data( e_container )->kind.
    create data ref_tab_line type handle mo_struc_descr.
    assign ref_tab_line->* to <record>.
    if l_container_kind = 'T'. " Table
      assign e_container   to <table>.
    endif.

    " Main parsing loop
    loop at it_data assigning <dataline>.
      mv_line_index = sy-tabix.

      if <dataline> is initial. " Check empty lines
        check mv_line_index < lines( it_data ). " Last line of a file may be empty, others - not
        raise_error( i_msg = 'Empty line cannot be parsed'  i_code = 'LE' ). "#EC NOTEXT
      endif.

      parse_line(
        exporting
          i_dataline     = <dataline>
          it_map         = it_map
        importing
          es_container   = <record> ).

      if l_container_kind = 'T'. " Table
        insert <record> into table <table>.
      else.                      " Structure
        e_container = <record>.
        exit. " Only first line goes to structure and then exits
      endif.

    endloop.

  endmethod.


  method parse_date.

    data: l_cursor  type i,
          l_iter    type i,
          l_part    type c,
          l_size    type i,
          l_offs    type i,
          l_home    type i,
          l_pad     type i,
          l_stencil type n length 4,
          l_rawdate type c length 8,
          l_charset type c length 11 value '0123456789',
          l_sep     type c.

    l_sep           = mv_date_format+3(1).
    l_charset+10(1) = l_sep.

    if i_value is initial or i_value co ` `. " Empty string -> empty date
      return.
    endif.

    if not i_value co l_charset.  " Check wrong symbols
      raise_error( i_msg = 'Date contains invalid symbols' i_code = 'DY' ). "#EC NOTEXT
    endif.

    " Not separated date must be 8 chars, separated not more than 10
    if l_sep <> space and strlen( i_value ) > 10  or l_sep = space and strlen( i_value ) <> 8.
      raise_error( i_msg = 'Incorrect date length' i_code = 'DL' ). "#EC NOTEXT
    endif.

    do 3 times.
      l_iter = sy-index - 1.
      l_part = mv_date_format+l_iter(1).

      case l_part.
        when 'D'.
          l_size = 2.
          l_home = 6.
        when 'M'.
          l_size = 2.
          l_home = 4.
        when 'Y'.
          l_size = 4.
          l_home = 0.
        when others.
          raise_error( i_msg = 'Wrong date format' ). "#EC NOTEXT
      endcase.

      if l_sep is initial. " No seps
        l_rawdate+l_home(l_size) = i_value+l_cursor(l_size).
        l_cursor                 = l_cursor + l_size.
      else.
        if l_iter = 2. " Last part
          l_offs = strlen( i_value+l_cursor ).
        else.
          find first occurrence of l_sep in i_value+l_cursor match offset l_offs.
        endif.
        if sy-subrc <> 0.
          raise_error( i_msg = 'Date separator is missing' i_code = 'DS' ). "#EC NOTEXT
        endif.
        if l_offs > l_size.
          raise_error( i_msg = 'Too long date part' i_code = 'DP' ). "#EC NOTEXT
        endif.
        l_stencil                = i_value+l_cursor(l_offs).
        l_pad                    = 4 - l_size. " Offset within stencil
        l_rawdate+l_home(l_size) = l_stencil+l_pad(l_size).
        l_cursor                 = l_cursor + l_offs + 1. " Including separator
      endif.

    enddo.

    " Native convert
    try.
      cl_abap_datfm=>conv_date_ext_to_int(
        exporting
          im_datext   = l_rawdate
          im_datfmdes = '4' " YYYY.MM.DD
        importing
          ex_datint   = r_date ).
    catch cx_abap_datfm.
      raise_error( i_msg = 'Date format unknown' i_code = 'DU' ). "#EC NOTEXT
    endtry.

  endmethod.


  method parse_field.

    data: l_unquoted type string,
          l_len      type i.

    clear e_field.

    " Unquote field
    l_len = strlen( i_value ).
    if l_len >= 2
       and substring( val = i_value off = 0         len = 1 ) = '"'
       and substring( val = i_value off = l_len - 1 len = 1 ) = '"'.
      l_unquoted = substring( val = i_value off = 1 len = l_len - 2 ).
    else.
      l_unquoted = i_value.
    endif.
    clear l_len.

    " Parse depending on output type
    case is_component-type_kind.
      when cl_abap_typedescr=>typekind_date. " Date
        e_field = parse_date( l_unquoted ).

      when cl_abap_typedescr=>typekind_char. " Char + Alpha
        if is_component-output_length < strlen( l_unquoted ).
          raise_error( i_msg = 'Value is longer than field' i_code = 'FS' ). "#EC NOTEXT
        endif.

        if is_component-edit_mask is initial.
          e_field = l_unquoted.
        else.
          apply_conv_exit(
            exporting
              i_value    = l_unquoted
              i_convexit = is_component-edit_mask
            importing
              e_field    = e_field ).
        endif.

      when cl_abap_typedescr=>typekind_string. " String
        e_field = l_unquoted.

      when cl_abap_typedescr=>typekind_packed. " Amount
        parse_float(
          exporting
            i_value    = l_unquoted
            i_decimals = is_component-decimals
          importing
            e_field    = e_field ).

      when cl_abap_typedescr=>typekind_decfloat16.
        parse_float(
          exporting
            i_value    = l_unquoted
            i_decimals = 16
          importing
            e_field    = e_field ).

      when cl_abap_typedescr=>typekind_decfloat34.
        parse_float(
          exporting
            i_value    = l_unquoted
            i_decimals = 34
          importing
            e_field    = e_field ).

      when cl_abap_typedescr=>typekind_float. " Float
        parse_float(
          exporting
            i_value    = l_unquoted
            i_decimals = 34 " Abap decimal digit limit ?
          importing
            e_field    = e_field ).

      when cl_abap_typedescr=>typekind_int
        or cl_abap_typedescr=>typekind_int1
        or cl_abap_typedescr=>typekind_int2. " Integer
        if l_unquoted co '0123456789'.
          e_field = l_unquoted.
        else.
          raise_error( i_msg = 'Field parsing failed' i_code = 'PF' ). "#EC NOTEXT
        endif.

      when cl_abap_typedescr=>typekind_time. " Time
        e_field = parse_time( l_unquoted ).

      when cl_abap_typedescr=>typekind_num. " Numchar
        if is_component-output_length < strlen( l_unquoted ).
          raise_error( i_msg = 'Value is longer than field' i_code = 'FS' ). "#EC NOTEXT
        endif.

        if l_unquoted co '0123456789'.
          e_field = l_unquoted.
        else.
          raise_error( i_msg = 'Field parsing failed' i_code = 'PF' ). "#EC NOTEXT
        endif.

      when cl_abap_typedescr=>typekind_hex. " Raw
        if is_component-length < strlen( l_unquoted ) / 2 + strlen( l_unquoted ) mod 2. " 2 hex-char per byte
          raise_error( i_msg = 'Value is longer than field' i_code = 'FS' ). "#EC NOTEXT
        endif.

        try .
          e_field = l_unquoted.
        catch cx_sy_conversion_no_raw cx_sy_conversion_error.
          raise_error( i_msg = 'Field parsing failed' i_code = 'PF' ). "#EC NOTEXT
        endtry.

      when others.
        raise_error( i_msg = 'Unsupported field type' i_code = 'UT' ). "#EC NOTEXT

    endcase.

  endmethod.


  method parse_float.

    data:
          l_decimal_sep  type c,
          l_thousand_sep type c,
          l_tmp          type string,
          l_count        type string,
          l_regex        type string.

    l_thousand_sep = mv_amount_format+0(1).
    l_decimal_sep  = mv_amount_format+1(1).
    clear e_field.

    if l_decimal_sep = '.'.
      try .
        e_field = i_value. " Try native format first - xxxx.xx
        return. " if successful - just return it
      catch cx_sy_arithmetic_error cx_sy_conversion_error.
        " Ignore, continue with custom parsing
      endtry.
    endif.

    l_tmp   = i_value.
    condense l_tmp no-gaps.
    if l_tmp is initial.
      return. " zero
    endif.

    l_regex = '^-?\d{1,3}(T\d{3})*(\D\d{1,C})?$'. "#EC NOTEXT
    if i_decimals > 0.
      replace 'C' in l_regex with |{ i_decimals }|.
    else.
      replace '1,C' in l_regex with '0'.
    endif.

    " Validate number
    find first occurrence of l_thousand_sep in l_tmp.
    if sy-subrc is initial. " Found
      replace 'T' in l_regex with l_thousand_sep.
    else.
      replace 'T' in l_regex with ''.
    endif.

    replace 'D' in l_regex with l_decimal_sep.
    find all occurrences of regex l_regex in l_tmp match count l_count.

    if l_count = 1.
      if not l_thousand_sep is initial.  " Remove thousand separators
        replace all occurrences of l_thousand_sep in l_tmp with ''.
      endif.

      if l_decimal_sep <> '.'.           " Replace decimal separator
        replace l_decimal_sep in l_tmp with '.'.
      endif.

      try. " Try converting again
        e_field = l_tmp.
      catch cx_sy_arithmetic_error cx_sy_conversion_error.
        raise_error( i_msg = 'Float number parsing failed' i_code = 'PF' ). "#EC NOTEXT
      endtry.
    else. " Not matched
      raise_error( i_msg = 'Float number parsing failed' i_code = 'PF' ). "#EC NOTEXT
    endif.

  endmethod.


  method parse_head_line.
    data l_header_str type string.

    read table ct_data into l_header_str index 1.
    if sy-subrc <> 0.
      raise_error( i_msg = 'Data empty' i_code = 'DE' ). "#EC NOTEXT
    endif.
    if l_header_str is initial.
      raise_error( i_msg = 'Header line is empty'  i_code = 'HE' ). "#EC NOTEXT
    endif.

    map_head_structure(
      exporting
        i_rename_map    = i_rename_map
        i_header        = to_upper( l_header_str )
        i_strict        = i_strict
        i_corresponding = i_corresponding
      importing
        et_map         = ct_map
        et_head_fields = ct_head_fields ).

    delete ct_data index 1.

  endmethod.


  method parse_line.

    data:
          lt_fields      type table of string,
          l_tab_cnt      type i,
          l_field_value  type string,
          ls_component   like line of mt_components,
          l_index        type i.

    field-symbols <field> type any.

    clear es_container.
    split i_dataline at c_tab into table lt_fields.

    " Count TABs, if line ends with TAB last empty field is not added to table, see help for 'split'
    find all occurrences of c_tab in i_dataline match count l_tab_cnt.
    l_tab_cnt = l_tab_cnt + 1. " Number of fields in the line

    " Check field number is the same as in header
    if l_tab_cnt > lines( it_map ).
      raise_error( i_msg = 'More fields than in header' i_code = '>H' ). "#EC NOTEXT
    elseif l_tab_cnt < lines( it_map ).
      raise_error( i_msg = 'Less fields than in header' i_code = '<H' ). "#EC NOTEXT
    endif.

    " Move data to table line
    loop at lt_fields into l_field_value.
      read table it_map into l_index index sy-tabix. " Read map
      if l_index = -1.
        continue. " corresponding parsing
      endif.

      read table mt_components into ls_component index l_index.  " Get component
      if sy-subrc is not initial.
        raise_error( 'No component found?!' ). "#EC NOTEXT
      endif.

      check ls_component-name <> 'MANDT'.   " Skip client fields
      mv_current_field = ls_component-name. " For error handling

      unassign <field>.
      assign component ls_component-name of structure es_container to <field>.
      if <field> is not assigned.
        raise_error( 'Field assign failed?!' ). "#EC NOTEXT
      endif.

      if mv_is_typeless = abap_true.
        <field> = l_field_value.
      elseif ls_component-type_kind = cl_abap_typedescr=>typekind_struct1
        or ls_component-type_kind = cl_abap_typedescr=>typekind_struct2
        or ls_component-type_kind = cl_abap_typedescr=>typekind_table.

        assert mi_deep_provider is bound.
        if l_field_value is not initial.
          mi_deep_provider->select(
            exporting
              i_address = l_field_value
              i_cursor  = es_container
            importing
              e_container = <field> ).
          " Potetial bug if key field is parsed AFTER deep field that references it
          " option 1 - just demand key fields before deep ones - look like normal constrain
          " option 2 - postpone parsing of deep fields till after all others were parsed
        endif.
      else.
        parse_field(
          exporting
            is_component = ls_component
            i_value      = l_field_value
          importing
            e_field      = <field> ).
      endif.

      clear mv_current_field. " For error handling - field is not processed any more
    endloop.

  endmethod.


  method parse_time.

    try.
      cl_abap_timefm=>conv_time_ext_to_int(
        exporting
          time_ext      = i_value
          is_24_allowed = abap_true
        importing
          time_int = r_time ).
    catch cx_abap_timefm_invalid.
      raise_error( i_msg = |{ i_value } is not a valid time| i_code = 'IT' ).
    endtry.

  endmethod.


  method parse_typefull.

    data:
          lt_data      type string_table,
          lt_map       type tt_field_map,
          ls_component like line of mt_components.

    clear: e_container, e_head_fields.
    clear: mv_line_index.

    " Validate params
    if i_has_head = abap_false and i_strict = abap_false.
      raise_error( i_msg = 'Header line mandatory for non-strict mode' i_code = 'WP' ). "#EC NOTEXT
    endif.

    if i_corresponding = abap_true and i_strict = abap_true.
      raise_error( i_msg = 'Cannot be strict and corresponding' i_code = 'WP' ). "#EC NOTEXT
    endif.

    if i_corresponding = abap_true and i_has_head = abap_false.
      raise_error( i_msg = 'Cannot be strict and has no head line' i_code = 'WP' ). "#EC NOTEXT
    endif.

    " Check container type
    if mo_struc_descr->absolute_name <> zcl_text2tab_utils=>get_safe_struc_descr( e_container )->absolute_name.
      raise_error( i_msg = 'Container type does not fit pattern' i_code = 'TE' ). "#EC NOTEXT
    endif.

    lt_data = zcl_text2tab_utils=>break_to_lines(
      i_text          = i_data
      i_begin_comment = mv_begin_comment ).

    " Read and process header line
    if i_has_head = abap_true.
      parse_head_line(
        exporting
          i_rename_map    = i_rename_map
          i_strict        = i_strict
          i_corresponding = i_corresponding
        changing
          ct_data        = lt_data
          ct_head_fields = e_head_fields
          ct_map         = lt_map ).
    else.
      loop at mt_components into ls_component.
        append sy-tabix to lt_map.
        append ls_component-name to e_head_fields.
      endloop.
    endif.

    " Do parsing
    parse_data(
      exporting
        it_data     = lt_data
        it_map      = lt_map
      importing
        e_container = e_container ).

  endmethod.


  method parse_typeless.
    data lt_data type string_table.
    data lt_map type tt_field_map.
    field-symbols <f> like line of e_head_fields.

    lt_data = zcl_text2tab_utils=>break_to_lines( i_text = i_data i_begin_comment = mv_begin_comment ).

    " Read and process header line
    parse_head_line(
      exporting
        i_rename_map    = i_rename_map
        i_strict        = abap_false
        i_corresponding = abap_false
      changing
        ct_data        = lt_data
        ct_head_fields = e_head_fields
        ct_map         = lt_map ).

    " Create dynamic structure
    data lt_components type abap_component_tab.
    data ls_comp like line of lt_components.
    data ld_struc type ref to cl_abap_structdescr.

    ls_comp-type ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
    loop at e_head_fields assigning <f>.
      ls_comp-name = <f>.
      append ls_comp to lt_components.
    endloop.

    ld_struc    = cl_abap_structdescr=>create( lt_components ).
    e_container = zcl_text2tab_utils=>create_standard_table_of( ld_struc ).

    " parse remaining data into the structure
    field-symbols <tab> type any.
    assign e_container->* to <tab>.
    mo_struc_descr = ld_struc. "TODO: hack, maybe improve
    mt_components  = zcl_text2tab_utils=>describe_struct(
      i_struc          = mo_struc_descr
      i_ignore_nonflat = abap_false ).
    parse_data(
      exporting
        it_data = lt_data
        it_map  = lt_map
      importing
        e_container = <tab> ).

  endmethod.


  method raise_error.

    data l_location   type string.
    data l_struc      type string.
    data lt_callstack type abap_callstack.
    data ls_callpoint like line of lt_callstack.

    call function 'SYSTEM_CALLSTACK'
      exporting
        max_level    = 2
      importing
        callstack = lt_callstack.

    read table lt_callstack into ls_callpoint index 2.

    if mo_struc_descr is bound.
      l_struc = mo_struc_descr->get_relative_name( ).
      if l_struc is not initial. " Format location
        l_location = l_struc.
        if mv_current_field is not initial.
          l_location = |{ l_location }-{ mv_current_field }|.
        endif.
        if mv_line_index is not initial.
          l_location = |{ l_location }@{ mv_line_index }|.
        endif.
      endif.
    endif.

    raise exception type zcx_text2tab_error
      exporting
        methname  = |{ ls_callpoint-blockname }|
        msg       = i_msg
        code      = i_code
        field     = mv_current_field
        line      = mv_line_index
        structure = l_struc
        location  = l_location.

  endmethod.


  method version.
    r_version = zif_text2tab=>version.
  endmethod.
ENDCLASS.
