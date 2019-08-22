class ZCL_TEXT2TAB_UTILS definition
  public
  final
  create public .

  public section.

    constants c_tab like cl_abap_char_utilities=>horizontal_tab value cl_abap_char_utilities=>horizontal_tab. "#EC NOTEXT
    constants c_crlf like cl_abap_char_utilities=>cr_lf value cl_abap_char_utilities=>cr_lf. "#EC NOTEXT
    constants c_lf like cl_abap_char_utilities=>newline value cl_abap_char_utilities=>newline. "#EC NOTEXT

    types:
      begin of ty_comp_descr.
        include type abap_compdescr.
        types:
        edit_mask type abap_editmask,
        output_length type i,
        ignore type abap_bool,
      end of ty_comp_descr .
    types:
      tt_comp_descr type standard table of ty_comp_descr with default key .
    types:
      begin of ty_deep_address,
        location  type string,
        key_field type abap_compname, " at source table
        ref_field type abap_compname, " at target (currently processed) table
        key_value type string,
      end of ty_deep_address.

    type-pools abap .
    class-methods function_exists
      importing
        !i_function_name type rs38l_fnam
      returning
        value(r_yes) type abap_bool .
    class-methods validate_date_format_spec
      importing
        !i_date_format type zcl_text2tab_parser=>ty_date_format
      raising
        zcx_text2tab_error .
    class-methods describe_struct
      importing
        !i_struc type ref to cl_abap_structdescr
        !i_ignore_nonflat type abap_bool default abap_false
      returning
        value(rt_descr) type tt_comp_descr
      raising
        zcx_text2tab_error .
    class-methods check_version_fits
      importing
        !i_required_version type string
        !i_current_version type string
      returning
        value(r_fits) type abap_bool .
    class-methods parse_deep_address
      importing
        !i_address type string
      returning
        value(rs_parsed) type ty_deep_address
      raising
        zcx_text2tab_error .
    class-methods get_struc_field_value_by_name
      importing
        !i_struc type any
        !i_field_name type abap_compname
      exporting
        !e_value type any
      raising
        zcx_text2tab_error .
    class-methods break_to_lines
      importing
        !i_text type string
        !i_begin_comment type c
      returning
        value(rt_tab) type string_table .
    class-methods get_safe_struc_descr
      importing
        !i_pattern type any
      returning
        value(ro_struc_descr) type ref to cl_abap_structdescr
      raising
        zcx_text2tab_error .

  protected section.
  private section.
    types:
      tts_checked_names type sorted table of rs38l_fnam with unique key table_line.
    class-data gt_checked_fm_names type tts_checked_names.

ENDCLASS.



CLASS ZCL_TEXT2TAB_UTILS IMPLEMENTATION.

  method break_to_lines.
    data:
      l_found type i,
      l_break type string value c_crlf.
    field-symbols: <line> type string.

    " Detect line break
    l_found = find( val = i_text sub = c_crlf ).
    if l_found < 0.
      l_found = find( val = i_text sub = c_lf ).
      if l_found >= 0.
        l_break = c_lf.
      endif.
    endif.

    split i_text at l_break into table rt_tab.

    if i_begin_comment <> space.
      loop at rt_tab assigning <line>.
        try.
            if <line>+0(1) = i_begin_comment.
              delete rt_tab index sy-tabix.
            endif.
          catch cx_sy_range_out_of_bounds.
            " if the row only consist of a linefeed. Some text editors add always a line feed at the end of the document
            delete rt_tab index sy-tabix.
        endtry.
      endloop.
    endif.

  endmethod.

  method check_version_fits.

    types:
      begin of ty_version,
        major type n length 4,
        minor type n length 4,
        patch type n length 4,
      end of ty_version.

    data ls_cur_ver type ty_version.
    data ls_req_ver type ty_version.
    data lv_buf type string.

    lv_buf = i_current_version.
    shift lv_buf left deleting leading 'v'.
    split lv_buf at '.' into ls_cur_ver-major ls_cur_ver-minor ls_cur_ver-patch.

    lv_buf = i_required_version.
    shift lv_buf left deleting leading 'v'.
    split lv_buf at '.' into ls_req_ver-major ls_req_ver-minor ls_req_ver-patch.

    if ls_req_ver-major <= ls_cur_ver-major.
      if ls_req_ver-minor <= ls_cur_ver-minor.
        if ls_req_ver-patch <= ls_cur_ver-patch.
          r_fits = abap_true.
        endif.
      endif.
    endif.

  endmethod.


  method describe_struct.

    field-symbols <c> like line of i_struc->components.
    field-symbols <descr> like line of rt_descr.
    data lo_data    type ref to cl_abap_datadescr.
    data lo_element type ref to cl_abap_elemdescr.

    loop at i_struc->components assigning <c>.
      append initial line to rt_descr assigning <descr>.
      move-corresponding <c> to <descr>.
      lo_data = i_struc->get_component_type( <c>-name ).
      if lo_data->kind = cl_abap_typedescr=>kind_elem.
        lo_element ?= lo_data.
        <descr>-output_length = lo_element->output_length.
        <descr>-edit_mask     = lo_element->edit_mask.
        shift <descr>-edit_mask left deleting leading '='.
      elseif i_ignore_nonflat = abap_true.
        <descr>-ignore = abap_true.
      else.
        zcx_text2tab_error=>raise(
          msg = 'Structure must be flat' "#EC NOTEXT
          code = 'SF' ).
      endif.
    endloop.

  endmethod.


  method function_exists.

    read table gt_checked_fm_names
      transporting no fields
      with key table_line = i_function_name
      binary search.

    if sy-subrc = 0. " found
      r_yes = abap_true.
      return.
    endif.

    call function 'FUNCTION_EXISTS'
      exporting
        funcname           = i_function_name
      exceptions
        function_not_exist = 1
        others             = 2.

    if sy-subrc = 0. " found, remember
      r_yes = abap_true.
      insert i_function_name into table gt_checked_fm_names.
    endif.

  endmethod.

  method get_safe_struc_descr.

    data:
          lo_type_descr  type ref to cl_abap_typedescr,
          lo_table_descr type ref to cl_abap_tabledescr.

    " Identify structure type
    lo_type_descr = cl_abap_typedescr=>describe_by_data( i_pattern ).
    case lo_type_descr->kind.
      when 'T'. " Table
        lo_table_descr ?= lo_type_descr.
        ro_struc_descr ?= lo_table_descr->get_table_line_type( ).
      when 'S'. " Structure
        ro_struc_descr ?= lo_type_descr.
      when others. " Not a table or structure ?
        raise exception type zcx_text2tab_error
          exporting
            methname = 'GET_SAFE_STRUC_DESCR'
            msg      = 'Table or structure patterns only' "#EC NOTEXT
            code     = 'PE'.
    endcase.

  endmethod.


  method get_struc_field_value_by_name.

    field-symbols <val> type any.

    assign component i_field_name of structure i_struc to <val>.
    if sy-subrc <> 0.
      raise exception type zcx_text2tab_error
        exporting
          methname = 'get_struc_field_value_by_name'
          msg      = |Field { i_field_name } not found in {
            cl_abap_typedescr=>describe_by_data( i_struc )->absolute_name }|
          code     = 'FN'. "#EC NOTEXT
    endif.

    e_value = <val>. " Maybe catch move error ?

  endmethod.


  method parse_deep_address.

    data lv_offs type i.
    data lv_tmp type string.
    data lv_len type i.

    lv_len = strlen( i_address ).
    find first occurrence of '[' in i_address match offset lv_offs.
    if sy-subrc <> 0 or lv_len = 0 or substring( val = i_address off = lv_len - 1 ) <> ']'.
      raise exception type zcx_text2tab_error
        exporting
          methname = 'get_struc_field_value_by_name'
          msg      = |Incorrect data address to parse { i_address }|
          code     = 'IA'. "#EC NOTEXT
    endif.

    rs_parsed-location = substring( val = i_address len = lv_offs ).
    lv_tmp = substring( val = i_address off = lv_offs + 1 len = lv_len - lv_offs - 2 ).

    find first occurrence of '=' in lv_tmp match offset lv_offs.
    if sy-subrc <> 0.
      raise exception type zcx_text2tab_error
        exporting
          methname = 'get_struc_field_value_by_name'
          msg      = |Incorrect data address to parse { i_address }|
          code     = 'IA'. "#EC NOTEXT
    endif.

    rs_parsed-key_field = substring( val = lv_tmp len = lv_offs ).
    if strlen( rs_parsed-key_field ) = 0.
      raise exception type zcx_text2tab_error
        exporting
          methname = 'get_struc_field_value_by_name'
          msg      = |Incorrect data address to parse { i_address }|
          code     = 'IA'. "#EC NOTEXT
    endif.

    rs_parsed-key_value = substring( val = lv_tmp off = lv_offs + 1 ).
    if strlen( rs_parsed-key_value ) >= 2 and rs_parsed-key_value+0(1) = '@'.
      rs_parsed-ref_field = substring( val = rs_parsed-key_value off = 1 ).
      clear rs_parsed-key_value.
    endif.

  endmethod.


  method validate_date_format_spec.
    if not i_date_format+3(1) co ' ./-' or not (
      i_date_format+0(3)    = 'DMY'
      or i_date_format+0(3) = 'MDY'
      or i_date_format+0(3) = 'YMD' ).
      raise exception type zcx_text2tab_error
        exporting
          methname = 'CREATE'
          msg      = |Unsupported date format { i_date_format }|
          code     = 'UD'. "#EC NOTEXT
    endif.
  endmethod.
ENDCLASS.
