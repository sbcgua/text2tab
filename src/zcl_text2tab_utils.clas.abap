class ZCL_TEXT2TAB_UTILS definition
  public
  final
  create public .

  public section.

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

  protected section.
  private section.
    types:
      tts_checked_names type sorted table of rs38l_fnam with unique key table_line.
    class-data gt_checked_fm_names type tts_checked_names.

ENDCLASS.



CLASS ZCL_TEXT2TAB_UTILS IMPLEMENTATION.


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
