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
    end of ty_comp_descr .
  types:
    tt_comp_descr type standard table of ty_comp_descr with default key .

  type-pools ABAP .
  class-methods FUNCTION_EXISTS
    importing
      !I_FUNCTION_NAME type RS38L_FNAM
    returning
      value(R_YES) type ABAP_BOOL .
  class-methods VALIDATE_DATE_FORMAT_SPEC
    importing
      !I_DATE_FORMAT type CHAR4
    raising
      ZCX_TEXT2TAB_ERROR .
  class-methods DESCRIBE_STRUCT
    importing
      !ID_STRUC type ref to CL_ABAP_STRUCTDESCR
    returning
      value(RT_DESCR) type TT_COMP_DESCR
    raising
      ZCX_TEXT2TAB_ERROR .
  class-methods CHECK_VERSION_FITS
    importing
      !I_REQUIRED_VERSION type STRING
      !I_CURRENT_VERSION type STRING
    returning
      value(R_FITS) type ABAP_BOOL .

protected section.
private section.
  types:
    tts_checked_names type sorted table of rs38l_fnam with unique key table_line.
  class-data gt_checked_fm_names type tts_checked_names.

ENDCLASS.



CLASS ZCL_TEXT2TAB_UTILS IMPLEMENTATION.


method CHECK_VERSION_FITS.

  types:
    begin of ty_version,
      major type numc4,
      minor type numc4,
      patch type numc4,
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

  field-symbols <c> like line of id_struc->components.
  field-symbols <descr> like line of rt_descr.
  data lo_ed type ref to cl_abap_elemdescr.

  try.
    loop at id_struc->components assigning <c>.
      append initial line to rt_descr assigning <descr>.
      move-corresponding <c> to <descr>.
      lo_ed ?= id_struc->get_component_type( <c>-name ).
      <descr>-output_length = lo_ed->output_length.
      <descr>-edit_mask     = lo_ed->edit_mask.
      shift <descr>-edit_mask left deleting leading '='.
    endloop.
  catch cx_sy_move_cast_error.
    zcx_text2tab_error=>raise(
      msg = 'Structure must be flat' "#EC NOTEXT
      code = 'SF' ).
  endtry.

endmethod.


method FUNCTION_EXISTS.

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


method VALIDATE_DATE_FORMAT_SPEC.
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
