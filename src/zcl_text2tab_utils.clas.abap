class ZCL_TEXT2TAB_UTILS definition
  public
  final
  create public .

public section.

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
protected section.
private section.
  types:
    tts_checked_names type sorted table of rs38l_fnam with unique key table_line.
  class-data gt_checked_fm_names type tts_checked_names.

ENDCLASS.



CLASS ZCL_TEXT2TAB_UTILS IMPLEMENTATION.


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
