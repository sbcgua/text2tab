class ZCL_TEXT2TAB_UTILS definition
  public
  final
  create public .

public section.

  class-methods VALIDATE_DATE_FORMAT_SPEC
    importing
      !I_DATE_FORMAT type CHAR4
    raising
      ZCX_TEXT2TAB_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TEXT2TAB_UTILS IMPLEMENTATION.


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
