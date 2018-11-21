class lcl_text2tab_utils_test definition
  for testing
  final
  risk level harmless
  duration short.

  private section.

* ==== TESTING ===
    methods validate_date_format_spec.


endclass.

**********************************************************************
* Implementation
**********************************************************************

class lcl_text2tab_utils_test implementation.


  method validate_date_format_spec.
    data:
          lx type ref to zcx_text2tab_error,
          lv_date_format type char4.

    do 3 times.
      case sy-index.
        when 1.
          lv_date_format = 'XXX'.
        when 2.
          lv_date_format = 'DM'.
        when 3.
          lv_date_format = 'DMY='.
        when 4.
          lv_date_format = 'DMM-'.
      endcase.

      clear lx.
      try.
        zcl_text2tab_utils=>validate_date_format_spec( lv_date_format ).
      catch zcx_text2tab_error into lx.
        cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'UD' ). " Unsupported date format
      endtry.
      cl_abap_unit_assert=>assert_not_initial( act = lx ).
    enddo.

  endmethod.      "validate_date_format_spec

endclass.
