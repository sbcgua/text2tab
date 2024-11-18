class ltcl_text2tab_error_test definition final
  for testing
  risk level harmless
  duration short.

  private section.
    methods smoke_test for testing.
endclass.

class ltcl_text2tab_error_test implementation.

  method smoke_test.
    data lx type ref to zcx_text2tab_error.

    try.
      clear lx.
      zcx_text2tab_error=>raise(
        msg  = 'Hello'
        code = 'C1'
        methname = 'my_method'
        location = '@line1' ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->msg      exp = 'Hello' ).
      cl_abap_unit_assert=>assert_equals( act = lx->code     exp = 'C1' ).
      cl_abap_unit_assert=>assert_equals( act = lx->methname exp = 'my_method' ).
      cl_abap_unit_assert=>assert_equals( act = lx->location exp = '@line1' ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( lx ).

    try.
      clear lx.
      raise exception type zcx_text2tab_error
        exporting
        msg  = 'Hello'
        code = 'C1'
        methname = 'my_method'
        location = '@line1'.
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->msg      exp = 'Hello' ).
      cl_abap_unit_assert=>assert_equals( act = lx->code     exp = 'C1' ).
      cl_abap_unit_assert=>assert_equals( act = lx->methname exp = 'my_method' ).
      cl_abap_unit_assert=>assert_equals( act = lx->location exp = '@line1' ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( lx ).


  endmethod.

endclass.
