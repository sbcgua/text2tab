class lcl_text2tab_utils_test definition
  for testing
  final
  risk level harmless
  duration short.

  private section.

* ==== TESTING ===
    methods validate_date_format_spec for testing.
    methods function_exists for testing.
    methods describe_struct for testing raising zcx_text2tab_error.
    methods describe_struct_ignoring for testing raising zcx_text2tab_error.
    methods check_version_fits for testing.

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

      try.
        zcl_text2tab_utils=>validate_date_format_spec( lv_date_format ).
        cl_abap_unit_assert=>fail( ).
      catch zcx_text2tab_error into lx.
        cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'UD' ). " Unsupported date format
      endtry.
    enddo.

  endmethod.      "validate_date_format_spec

  method function_exists.

    data lv_act type abap_bool.

    lv_act = zcl_text2tab_utils=>function_exists( 'CONVERSION_EXIT_?????_OUTPUT' ).
    cl_abap_unit_assert=>assert_equals( act = lv_act exp = abap_false ).

    lv_act = zcl_text2tab_utils=>function_exists( 'CONVERSION_EXIT_ALPHA_OUTPUT' ).
    cl_abap_unit_assert=>assert_equals( act = lv_act exp = abap_true ).

    " cached
    lv_act = zcl_text2tab_utils=>function_exists( 'CONVERSION_EXIT_ALPHA_OUTPUT' ).
    cl_abap_unit_assert=>assert_equals( act = lv_act exp = abap_true ).

  endmethod.

  method describe_struct.

    types:
      begin of lty_dummy,
        talpha type veri_alpha,
      end of lty_dummy.

    data ld_struc type ref to cl_abap_structdescr.
    data ls_dummy type lty_dummy.
    data lt_descr type zcl_text2tab_utils=>tt_comp_descr.
    data lx type ref to zcx_text2tab_error.
    field-symbols <c> like line of lt_descr.

    ld_struc ?= cl_abap_structdescr=>describe_by_data( ls_dummy ).
    lt_descr = zcl_text2tab_utils=>describe_struct( i_struc = ld_struc i_ignore_nonflat = abap_false ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_descr ) exp = 1 ).
    read table lt_descr assigning <c> index 1.
    cl_abap_unit_assert=>assert_equals( act = <c>-name exp = 'TALPHA' ).
    cl_abap_unit_assert=>assert_equals( act = <c>-edit_mask exp = 'ALPHA' ).
    cl_abap_unit_assert=>assert_equals( act = <c>-output_length exp = 10 ).

    " Test 2: fail on non-flat components
    types:
      begin of lty_dummy2,
        tstruc type lty_dummy,
      end of lty_dummy2.
    data ls_dummy2 type lty_dummy2.

    try.
      ld_struc ?= cl_abap_structdescr=>describe_by_data( ls_dummy2 ).
      lt_descr = zcl_text2tab_utils=>describe_struct( i_struc = ld_struc i_ignore_nonflat = abap_false ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'SF' ).
    endtry.

  endmethod.

  method describe_struct_ignoring.

    types:
      begin of lty_dummy,
        talpha type veri_alpha,
        tstruc type abap_compdescr, " Deep
      end of lty_dummy.

    data ld_struc type ref to cl_abap_structdescr.
    data ls_dummy type lty_dummy.
    data lt_descr type zcl_text2tab_utils=>tt_comp_descr.
    field-symbols <c> like line of lt_descr.

    ld_struc ?= cl_abap_structdescr=>describe_by_data( ls_dummy ).
    lt_descr = zcl_text2tab_utils=>describe_struct( i_struc = ld_struc i_ignore_nonflat = abap_true ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_descr ) exp = 2 ).

    read table lt_descr assigning <c> index 1.
    cl_abap_unit_assert=>assert_equals( act = <c>-name exp = 'TALPHA' ).
    cl_abap_unit_assert=>assert_equals( act = <c>-edit_mask exp = 'ALPHA' ).
    cl_abap_unit_assert=>assert_equals( act = <c>-output_length exp = 10 ).
    cl_abap_unit_assert=>assert_equals( act = <c>-ignore exp = abap_false ).

    read table lt_descr assigning <c> index 2.
    cl_abap_unit_assert=>assert_equals( act = <c>-name exp = 'TSTRUC' ).
    cl_abap_unit_assert=>assert_equals( act = <c>-edit_mask exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = <c>-output_length exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = <c>-ignore exp = abap_true ).

  endmethod.

  method check_version_fits.
    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.2.2' ) ).
    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.1.2' ) ).
    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v1.0.0' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.2.3' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.2.30' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.3.1' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v3.0.0' ) ).
  endmethod.

endclass.
