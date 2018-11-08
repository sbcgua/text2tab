**********************************************************************
* MACRO
**********************************************************************

define append_dummy.
  e_dummy_struc-tdate    = &1.
  e_dummy_struc-tchar    = &2.
  e_dummy_struc-tstring  = &3.
  e_dummy_struc-tdecimal = &4.
  e_dummy_struc-tnumber  = &5.
  e_dummy_struc-traw     = &6.
  e_dummy_struc-tinteger = &7.
  e_dummy_struc-talpha   = &8.
  e_dummy_struc-tfloat   = &9.
  append e_dummy_struc to e_dummy_tab.
end-of-definition.

define test_field.
  ls_dummy-&1 = &2.
  read table ld_type->components with key name = '&1' assigning <comp>.
  l_act = o->serialize_field( i_value = ls_dummy-&1 is_component = <comp> ).
  cl_abap_unit_assert=>assert_equals( act = l_act exp = &3 ).
end-of-definition.

**********************************************************************
* Test Class definition
**********************************************************************

class lcl_text2tab_serializer_test definition final
  for testing
  risk level harmless
  duration short.

  public section.

    types:
      begin of ty_dummy,
        mandt    type mandt,
        tdate    type datum,
        tchar    type char08,
        traw     type thraw1,
        tstring  type string,
        talpha   type veri_alpha,
        tdecimal type dmbtr,
        tnumber  type numc4,
        tinteger type i,
        tfloat   type float,
      end of ty_dummy,
      tt_dummy type standard table of ty_dummy with default key.

* ================
  private section.
    constants c_tab   like cl_abap_char_utilities=>horizontal_tab value cl_abap_char_utilities=>horizontal_tab.
    constants c_crlf  like cl_abap_char_utilities=>cr_lf value cl_abap_char_utilities=>cr_lf.
    constants c_lf    like cl_abap_char_utilities=>newline value cl_abap_char_utilities=>newline.

    data o type ref to zcl_text2tab_serializer.  "class under test

* ==== TESTING ===

    methods smoke_test for testing.
    methods serialize_date for testing.
    methods serialize_field for testing.
    methods negatives for testing.
    methods create for testing.

* ==== HELPERS ===

    methods setup.
    methods get_dummy_data
      exporting
        e_dummy_struc     type ty_dummy
        e_dummy_tab       type tt_dummy
        e_dummy_struc_str type string
        e_dummy_string    type string.

endclass.

class zcl_text2tab_serializer definition local friends lcl_text2tab_serializer_test.

**********************************************************************
* Implementation
**********************************************************************

class lcl_text2tab_serializer_test implementation.

  method setup.
    data lx type ref to zcx_text2tab_error.
    try.
      o = zcl_text2tab_serializer=>create( ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>fail( lx->get_text( ) ).
    endtry.
  endmethod.      "setup

  method smoke_test.
    data:
          lv_act       type string,
          lv_exp_struc type string,
          lv_exp_tab   type string,
          lt_tab       type tt_dummy,
          ls_struc     type ty_dummy,
          lx type ref to zcx_text2tab_error.

    get_dummy_data( importing
      e_dummy_struc     = ls_struc
      e_dummy_tab       = lt_tab
      e_dummy_string    = lv_exp_tab
      e_dummy_struc_str = lv_exp_struc ).

    try.
      lv_act = o->serialize( lt_tab ).
      cl_abap_unit_assert=>assert_equals( act = lv_act exp = lv_exp_tab ).

      lv_act = o->serialize( ls_struc ).
      cl_abap_unit_assert=>assert_equals( act = lv_act exp = lv_exp_struc ).

    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>fail( lx->get_text( ) ).
    endtry.
  endmethod.

  method get_dummy_data.
    data:
          l_offs    type i,
          l_string  type string.

    l_string = 'MANDT\tTDATE\tTCHAR\tTRAW\tTSTRING\tTALPHA\tTDECIMAL\tTNUMBER\tTINTEGER\tTFLOAT\n'
            && '\t01.01.2015\tTrololo1\t8A\tString1\t100000\t1234567.81\t2015\t1111\t1.12345\n'
            && '\t02.01.2016\tTrololo2\t8B\tString2\t200000\t1234567.82\t2016\t2222\t1.1\n'
            && '\t03.01.2016\tTrololo3\t8C\tString3\t300000\t1234567.83\t2015\t3333\t1000' .

    replace all occurrences of '\t' in l_string with c_tab.
    replace all occurrences of '\n' in l_string with c_crlf.

    clear e_dummy_tab.

    "             TDATE      TCHAR      TSTRING   TDECIMAL    TNUM TRAW  TINT  TALPHA      TFLOAT
    append_dummy '20150101' 'Trololo1' 'String1' '1234567.81' 2015 '8A'  1111 '0000100000' '1.12345'.
    append_dummy '20160102' 'Trololo2' 'String2' '1234567.82' 2016 '8B'  2222 '0000200000' '1.10'.
    append_dummy '20160103' 'Trololo3' 'String3' '1234567.83' 2015 '8C'  3333 '0000300000' '1000.00'.

    read table e_dummy_tab into e_dummy_struc index 1.
    e_dummy_string = l_string.

    l_offs = find( val = l_string sub = c_crlf ).
    l_offs = find( val = l_string sub = c_crlf off = l_offs + 1 ). " second crlf
    e_dummy_struc_str = l_string+0(l_offs).

  endmethod.       " get_dummy_data

  method serialize_date.
    data l_act type string.

    l_act = zcl_text2tab_serializer=>serialize_date( i_date = '20180901' iv_date_format = 'DMY' ).
    cl_abap_unit_assert=>assert_equals( act = l_act exp = '01092018' ).
    l_act = zcl_text2tab_serializer=>serialize_date( i_date = '20180901' iv_date_format = 'DMY.' ).
    cl_abap_unit_assert=>assert_equals( act = l_act exp = '01.09.2018' ).
    l_act = zcl_text2tab_serializer=>serialize_date( i_date = '20180901' iv_date_format = 'YMD-' ).
    cl_abap_unit_assert=>assert_equals( act = l_act exp = '2018-09-01' ).
    l_act = zcl_text2tab_serializer=>serialize_date( i_date = '00000000' iv_date_format = 'YMD-' ).
    cl_abap_unit_assert=>assert_equals( act = l_act exp = '' ).

  endmethod.

  method serialize_field.
    data:
          lx        type ref to zcx_text2tab_error,
          l_act     type string,
          ls_dummy  type ty_dummy,
          ld_type   type ref to cl_abap_structdescr.

    data lv_meins type meins.
    data ls_comp like line of ld_type->components.

    field-symbols: <comp> like line of ld_type->components.

    ld_type ?= cl_abap_typedescr=>describe_by_data( ls_dummy ).

    try.
      test_field TFLOAT '1.123456' '1.12346'.
      test_field TFLOAT '1.00'     '1'.
      test_field TFLOAT '1.10'     '1.1'.
      test_field TFLOAT '1231.10'  '1231.1'.

      o->mv_decimal_sep = ','.
      test_field TFLOAT '1.10'     '1,1'.
      o->mv_max_frac_digits = 3.
      test_field TFLOAT '1.123456' '1,123'.

      test_field TDECIMAL  '1.12'       '1,12'.
      test_field TDECIMAL  '1111.12'    '1111,12'.
      test_field TALPHA    '0000100000' '100000'.
      test_field TINTEGER  3333         '3333'.

      test_field TDATE  '20180901' '01.09.2018'.
      test_field TDATE  '00000000' ''.
      test_field TDATE  '' ''.

      lv_meins          = 'KG'.
      ls_comp-type_kind = cl_abap_typedescr=>typekind_char.
      l_act = o->serialize_field( i_value = lv_meins is_component = ls_comp ).
      cl_abap_unit_assert=>assert_equals( act = l_act exp = 'KG' ).

    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>fail( lx->get_text( ) ).
    endtry.

    " Negative tests
    try.
      clear lx.
      lv_meins = '??'.
      ls_comp-type_kind = cl_abap_typedescr=>typekind_char.
      l_act = o->serialize_field( i_value = lv_meins is_component = ls_comp ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'CF' ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( lx ).

  endmethod.

  method negatives.
    data:
          lx        type ref to zcx_text2tab_error,
          l_act     type string.
    data:
          begin of ls_deep,
            str type string,
            tab type tt_dummy,
          end of ls_deep.

    try.
      clear lx.
      o->serialize( i_data = l_act ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'ST' ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( lx ).

    try.
      clear lx.
      o->serialize( i_data = ls_deep ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'ET' ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( lx ).

  endmethod.

  method create.
    data:
          lx        type ref to zcx_text2tab_error.

    try.
      clear lx.
      o = zcl_text2tab_serializer=>create(
        i_date_format = 'YYM-' ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'UD' ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( lx ).

  endmethod.

endclass.
