class ltcl_test_conversions definition final
  for testing
  risk level harmless
  duration short.

  private section.

    types:
      begin of ty_dummy_ab,
        a type string,
        b type string,
        c type string,
        d type string,
        e type string,
        f type string,
        g type string,
        h type string,
      end of ty_dummy_ab,
      tt_dummy_ab type standard table of ty_dummy_ab with default key,
      begin of ty_dummy_typed,
        tdate    type d,
        tchar    type c length 8,
        tstring  type string,
        talpha   type veri_alpha,
        tdecimal type p length 13 decimals 2, " dmbtr
        tnumber  type n length 4,
        tinteger type i,
        tfloat   type f,
      end of ty_dummy_typed,
      tt_dummy_typed type standard table of ty_dummy_typed with default key,
      begin of ty_dummy_typeless,
        tdate    type string,
        tchar    type string,
        tstring  type string,
        talpha   type string,
        tdecimal type string,
        tnumber  type string,
        tinteger type string,
        tfloat   type string,
      end of ty_dummy_typeless,
      tt_dummy_typeless type standard table of ty_dummy_typeless with default key.

    methods dummies
      exporting
        et_typed type tt_dummy_typed
        et_typeless type tt_dummy_typeless
        et_ab type tt_dummy_ab
        et_grid type zif_text2tab=>ty_grid.

    methods index_to_col for testing raising zcx_text2tab_error.


    methods typed_to_grid for testing raising zcx_text2tab_error.
    methods typeless_to_grid for testing raising zcx_text2tab_error.
    methods typeless_to_ab for testing raising zcx_text2tab_error.

    methods grid_to_ab for testing raising zcx_text2tab_error.
    methods grid_to_typeless for testing raising zcx_text2tab_error.

endclass.

class zcl_text2tab_conv definition local friends ltcl_test_conversions.

class ltcl_test_conversions implementation.

  method dummies.

    field-symbols <typed> like line of et_typed.
    field-symbols <typeless> like line of et_typeless.
    field-symbols <ab> like line of et_ab.
    field-symbols <grid> like line of et_grid.

    if et_typed is supplied.
      append initial line to et_typed assigning <typed>.
      <typed>-tdate    = '20260601'.
      <typed>-tchar    = 'XYZ1'.
      <typed>-tstring  = 'xyz1'.
      <typed>-talpha   = '0000100000'.
      <typed>-tdecimal = '123.54'.
      <typed>-tnumber  = '1234'.
      <typed>-tinteger = 23451.
      <typed>-tfloat   = '123.25'.

      append initial line to et_typed assigning <typed>.
      <typed>-tdate    = '20260602'.
      <typed>-tchar    = 'XYZ2'.
      <typed>-tstring  = 'xyz2'.
      <typed>-talpha   = '0000200000'.
      <typed>-tdecimal = '123.54'.
      <typed>-tnumber  = '2234'.
      <typed>-tinteger = 23452.
      <typed>-tfloat   = '123.75'.
    endif.

    if et_typeless is supplied.
      append initial line to et_typeless assigning <typeless>.
      <typeless>-tdate    = '20260601'.
      <typeless>-tchar    = 'XYZ1'.
      <typeless>-tstring  = 'xyz1'.
      <typeless>-talpha   = '0000100000'.
      <typeless>-tdecimal = '123.54'.
      <typeless>-tnumber  = '1234'.
      <typeless>-tinteger = '23451'.
      <typeless>-tfloat   = '123.25'.

      append initial line to et_typeless assigning <typeless>.
      <typeless>-tdate    = '20260602'.
      <typeless>-tchar    = 'XYZ2'.
      <typeless>-tstring  = 'xyz2'.
      <typeless>-talpha   = '0000200000'.
      <typeless>-tdecimal = '123.54'.
      <typeless>-tnumber  = '2234'.
      <typeless>-tinteger = '23452'.
      <typeless>-tfloat   = '123.75'.
    endif.

    if et_ab is supplied.
      append initial line to et_ab assigning <ab>.
      <ab>-a = 'TDATE'.
      <ab>-b = 'TCHAR'.
      <ab>-c = 'TSTRING'.
      <ab>-d = 'TALPHA'.
      <ab>-e = 'TDECIMAL'.
      <ab>-f = 'TNUMBER'.
      <ab>-g = 'TINTEGER'.
      <ab>-h = 'TFLOAT'.

      append initial line to et_ab assigning <ab>.
      <ab>-a = '20260601'.
      <ab>-b = 'XYZ1'.
      <ab>-c = 'xyz1'.
      <ab>-d = '0000100000'.
      <ab>-e = '123.54'.
      <ab>-f = '1234'.
      <ab>-g = '23451'.
      <ab>-h = '123.25'.

      append initial line to et_ab assigning <ab>.
      <ab>-a = '20260602'.
      <ab>-b = 'XYZ2'.
      <ab>-c = 'xyz2'.
      <ab>-d = '0000200000'.
      <ab>-e = '123.54'.
      <ab>-f = '2234'.
      <ab>-g = '23452'.
      <ab>-h = '123.75'.
    endif.

    if et_grid is supplied.
      append initial line to et_grid assigning <grid>.
      append 'TDATE'    to <grid>.
      append 'TCHAR'    to <grid>.
      append 'TSTRING'  to <grid>.
      append 'TALPHA'   to <grid>.
      append 'TDECIMAL' to <grid>.
      append 'TNUMBER'  to <grid>.
      append 'TINTEGER' to <grid>.
      append 'TFLOAT'   to <grid>.

      append initial line to et_grid assigning <grid>.
      append '20260601'   to <grid>.
      append 'XYZ1'       to <grid>.
      append 'xyz1'       to <grid>.
      append '0000100000' to <grid>.
      append '123.54'     to <grid>.
      append '1234'       to <grid>.
      append '23451'      to <grid>.
      append '123.25'     to <grid>.

      append initial line to et_grid assigning <grid>.
      append '20260602'   to <grid>.
      append 'XYZ2'       to <grid>.
      append 'xyz2'       to <grid>.
      append '0000200000' to <grid>.
      append '123.54'     to <grid>.
      append '2234'       to <grid>.
      append '23452'      to <grid>.
      append '123.75'     to <grid>.
    endif.


  endmethod.

  method index_to_col.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_text2tab_conv=>index_to_col( 1 )
      exp = 'A' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_text2tab_conv=>index_to_col( 26 )
      exp = 'Z' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_text2tab_conv=>index_to_col( 27 )
      exp = 'AA' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_text2tab_conv=>index_to_col( 52 )
      exp = 'AZ' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_text2tab_conv=>index_to_col( 53 )
      exp = 'BA' ).

  endmethod.

  method typed_to_grid.

    data lt_src type tt_dummy_typed.
    data lt_act type zif_text2tab=>ty_grid.
    data lt_exp type zif_text2tab=>ty_grid.

    dummies(
      importing
        et_typed = lt_src
        et_grid  = lt_exp ).

    lt_act = zcl_text2tab_conv=>typed_to_grid( lt_src ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    delete lt_exp index 1.
    lt_act = zcl_text2tab_conv=>typed_to_grid(
      i_typed_tab = lt_src
      i_with_header = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  endmethod.

  method typeless_to_grid.

    data lt_src type tt_dummy_typeless.
    data lt_act type zif_text2tab=>ty_grid.
    data lt_exp type zif_text2tab=>ty_grid.

    dummies(
      importing
        et_typeless = lt_src
        et_grid     = lt_exp ).

    lt_act = zcl_text2tab_conv=>typeless_to_grid( lt_src ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    delete lt_exp index 1.
    lt_act = zcl_text2tab_conv=>typed_to_grid(
      i_typed_tab = lt_src
      i_with_header = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  endmethod.

  method typeless_to_ab.

    data lt_src type tt_dummy_typeless.
    data lr_act type ref to data.
    data lt_exp type tt_dummy_ab.

    field-symbols <act> type standard table.

    dummies(
      importing
        et_typeless = lt_src
        et_ab       = lt_exp ).

    lr_act = zcl_text2tab_conv=>typeless_to_ab( lt_src ).

    assign lr_act->* to <act>.
    cl_abap_unit_assert=>assert_equals(
      act = <act>
      exp = lt_exp ).

  endmethod.

  method grid_to_ab.

    data lt_src type zif_text2tab=>ty_grid.
    data lr_act type ref to data.
    data lt_exp type tt_dummy_ab.

    field-symbols <act> type standard table.

    dummies(
      importing
        et_grid = lt_src
        et_ab   = lt_exp ).

    lr_act = zcl_text2tab_conv=>grid_to_ab( lt_src ).

    assign lr_act->* to <act>.
    cl_abap_unit_assert=>assert_equals(
      act = <act>
      exp = lt_exp ).

  endmethod.

  method grid_to_typeless.

    data lt_src type zif_text2tab=>ty_grid.
    data lr_act type ref to data.
    data lt_exp type tt_dummy_typeless.
    field-symbols <act> type standard table.

    dummies(
      importing
        et_grid = lt_src
        et_typeless = lt_exp ).

    lr_act = zcl_text2tab_conv=>grid_to_typeless( lt_src ).

    assign lr_act->* to <act>.
    cl_abap_unit_assert=>assert_equals(
      act = <act>
      exp = lt_exp ).

  endmethod.

endclass.
