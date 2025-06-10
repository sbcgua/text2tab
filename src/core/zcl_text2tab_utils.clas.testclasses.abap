class ltcl_text2tab_utils_test definition
  for testing
  final
  risk level harmless
  duration short.

  private section.

    constants c_tab   like cl_abap_char_utilities=>horizontal_tab value cl_abap_char_utilities=>horizontal_tab.
    constants c_crlf  like cl_abap_char_utilities=>cr_lf value cl_abap_char_utilities=>cr_lf.
    constants c_lf    like cl_abap_char_utilities=>newline value cl_abap_char_utilities=>newline.

* ==== TESTING ===
    methods validate_date_format_spec for testing.
    methods function_exists for testing.
    methods get_safe_struc_descr for testing raising zcx_text2tab_error.
    methods describe_struct for testing raising zcx_text2tab_error.
    methods describe_struct_with_descr for testing raising zcx_text2tab_error.
    methods describe_struct_ignoring for testing raising zcx_text2tab_error.
    methods describe_struct_deep for testing raising zcx_text2tab_error.
    methods break_to_lines for testing.
    methods build_rename_map for testing.
    methods create_standard_table_of for testing raising zcx_text2tab_error.

    methods parse_deep_address for testing raising zcx_text2tab_error.
    methods get_struc_field_value_by_name for testing raising zcx_text2tab_error.

    methods check_version_fits for testing.
    methods check_version_fits_w_pre for testing.

endclass.

**********************************************************************
* Implementation
**********************************************************************

class ltcl_text2tab_utils_test implementation.

  method build_rename_map.
    data lx type ref to zcx_text2tab_error.

    try.
      clear lx.
      zcl_text2tab_utils=>build_rename_map( 1234 ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'WY' ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( act = lx ).

    try.
      clear lx.
      zcl_text2tab_utils=>build_rename_map( 'abc' ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'WR' ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( act = lx ).

    data lt_fields  type zif_text2tab=>tt_field_name_map.
    data lt_map_act type zif_text2tab=>tt_field_name_map.
    data lt_map_exp type zif_text2tab=>tt_field_name_map.
    field-symbols <map> like line of lt_map_exp.

    append initial line to lt_fields assigning <map>.
    <map>-from = 'some_field'.
    <map>-to   = 'tstring'.
    append initial line to lt_fields assigning <map>.
    <map>-from = 'some_field2'.
    <map>-to   = 'tstring2'.

    append initial line to lt_map_exp assigning <map>.
    <map>-from = 'SOME_FIELD'.
    <map>-to   = 'TSTRING'.
    append initial line to lt_map_exp assigning <map>.
    <map>-from = 'SOME_FIELD2'.
    <map>-to   = 'TSTRING2'.

    " Table based
    try.
      lt_map_act = zcl_text2tab_utils=>build_rename_map( lt_fields ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>fail( lx->get_text( ) ).
    endtry.
    cl_abap_unit_assert=>assert_equals( act = lt_map_act exp = lt_map_exp ).

    " string based
    data lv_fields type string.
    lv_fields = 'some_field:tstring;some_field2:tstring2;;'.

    try.
      lt_map_act = zcl_text2tab_utils=>build_rename_map( lv_fields ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>fail( lx->get_text( ) ).
    endtry.
    cl_abap_unit_assert=>assert_equals( act = lt_map_act exp = lt_map_exp ).

  endmethod.

  method break_to_lines.

    data lt_act type string_table.
    data lt_exp type string_table.

    append 'line1' to lt_exp.
    append 'line2' to lt_exp.

    " no comment
    lt_act = zcl_text2tab_utils=>break_to_lines(
      i_text = 'line1' && c_crlf && 'line2'
      i_begin_comment = space ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).
    lt_act = zcl_text2tab_utils=>break_to_lines(
      i_text = 'line1' && c_lf && 'line2'
      i_begin_comment = space ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    " with comment line
    clear lt_exp.
    append 'not a comment 1' to lt_exp.
    append 'not a comment 2' to lt_exp.
    lt_act = zcl_text2tab_utils=>break_to_lines(
      i_text = |*a comment\nnot a comment 1\nnot a comment 2\n\n|
      i_begin_comment = '*' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    " with auto comment
    clear lt_exp.
    append 'line1' to lt_exp.
    append 'line2' to lt_exp.

    lt_act = zcl_text2tab_utils=>break_to_lines(
      i_text = |line1\nline2|
      i_begin_comment = zif_text2tab=>c_auto_detect_by_space ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    lt_act = zcl_text2tab_utils=>break_to_lines(
      i_text = |The description\nline1\nline2|
      i_begin_comment = zif_text2tab=>c_auto_detect_by_space ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  endmethod.

  method get_safe_struc_descr.
    data:
          ls_dummy  type abap_compdescr,
          lt_dummy  type standard table of abap_compdescr,
          lo_td_exp type ref to cl_abap_structdescr,
          lo_td_act type ref to cl_abap_structdescr,
          lx        type ref to zcx_text2tab_error.

    lo_td_exp ?= cl_abap_typedescr=>describe_by_data( ls_dummy ).

    " Positive
    lo_td_act = zcl_text2tab_utils=>get_safe_struc_descr( ls_dummy ).
    cl_abap_unit_assert=>assert_equals( act = lo_td_act->absolute_name exp = lo_td_exp->absolute_name ).
    lo_td_act = zcl_text2tab_utils=>get_safe_struc_descr( lt_dummy ).
    cl_abap_unit_assert=>assert_equals( act = lo_td_act->absolute_name exp = lo_td_exp->absolute_name ).

    " Positive with class
    lo_td_act = zcl_text2tab_utils=>get_safe_struc_descr( lo_td_exp ).
    cl_abap_unit_assert=>assert_equals( act = lo_td_act->absolute_name exp = lo_td_exp->absolute_name ).
    lo_td_act = zcl_text2tab_utils=>get_safe_struc_descr( cl_abap_tabledescr=>create( lo_td_exp ) ).
    cl_abap_unit_assert=>assert_equals( act = lo_td_act->absolute_name exp = lo_td_exp->absolute_name ).

    " Negative
    try.
      lo_td_act = zcl_text2tab_utils=>get_safe_struc_descr( 'ABC' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>assert_equals( exp = 'PE' act = lx->code ).
    endtry.

    " Negative with class
    try.
      lo_td_act = zcl_text2tab_utils=>get_safe_struc_descr( me ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>assert_equals( exp = 'PE' act = lx->code ).
    endtry.

  endmethod.

  method validate_date_format_spec.
    data:
          lx type ref to zcx_text2tab_error,
          lv_date_format type zif_text2tab=>ty_date_format.

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

  endmethod.

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
    data lt_descr type zif_text2tab=>tt_comp_descr.
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

  method describe_struct_with_descr.

    types:
      begin of lty_dummy,
        mandt type mandt,
        char  type c length 8,
        date  type datum,
      end of lty_dummy.

    data ld_struc type ref to cl_abap_structdescr.
    data ls_dummy type lty_dummy.
    data lt_descr type zif_text2tab=>tt_comp_descr.
    field-symbols <c> like line of lt_descr.

    ld_struc ?= cl_abap_structdescr=>describe_by_data( ls_dummy ).
    lt_descr = zcl_text2tab_utils=>describe_struct(
      i_struc      = ld_struc
      i_with_descr_in_lang = 'E' ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_descr ) exp = 3 ).

    read table lt_descr assigning <c> index 1.
    cl_abap_unit_assert=>assert_equals( act = <c>-description exp = 'Client' ).
    read table lt_descr assigning <c> index 2.
    cl_abap_unit_assert=>assert_equals( act = <c>-description exp = '' ).
    read table lt_descr assigning <c> index 3.
    cl_abap_unit_assert=>assert_equals( act = <c>-description exp = 'Date' ).

  endmethod.

  method describe_struct_ignoring.

    types:
      begin of lty_dummy,
        talpha type veri_alpha,
        tstruc type abap_compdescr, " Deep
      end of lty_dummy.

    data ld_struc type ref to cl_abap_structdescr.
    data ls_dummy type lty_dummy.
    data lt_descr type zif_text2tab=>tt_comp_descr.
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

  method describe_struct_deep.

    types:
      begin of lty_dummy,
        talpha type veri_alpha,
        tstruc type abap_compdescr, " Deep
      end of lty_dummy.

    data ld_struc type ref to cl_abap_structdescr.
    data ls_dummy type lty_dummy.
    data lt_descr type zif_text2tab=>tt_comp_descr.
    field-symbols <c> like line of lt_descr.

    ld_struc ?= cl_abap_structdescr=>describe_by_data( ls_dummy ).
    lt_descr = zcl_text2tab_utils=>describe_struct( i_struc = ld_struc i_is_deep = abap_true ).

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
    cl_abap_unit_assert=>assert_equals( act = <c>-ignore exp = abap_false ).

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
    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.3.0'
        i_required_version = 'v2.2.5' ) ).

    cl_abap_unit_assert=>assert_false(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.1.2'
        i_required_version = 'v2.2.2' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v1.0.0'
        i_required_version = 'v2.2.2' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.5'
        i_required_version = 'v2.3.0' ) ).

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

    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.3'
        i_required_version = 'v2.2.2' ) ).
    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.30'
        i_required_version = 'v2.2.2' ) ).
    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.3.1'
        i_required_version = 'v2.2.2' ) ).
    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v3.0.0'
        i_required_version = 'v2.2.2' ) ).

    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.1.10'
        i_required_version = 'v2.1.7' ) ).

  endmethod.

  method check_version_fits_w_pre.

    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version  = 'v2.1.10-beta'
        i_required_version = 'v2.1.7' ) ).

    cl_abap_unit_assert=>assert_false(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version  = 'v2.1.10-beta'
        i_required_version = 'v2.1.10' ) ).

    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version  = 'v2.1.10-beta'
        i_required_version = 'v2.1.10-alpha' ) ).

    cl_abap_unit_assert=>assert_false(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version  = 'v2.1.10-beta'
        i_required_version = 'v2.1.10-gamma' ) ).

  endmethod.

  method get_struc_field_value_by_name.

    data lx type ref to zcx_text2tab_error.
    data:
      lv_a type string,
      lv_b type d,
      begin of ls_dummy,
        a type string,
        b type d,
      end of ls_dummy.

    ls_dummy-a = 'ABC'.
    ls_dummy-b = '20190820'.

    zcl_text2tab_utils=>get_struc_field_value_by_name(
      exporting
        i_struc = ls_dummy
        i_field_name = 'A'
      importing
        e_value = lv_a ).
    cl_abap_unit_assert=>assert_equals( act = lv_a exp = 'ABC' ).

    zcl_text2tab_utils=>get_struc_field_value_by_name(
      exporting
        i_struc = ls_dummy
        i_field_name = 'B'
      importing
        e_value = lv_b ).
    cl_abap_unit_assert=>assert_equals( act = lv_b exp = '20190820' ).

    try.
      zcl_text2tab_utils=>get_struc_field_value_by_name(
        i_struc = ls_dummy
        i_field_name = 'C' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'FN' ).
    endtry.

  endmethod.

  method parse_deep_address.

    data ls_parsed type zif_text2tab=>ty_deep_address.
    data lx type ref to zcx_text2tab_error.

    ls_parsed = zcl_text2tab_utils=>parse_deep_address( 'filename[id=@headid]' ).
    cl_abap_unit_assert=>assert_equals( act = ls_parsed-location  exp = 'filename' ).
    cl_abap_unit_assert=>assert_equals( act = ls_parsed-key_field exp = 'ID' ).
    cl_abap_unit_assert=>assert_equals( act = ls_parsed-ref_field exp = 'HEADID' ).
    cl_abap_unit_assert=>assert_equals( act = ls_parsed-key_value exp = '' ).

    ls_parsed = zcl_text2tab_utils=>parse_deep_address( 'filename[id=12345]' ).
    cl_abap_unit_assert=>assert_equals( act = ls_parsed-location  exp = 'filename' ).
    cl_abap_unit_assert=>assert_equals( act = ls_parsed-key_field exp = 'ID' ).
    cl_abap_unit_assert=>assert_equals( act = ls_parsed-ref_field exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = ls_parsed-key_value exp = '12345' ).

    try.
      zcl_text2tab_utils=>parse_deep_address( 'XYZ' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_text2tab_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'IA' ).
    endtry.

  endmethod.

  method create_standard_table_of.

    types:
      begin of lty_dummy,
        a type c length 1,
        b type i,
      end of lty_dummy.

    data ls_dummy type lty_dummy.
    data lt_dummy type hashed table of lty_dummy with unique key a.
    data lr_dref type ref to data.
    data lo_type type ref to cl_abap_typedescr.
    data lo_ttype type ref to cl_abap_tabledescr.

    lr_dref = zcl_text2tab_utils=>create_standard_table_of( ls_dummy ).
    lo_type = cl_abap_typedescr=>describe_by_data_ref( lr_dref ).
    cl_abap_unit_assert=>assert_equals( act = lo_type->kind exp = cl_abap_typedescr=>kind_table ).
    lo_ttype ?= lo_type.
    cl_abap_unit_assert=>assert_equals( act = lo_ttype->table_kind exp = cl_abap_tabledescr=>tablekind_std ).

    lr_dref = zcl_text2tab_utils=>create_standard_table_of( lt_dummy ).
    lo_type = cl_abap_typedescr=>describe_by_data_ref( lr_dref ).
    cl_abap_unit_assert=>assert_equals( act = lo_type->kind exp = cl_abap_typedescr=>kind_table ).
    lo_ttype ?= lo_type.
    cl_abap_unit_assert=>assert_equals( act = lo_ttype->table_kind exp = cl_abap_tabledescr=>tablekind_std ).

    lr_dref = zcl_text2tab_utils=>create_standard_table_of( cl_abap_typedescr=>describe_by_data( ls_dummy ) ).
    lo_type = cl_abap_typedescr=>describe_by_data_ref( lr_dref ).
    cl_abap_unit_assert=>assert_equals( act = lo_type->kind exp = cl_abap_typedescr=>kind_table ).
    lo_ttype ?= lo_type.
    cl_abap_unit_assert=>assert_equals( act = lo_ttype->table_kind exp = cl_abap_tabledescr=>tablekind_std ).

  endmethod.

endclass.
