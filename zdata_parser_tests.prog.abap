*&---------------------------------------------------------------------*
*&  Include           ZTEXT_TABDATA_PARSER_TESTS
*&---------------------------------------------------------------------*

**********************************************************************
* MACRO
**********************************************************************
define test_parse.
  clear ls_dummy.
  read table lo_struc_descr->components into ls_component with key name = '&1'.
  o->parse_field(
    exporting
      is_component = ls_component
      i_value      = &2
    importing
      e_field      = ls_dummy-&1 ).
end-of-definition.

define test_parse_positive.
  clear lx.
  try.
    test_parse &1 &2.
  catch lcx_data_parser_error into lx.
    fail( lx->get_text( ) ).
  endtry.
  assert_equals( act = ls_dummy-&1 exp = &3 msg = 'Parse field positive:' && &2 ).
end-of-definition.

define test_parse_negative.
  clear lx.
  try.
    test_parse &1 &2.
  catch lcx_data_parser_error into lx.
    assert_equals( exp = &3 act = lx->code ).
  endtry.
  assert_not_initial( act = lx msg = 'Parse field negative:' && &2 ).
end-of-definition.

define append_dummy.
  e_dummy_struc-tdate    = &1.
  e_dummy_struc-tchar    = &2.
  e_dummy_struc-tstring  = &3.
  e_dummy_struc-tdecimal = &4.
  e_dummy_struc-tnumber  = &5.
  if i_strict = abap_true.
    e_dummy_struc-traw     = &6.
    e_dummy_struc-tinteger = &7.
    e_dummy_struc-talpha   = &8.
    e_dummy_struc-tfloat   = &9.
  endif.
  append e_dummy_struc to e_dummy_tab.
end-of-definition.

*  define assert_excode.
*    assert_not_initial( act = lx ).
*    assert_equals( exp = &1 act = lx->code ).
*  end-of-definition.

**********************************************************************
* Test Class definition
**********************************************************************


class lcl_test_data_parser definition for testing
  inheriting from cl_aunit_assert final
  risk level harmless duration short.

* ================
  public section.

    types:
      begin of ty_dummy,
        mandt    type mandt,
        tdate    type datum,
        tchar    type veri_c08,
        traw     type veri_x1,
        tstring  type string,
        talpha   type veri_alpha,
        tdecimal type veri_cur13,
        tnumber  type veri_n04,
        tinteger type i,
        tfloat   type float,
      end of ty_dummy.

    types: tt_dummy type standard table of ty_dummy with default key.

* ================
  private section.
    constants c_tab  like cl_abap_char_utilities=>horizontal_tab value cl_abap_char_utilities=>horizontal_tab.
    constants c_crlf like cl_abap_char_utilities=>cr_lf value cl_abap_char_utilities=>cr_lf.

    data o type ref to lcl_data_parser.  "class under test

* ==== TESTING ===

    methods create                for testing.
    methods apply_conv_exit       for testing.
    methods parse_field           for testing.
    methods parse_field_unsupp    for testing.
    methods map_head_structure    for testing.
    methods get_safe_struc_descr  for testing.

    methods parse_line_negative   for testing.
    methods parse_data_empty_line for testing.
    methods parse_negative        for testing.
    methods parse                 for testing.

* ==== HELPERS ===

    methods setup.
    methods get_dummy_data importing i_strict       type abap_bool default abap_true
                           exporting e_dummy_struc  type ty_dummy
                                     e_dummy_tab    type tt_dummy
                                     e_dummy_header type string
                                     e_dummy_string type string
                                     e_map          type int4_table.


endclass.       "lcl_test_data_parser

**********************************************************************
* Implementation
**********************************************************************

class lcl_test_data_parser implementation.

  method setup.
    data:
          lx      type ref to lcx_data_parser_error,
          l_dummy type ty_dummy.

    try.
      o = lcl_data_parser=>create( l_dummy ).
    catch lcx_data_parser_error into lx.
      fail( lx->get_text( ) ).
    endtry.
  endmethod.      "setup

  method create.
    data:
          lo type ref to lcl_data_parser,
          lx type ref to lcx_data_parser_error,
          lv_date_format type char4,
          ls_dummy       type ty_dummy,
          lt_dummy       type tt_dummy,
          lv_dummy       type i.

    try.
      lo = lcl_data_parser=>create( i_pattern = ls_dummy ).
      assert_not_initial( act = lo ).
      assert_equals( act = lo->mv_amount_format exp = ' ,' ).

      lo = lcl_data_parser=>create( i_pattern = lt_dummy i_amount_format = ' .' ).
      assert_not_initial( act = lo ).
      assert_equals( act = lo->mv_amount_format exp = ' .' ).

      lo = lcl_data_parser=>create( i_pattern = ls_dummy i_amount_format = 'x' ).
      assert_not_initial( act = lo ).
      assert_equals( act = lo->mv_amount_format exp = ' ,' ).

      lo = lcl_data_parser=>create( i_pattern = ls_dummy ).
      assert_not_initial( act = lo ).
      assert_equals( act = lo->mv_date_format exp = 'DMY.' ).

      lo = lcl_data_parser=>create( i_pattern = ls_dummy i_date_format = 'YMD' ).
      assert_not_initial( act = lo ).
      assert_equals( act = lo->mv_date_format exp = 'YMD' ).

    catch lcx_data_parser_error into lx.
      fail( lx->get_text( ) ).
    endtry.

    clear lx.
    try.
      lo = lcl_data_parser=>create( i_pattern = lv_dummy ).
    catch lcx_data_parser_error into lx.
      assert_equals( act = lx->code exp = 'PE' ). " Pattern error
    endtry.
    assert_not_initial( act = lx ).

    do 3 times.
      case sy-index.
        when 1.
          lv_date_format = 'XXX'.
        when 2.
          lv_date_format = 'DM'.
        when 3.
          lv_date_format = 'DMY='.
      endcase.

      clear lx.
      try.
        lo = lcl_data_parser=>create( i_pattern = ls_dummy i_date_format = lv_date_format ).
      catch lcx_data_parser_error into lx.
        assert_equals( act = lx->code exp = 'UD' ). " Unsupported date format
      endtry.
      assert_not_initial( act = lx ).
    enddo.

  endmethod.      "create

  method get_safe_struc_descr.
    data:
          ls_dummy  type ty_dummy,
          lt_dummy  type tt_dummy,
          lo_td_exp type ref to cl_abap_structdescr,
          lo_td_act type ref to cl_abap_structdescr,
          lx        type ref to lcx_data_parser_error.

    lo_td_exp ?= cl_abap_typedescr=>describe_by_data( ls_dummy ).

    try. " Positive
      lo_td_act = lcl_data_parser=>get_safe_struc_descr( ls_dummy ).
      assert_equals( act = lo_td_act->absolute_name exp = lo_td_exp->absolute_name ).
      lo_td_act = lcl_data_parser=>get_safe_struc_descr( lt_dummy ).
      assert_equals( act = lo_td_act->absolute_name exp = lo_td_exp->absolute_name ).
    catch lcx_data_parser_error into lx.
      fail( lx->get_text( ) ).
    endtry.

    try. " Negative
      lo_td_act = lcl_data_parser=>get_safe_struc_descr( 'ABC' ).
    catch lcx_data_parser_error into lx.
      assert_equals( exp = 'PE' act = lx->code ).
    endtry.
    assert_not_initial( act = lx ).

  endmethod.  "get_safe_struc_descr

  method parse.
    data:
          dummy_act      type ty_dummy,
          dummy_tab_act  type tt_dummy,
          dummy_htab     type hashed table of ty_dummy with unique key tdate,
          dummy_stab     type sorted table of ty_dummy with unique key tdate,
          dummy_exp      type ty_dummy,
          dummy_tab_exp  type tt_dummy,
          dummy_head     type string,
          l_string       type string,
          lt_strings     type table of string,
          lt_header_act  type standard table of string,
          lt_header_exp  type standard table of string,
          lx             type ref to lcx_data_parser_error.

    " Strict parsing *********************************
    get_dummy_data( importing e_dummy_struc  = dummy_exp
                              e_dummy_tab    = dummy_tab_exp
                              e_dummy_header = dummy_head
                              e_dummy_string = l_string ).
    split dummy_head at c_tab into table lt_header_exp.

    try.
      o->parse( exporting i_data      = l_string
                importing e_container = dummy_act ).
      assert_equals( act = dummy_act     exp = dummy_exp ).

      o->parse( exporting i_data        = l_string
                importing e_container   = dummy_tab_act
                          e_head_fields = lt_header_act ).
      assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).
      assert_equals( act = lt_header_act exp = lt_header_exp ).
    catch lcx_data_parser_error into lx.
      fail( lx->get_text( ) ).
    endtry.

    " Parse to sorted and hashed tables ***************
    try.
      o->parse( exporting i_data      = l_string
                importing e_container = dummy_stab ).
      assert_equals( act = dummy_stab exp = dummy_tab_exp ).

      o->parse( exporting i_data      = l_string
                importing e_container = dummy_htab ).
      assert_equals( act = dummy_htab exp = dummy_tab_exp ).
    catch lcx_data_parser_error into lx.
      fail( lx->get_text( ) ).
    endtry.

    " Parse without head
    split l_string at c_crlf into table lt_strings.
    delete lt_strings index 1.
    concatenate lines of lt_strings into l_string separated by c_crlf.

    try.
      o->parse( exporting i_data      = l_string
                          i_has_head  = abap_false
                importing e_container = dummy_tab_act ).
      assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).
    catch lcx_data_parser_error into lx.
      fail( lx->get_text( ) ).
    endtry.

    " NOT STRICT parsing ******************************
    get_dummy_data( exporting i_strict       = abap_false
                    importing e_dummy_tab    = dummy_tab_exp
                              e_dummy_header = dummy_head
                              e_dummy_string = l_string ).
    split dummy_head at c_tab into table lt_header_exp.

    try.
      o->parse( exporting i_data        = l_string
                          i_strict      = abap_false
                importing e_container   = dummy_tab_act
                          e_head_fields = lt_header_act ).
      assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).
      assert_equals( act = lt_header_act exp = lt_header_exp ).
    catch lcx_data_parser_error into lx.
      fail( lx->get_text( ) ).
    endtry.

  endmethod.  "parse


  method parse_negative.

    data: begin of wrong_struc ##NEEDED,
            mandt    type mandt,
            tdate    type datum,
            tchar    type veri_c08,
          end of   wrong_struc.

    data:
          l_exp_code     type char2,
          dummy_val      type char40 ##NEEDED,
          dummy_tab_act  type tt_dummy ##NEEDED,
          l_string       type string,
          l_string_bak   type string,
          lx             type ref to lcx_data_parser_error.

    get_dummy_data( importing e_dummy_string = l_string_bak ).

    do 5 times.
      clear lx.
      l_string = l_string_bak.

      try.
        case sy-index.
          when 1. " Parse to field (not table or structure)
            l_exp_code = 'PE'.
            o->parse( exporting i_data      = l_string
                      importing e_container = dummy_val ).
          when 2. " Parse empty file
            clear l_string.
            l_exp_code = 'DE'.
            o->parse( exporting i_data      = l_string
                      importing e_container = dummy_tab_act ).
          when 3. " Add empty line at the beginning
            l_string = c_crlf && l_string.
            l_exp_code = 'HE'.
            o->parse( exporting i_data      = l_string
                      importing e_container = dummy_tab_act ).
          when 4. " Wrong params
            l_exp_code = 'WP'.
            o->parse( exporting i_data      = l_string
                                i_strict    = abap_false
                                i_has_head  = abap_false
                      importing e_container = dummy_tab_act ).
          when 5. " Wrong container type
            l_exp_code = 'TE'.
            o->parse( exporting i_data      = l_string
                      importing e_container = wrong_struc ).
        endcase.

      catch lcx_data_parser_error into lx.
        assert_equals( exp = l_exp_code act = lx->code msg = |parse, case { sy-index }| ).
      endtry.
      assert_not_initial( act = lx msg = |parse, case { sy-index }| ).

    enddo.

  endmethod.  "parse_negative


  method apply_conv_exit.
    data:
          l_dummy  type ty_dummy,
          lx       type ref to lcx_data_parser_error.

    try .
      o->apply_conv_exit( exporting i_convexit = 'ALPHA'
                                    i_value    = '123'
                          importing e_field    = l_dummy-talpha ).
    catch lcx_data_parser_error into lx.
      fail( lx->get_text( ) ).
    endtry.

    assert_equals( act = l_dummy-talpha exp = '0000000123' ).

    " Check wrong exit
    clear lx.
    try .
      o->apply_conv_exit( exporting i_convexit = 'NONAME'
                                    i_value    = '123'
                          importing e_field    = l_dummy-talpha ).
    catch lcx_data_parser_error into lx.
      assert_equals( act = lx->code exp = 'EM' ).
    endtry.
    assert_not_initial( act = lx ).

  endmethod. "apply_conv_exit

  method parse_field.
    data:
          ls_dummy       type ty_dummy,
          lo_struc_descr type ref to cl_abap_structdescr,
          ls_component   type abap_compdescr,
          lx             type ref to lcx_data_parser_error.

    lo_struc_descr ?= cl_abap_structdescr=>describe_by_data( ls_dummy ).

    " Positive tests ******************************
    test_parse_positive TDATE    '01.02.2015'      '20150201'.
    test_parse_positive TDATE    '1.2.2015'        '20150201'.
    test_parse_positive TCHAR    'ABC'             'ABC'.
    test_parse_positive TSTRING  'The string test' 'The string test'.
    test_parse_positive TALPHA   '100000'          '0000100000'.
    test_parse_positive TNUMBER  '2015'            '2015'.
    test_parse_positive TINTEGER '123'             123.
    test_parse_positive TRAW     '8E'              '8E'.
    test_parse_positive TFLOAT   '1.123456789'     '1.123456789'.
    test_parse_positive TFLOAT   '"1.123456789"'   '1.123456789'. " Quoted data, issue#6

    " Negative tests ******************************
    test_parse_negative TNUMBER  '20ha'      'PF'.

    " Decimal converion tests *********************
    test_parse_positive TDECIMAL '1234.12'         '1234.12'. " Native ABAP format
    test_parse_positive TDECIMAL '-1234.12'        '-1234.12'." Native ABAP format

    " Different amount formats
    test_parse_positive TDECIMAL '-1234,12'        '-1234.12'.
    test_parse_positive TDECIMAL '1234,12'         '1234.12'.
    test_parse_positive TDECIMAL '1 234,12'        '1234.12'.
    test_parse_positive TDECIMAL '14,12'           '14.12'.
    test_parse_positive TDECIMAL '1 234 567,12'    '1234567.12'.

    o->mv_amount_format = '.,'.
    test_parse_positive TDECIMAL '1234,12'         '1234.12'.
    test_parse_positive TDECIMAL '1 234,12'        '1234.12'.
    test_parse_positive TDECIMAL '1.234,12'        '1234.12'.
    test_parse_positive TDECIMAL '14,12'           '14.12'.
    test_parse_positive TDECIMAL '1.234.567,12'    '1234567.12'.

    o->mv_amount_format = ',.'.
    test_parse_positive TDECIMAL '1234.12'         '1234.12'.
    test_parse_positive TDECIMAL '1 234.12'        '1234.12'.
    test_parse_positive TDECIMAL '1,234.12'        '1234.12'.
    test_parse_positive TDECIMAL '14.12'           '14.12'.
    test_parse_positive TDECIMAL '1,234,567.12'    '1234567.12'.

    " Negative decimal tests
    o->mv_amount_format = ' ,'. " Set defaults
    test_parse_negative TDECIMAL '1 234.12' 'PF'.
    test_parse_negative TDECIMAL '1 234_12' 'PF'.
    test_parse_negative TDECIMAL '1234,123' 'PF'. " 3 decimal digits into amount which has just 2
    test_parse_negative TDECIMAL '1234,12_' 'PF'.
    test_parse_negative TDECIMAL 'Not-a-number' 'PF'.

    o->mv_amount_format = '.,'.
    test_parse_negative TDECIMAL '1 234.12' 'PF'.
    test_parse_negative TDECIMAL '1,234.12' 'PF'.

    o->mv_amount_format = ',.'.
    test_parse_negative TDECIMAL '1 234,12' 'PF'.
    test_parse_negative TDECIMAL '1.234,12' 'PF'.

    " Date tests **********************************
    o->mv_date_format = 'MDY'.
    test_parse_positive TDATE    '02012015'    '20150201'.
    o->mv_date_format = 'YMD'.
    test_parse_positive TDATE    '20150201'    '20150201'.
    test_parse_negative TDATE    '2015020'     'DL'.  " Too short
    o->mv_date_format = 'YMD-'.
    test_parse_positive TDATE    '2015-02-01'  '20150201'.
    test_parse_positive TDATE    '2015-2-1'    '20150201'.
    test_parse_positive TDATE    `        `    '00000000'.
    test_parse_positive TDATE    ''            '00000000'.
    o->mv_date_format = 'DMY.'. " Back to default

    " Negative tests
    test_parse_negative TDATE    'AB022015'    'DY'. " Wrong symbols
    test_parse_negative TDATE    '01.02-2015'  'DY'. " Wrong separators
    test_parse_negative TDATE    '01.02.20156' 'DL'. " Too long
    test_parse_negative TDATE    '1.2.201567'  'DP'. " Wrong part length
    test_parse_negative TDATE    '123.2.2015'  'DP'. " Wrong part length
    test_parse_negative TDATE    '01022015'    'DS'. " No separators
    test_parse_negative TDATE    '01.012015'   'DS'. " No second separator
    test_parse_negative TDATE    '40.01.2015'  'DU'. " Incorrect day
    test_parse_negative TDATE    '01.13.2015'  'DU'. " Incorrect month

    " Overflow ************************************
    test_parse_negative TCHAR    'ABCDEFGH123' 'FS'.
    test_parse_negative TNUMBER  '201567'      'FS'.
    test_parse_negative TRAW     '8E8F'        'FS'.
    test_parse_negative TRAW     '8E8'         'FS'.

  endmethod.       "parse_field

  method parse_field_unsupp.
    data:
          begin of ls_dummy,
            struc type ty_dummy,
            float type float,
          end of ls_dummy,
          lo_struc_descr type ref to cl_abap_structdescr,
          ls_component   type abap_compdescr,
          lx             type ref to lcx_data_parser_error.

    lo_struc_descr ?= cl_abap_structdescr=>describe_by_data( ls_dummy ).
    test_parse_negative STRUC '12345' 'UT'.

  endmethod.       "parse_field_unsupp

  method map_head_structure.
    data:
          l_header      type string,
          l_header_bak  type string,
          l_exp_code    type char2,
          l_act_map     type int4_table,
          l_exp_map     type int4_table,
          lx            type ref to lcx_data_parser_error.

    get_dummy_data( importing e_dummy_header = l_header_bak ). " Complete
    get_dummy_data( exporting i_strict       = abap_false      " Reduced
                    importing e_dummy_header = l_header
                              e_map          = l_exp_map ).

    " Positive test
    try.
      l_act_map = o->map_head_structure( i_header = l_header i_strict = abap_false ).
    catch lcx_data_parser_error into lx.
      fail( lx->get_text( ) ).
    endtry.
    assert_equals( act = l_act_map exp = l_exp_map ).

    " Skip MANDT
    l_header = l_header_bak.
    replace first occurrence of 'MANDT' && c_tab in l_header with ''.
    try.
      o->map_head_structure( i_header = l_header i_strict = abap_true ).
    catch lcx_data_parser_error into lx.
      fail( lx->get_text( ) ).
    endtry.

    " Negative tests
    do 5 times.
      clear lx.
      l_header = l_header_bak.

      case sy-index.
        when 1. " Duplicate field names
          replace first occurrence of 'TCHAR' in l_header with 'TDATE'.
          l_exp_code = 'DN'.
        when 2. " Empty field names
          replace first occurrence of 'TCHAR' in l_header with ''.
          l_exp_code = 'EN'.
        when 3. " Unknown field in text
          replace first occurrence of 'TCHAR' in l_header with 'UNKNOWN'.
          l_exp_code = 'MC'.
        when 4. " More fields than in target structure
          l_header = l_header && c_tab && 'EXCESS_FIELD'.
          l_exp_code = 'CN'.
        when 5. " Empty field at the end
          l_header = l_header && c_tab.
          l_exp_code = 'EE'.
      endcase.

      try.
        o->map_head_structure( i_header = l_header i_strict = abap_true ).
      catch lcx_data_parser_error into lx.
        assert_equals( exp = l_exp_code act = lx->code msg = |map_head_structure, case { sy-index }| ).
      endtry.
      assert_not_initial( act = lx msg = |map_head_structure, case { sy-index }| ).

    enddo.

  endmethod.     "map_head_structure

  method parse_line_negative.
    data:
          l_dataline    type string,
          l_header_bak  type string,
          l_exp_code    type char2,
          lt_map        type int4_table,
          lx            type ref to lcx_data_parser_error.

    get_dummy_data( importing e_dummy_header = l_header_bak
                              e_map          = lt_map ).

    " Negative tests
    do 2 times.
      clear lx.
      l_dataline = l_header_bak.

      case sy-index.
        when 1. " More fields than in header
          l_dataline = l_dataline && c_tab && 'EXCESSFLD'.
          l_exp_code = '>H'.
        when 2. " Less fields than in header
          replace first occurrence of c_tab && 'TINTEGER' in l_dataline with ''.
          l_exp_code = '<H'.
      endcase.

      try.
        o->parse_line( i_dataline = l_dataline it_map = lt_map ).
      catch lcx_data_parser_error into lx.
        assert_equals( exp = l_exp_code act = lx->code msg = |parse_line_negative, case { sy-index }| ).
      endtry.
      assert_not_initial( act = lx msg = |parse_line_negative, case { sy-index }| ).

    enddo.
  endmethod.    "parse_line_negative

  method parse_data_empty_line.

    data:
          dummy_tab_exp type tt_dummy,
          dummy_tab_act type tt_dummy,
          l_string      type string,
          lt_data       type table of string,
          lt_map        type int4_table,
          lx            type ref to lcx_data_parser_error.

    get_dummy_data( importing e_dummy_tab    = dummy_tab_exp
                              e_dummy_string = l_string
                              e_map          = lt_map ).

    split l_string at c_crlf into table lt_data.
    delete lt_data index 1.

    " Add empty line at the end *****************************
    try.
      append '' to lt_data.
      o->parse_data( exporting it_data     = lt_data
                               it_map      = lt_map
                     importing e_container = dummy_tab_act ).
    catch lcx_data_parser_error into lx.
      fail( lx->get_text( ) ).
    endtry.

    assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    " Add empty line in the middle ***************************
    try.
      insert '' into lt_data index 2.
      o->parse_data( exporting it_data     = lt_data
                               it_map      = lt_map
                     importing e_container = dummy_tab_act ).
    catch lcx_data_parser_error into lx.
      assert_equals( act = lx->code exp = 'LE' ).
    endtry.
    assert_not_initial( act = lx ).

  endmethod.  "parse_data_empty_line

  method get_dummy_data.

    data:
          l_offs   type i,
          l_fields type i,
          l_string type string.

    clear e_map.

    if i_strict = abap_true.
      l_string = 'MANDT\tTDATE\tTCHAR\tTRAW\tTSTRING\tTALPHA\tTDECIMAL\tTNUMBER\tTINTEGER\tTFLOAT\n'
              && '\t01.01.2015\tTrololo1\t8A\tString1\t100000\t1234567,81\t2015\t1111\t1,12345\n'
              && '\t02.01.2016\tTrololo2\t8B\tString2\t200000\t1234567,82\t2016\t2222\t1,00\n'
              && '\t03.01.2016\tTrololo3\t8C\tString3\t300000\t1234567,83\t2015\t3333\t1\n' .

      do 10 times.
        append sy-index to e_map.
      enddo.

    else.
      l_string = 'TDATE\tTSTRING\tTCHAR\tTDECIMAL\tTNUMBER\n'
              && '01.01.2015\tString1\tTrololo1\t1234567,81\t2015\n'
              && '02.01.2016\tString2\tTrololo2\t1234567,82\t2016\n'
              && '03.01.2016\tString3\tTrololo3\t1234567,83\t2015\n' .

      append '2' to e_map.
      append '5' to e_map.
      append '3' to e_map.
      append '7' to e_map.
      append '8' to e_map.

    endif.

    replace all occurrences of '\t' in l_string with c_tab.
    replace all occurrences of '\n' in l_string with c_crlf.

    clear e_dummy_tab.

    "             TDATE      TCHAR      TSTRING   TDECIMAL    TNUM TRAW  TINT  TALPHA      TFLOAT
    append_dummy '20150101' 'Trololo1' 'String1' '1234567.81' 2015 '8A'  1111 '0000100000' '1.12345'.
    append_dummy '20160102' 'Trololo2' 'String2' '1234567.82' 2016 '8B'  2222 '0000200000' '1.00'.
    append_dummy '20160103' 'Trololo3' 'String3' '1234567.83' 2015 '8C'  3333 '0000300000' '1.00'.

    read table e_dummy_tab into e_dummy_struc index 1.
    e_dummy_string = l_string.

    find first occurrence of c_crlf in l_string match offset l_offs.
    e_dummy_header = l_string+0(l_offs).

  endmethod.       " get_dummy_data

endclass.
