report ztext2tab_benchmarks.

**********************************************************************
* ZTEST_BENCHMARKS CONTRIB
**********************************************************************

class lcl_benchmark definition final.
  public section.
    methods constructor
      importing
        io_object type ref to object
        iv_method type string
        iv_times  type i.
    methods run.
    methods print.

  private section.
    data mo_object type ref to object.
    data mv_method type string.
    data mv_times type i.
    data mv_diff type p decimals 6.
endclass.

class lcl_benchmark implementation.

  method constructor.
    mo_object = io_object.
    mv_method = to_upper( iv_method ).
    mv_times = iv_times.
  endmethod.

  method run.
    data:
      lv_sta_time     type timestampl,
      lv_end_time     type timestampl.

    get time stamp field lv_sta_time.
    do mv_times times.
      call method mo_object->(mv_method).
    enddo.
    get time stamp field lv_end_time.
    mv_diff  = lv_end_time - lv_sta_time ##TIME_ARITH.

  endmethod.

  method print.
    write: /(30) mv_method, 'results', mv_diff  exponent 0.
    uline.
  endmethod.

endclass.

class lcl_runner_base definition.
  public section.

    methods run
      importing
        iv_method type string.

  private section.
    data mv_num_rounds type i.

endclass.

class lcl_runner_base implementation.

  method run.

    data lo_benchmark type ref to lcl_benchmark.

    create object lo_benchmark
      exporting
        io_object = me
        iv_method = iv_method
        iv_times  = mv_num_rounds.

    lo_benchmark->run( ).
    lo_benchmark->print( ).

  endmethod.

endclass.

**********************************************************************
* END-OF ZTEST_BENCHMARKS CONTRIB
**********************************************************************

class lcl_app definition final.
  public section.

    types:
      begin of ty_dummy,
        mandt    type mandt,
        tdate    type datum,
        tchar    type c length 8,
        traw     type x length 1,
        tstring  type string,
        talpha   type veri_alpha,
        tdecimal type p length 13 decimals 2, " dmbtr
        tnumber  type n length 4,
        tinteger type i,
        tfloat   type f,
        extra1   type string,
        extra2   type string,
        extra3   type string,
        extra4   type string,
        extra5   type string,
        extra6   type string,
        extra7   type string,
        extra8   type string,
        extra9   type string,
        extra10  type string,
        extra11  type string,
        extra12  type string,
      end of ty_dummy,
      tt_dummy type standard table of ty_dummy with default key.

    methods prepare_data.
    methods serialize.
    methods serialize_html.
    methods serialize_with_fields_only.

    class-methods main.
    methods run
      importing
        iv_method type string.

  private section.
    data mv_num_rounds type i.
    data mt_dummy type tt_dummy.

endclass.

class lcl_app implementation.

  method prepare_data.

    data ls_dummy type ty_dummy.

    ls_dummy-tdate    = '20150101'.
    ls_dummy-tchar    = 'Trololo1'.
    ls_dummy-tstring  = 'Trololo2'.
    ls_dummy-tdecimal = '1234567.81'.
    ls_dummy-tnumber  = 2020.
    ls_dummy-traw     = '8A'.
    ls_dummy-tinteger = 1234.
    ls_dummy-talpha   = '0000100000'.
    ls_dummy-tfloat   = '1.12345'.

    ls_dummy-extra1  = '1'.
    ls_dummy-extra2  = '1'.
    ls_dummy-extra3  = '1'.
    ls_dummy-extra4  = '1'.
    ls_dummy-extra5  = '1'.
    ls_dummy-extra6  = '1'.
    ls_dummy-extra7  = '1'.
    ls_dummy-extra8  = '1'.
    ls_dummy-extra9  = '1'.
    ls_dummy-extra10 = '1'.
    ls_dummy-extra11 = '1'.
    ls_dummy-extra12 = '1'.

    do 50 times.
      append ls_dummy to mt_dummy.
    enddo.

  endmethod.

  method serialize.

    data lo_serializer type ref to zcl_text2tab_serializer.

    try.
      lo_serializer = zcl_text2tab_serializer=>create( ).
      do 100 times.
        lo_serializer->serialize( mt_dummy ).
      enddo.
    catch zcx_text2tab_error.
    endtry.

  endmethod.

  method serialize_html.

    data lo_serializer type ref to zcl_text2tab_serializer.

    try.
      lo_serializer = zcl_text2tab_serializer=>create( )->as_html( ).
      do 100 times.
        lo_serializer->serialize( mt_dummy ).
      enddo.
    catch zcx_text2tab_error.
    endtry.

  endmethod.

  method serialize_with_fields_only.

    data lo_serializer type ref to zcl_text2tab_serializer.
    data lt_fields type zif_text2tab=>tt_fields_list.

    append 'MANDT' to lt_fields.
    append 'TDATE' to lt_fields.
    append 'TCHAR' to lt_fields.
    append 'TRAW' to lt_fields.
    append 'TSTRING' to lt_fields.
    append 'TALPHA' to lt_fields.
    append 'TDECIMAL' to lt_fields.
    append 'TNUMBER' to lt_fields.
    append 'TINTEGER' to lt_fields.
    append 'TFLOAT' to lt_fields.

    try.
      lo_serializer = zcl_text2tab_serializer=>create( ).
      do 100 times.
        lo_serializer->serialize(
          i_data        = mt_dummy
          i_fields_only = lt_fields ).
      enddo.
    catch zcx_text2tab_error.
    endtry.

  endmethod.

  method run.

    data lo_benchmark type ref to lcl_benchmark.

    create object lo_benchmark
      exporting
        io_object = me
        iv_method = iv_method
        iv_times  = mv_num_rounds.

    lo_benchmark->run( ).
    lo_benchmark->print( ).

  endmethod.

  method main.

    data lo_app type ref to lcl_app.
    create object lo_app.

    lo_app->mv_num_rounds = 100.
    lo_app->run( 'serialize' ).
    lo_app->run( 'serialize_html' ).
    lo_app->run( 'serialize_with_fields_only' ).

  endmethod.

endclass.

start-of-selection.

  lcl_app=>main( ).
