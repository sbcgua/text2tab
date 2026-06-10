class ZCL_TEXT2TAB_CONV definition
  public
  final
  create public.

  public section.

    class-methods typed_to_grid
      importing
        i_typed_tab type standard table
        i_with_header type abap_bool default abap_true
      returning
        value(rt_grid) type zif_text2tab=>ty_grid
      raising
        zcx_text2tab_error.
    class-methods typeless_to_grid
      importing
        i_typeless_tab type standard table
        i_with_header type abap_bool default abap_true
      returning
        value(rt_grid) type zif_text2tab=>ty_grid
      raising
        zcx_text2tab_error.

    class-methods typeless_to_ab
      importing
        i_typeless_tab type standard table
        i_with_header type abap_bool default abap_true
      returning
        value(r_dref) type ref to data
      raising
        zcx_text2tab_error.

    class-methods grid_to_ab
      importing
        i_grid type zif_text2tab=>ty_grid
      returning
        value(r_dref) type ref to data
      raising
        zcx_text2tab_error.
    class-methods grid_to_typeless
      importing
        i_grid type zif_text2tab=>ty_grid
      returning
        value(r_dref) type ref to data
      raising
        zcx_text2tab_error.

  protected section.
  private section.

    class-methods create_ab
      importing
        i_width type i
      returning
        value(r_dref) type ref to data
      raising
        zcx_text2tab_error.
    class-methods index_to_col
      importing
        i_index type i
      returning
        value(r_col) type string
      raising
        zcx_text2tab_error.
ENDCLASS.



CLASS ZCL_TEXT2TAB_CONV IMPLEMENTATION.


  method create_ab.

    data lo_string_descr type ref to cl_abap_elemdescr.
    data lo_ttype type ref to cl_abap_tabledescr.
    data lo_stype type ref to cl_abap_structdescr.
    data lt_components type cl_abap_structdescr=>component_table.
    data ls_component like line of lt_components.

    if i_width <= 0.
      zcx_text2tab_error=>raise(
        msg = 'width of AB table must be >0'
        code = 'A0' ).
    endif.

    lo_string_descr = cl_abap_elemdescr=>get_string( ).

    do i_width times.
      ls_component-name = index_to_col( sy-index ).
      ls_component-type = lo_string_descr.
      append ls_component to lt_components.
    enddo.

    lo_stype = cl_abap_structdescr=>create( lt_components ).
    lo_ttype = cl_abap_tabledescr=>create( lo_stype ).
    create data r_dref type handle lo_ttype.

  endmethod.


  method grid_to_ab.

    field-symbols <dtab> type standard table.
    field-symbols <dtab_line> type any.
    field-symbols <dtab_f> type any.

    field-symbols <src_line> like line of i_grid.
    field-symbols <f> type string.

    read table i_grid index 1 assigning <src_line>.
    if sy-subrc <> 0.
      zcx_text2tab_error=>raise(
        msg  = 'Empty grid'
        code = 'EG' ).
    endif.

    r_dref = create_ab( lines( <src_line> ) ).
    assign r_dref->* to <dtab>.

    loop at i_grid assigning <src_line>.
      append initial line to <dtab> assigning <dtab_line>.
      do.
        assign component sy-index of structure <dtab_line> to <dtab_f>.
        if sy-subrc <> 0.
          exit. " If reached end of dest table - exit, ignore excess grid fields
        endif.
        read table <src_line> index sy-index assigning <f>.
        if sy-subrc <> 0.
          exit. " If reached and of src line, the rest of AB line is empty
        endif.
        <dtab_f> = |{ <f> }|.
      enddo.
    endloop.

  endmethod.


  method grid_to_typeless.

    field-symbols <dtab> type standard table.
    field-symbols <dtab_line> type any.
    field-symbols <dtab_f> type any.

    field-symbols <src_line> like line of i_grid.
    field-symbols <f> type string.

    read table i_grid index 1 assigning <src_line>.
    if sy-subrc <> 0.
      zcx_text2tab_error=>raise(
        msg  = 'Empty grid'
        code = 'EG' ).
    endif.

    data lx_type type ref to cx_sy_struct_creation.
    try.
      r_dref = zcl_text2tab_utils=>create_standard_table_of(
        zcl_text2tab_utils=>get_typeless_struc_descr( <src_line> ) ).
    catch cx_sy_struct_creation into lx_type.
      zcx_text2tab_error=>raise( 'Error creating receiving typeless structure' ).
    endtry.

    assign r_dref->* to <dtab>.

    loop at i_grid assigning <src_line> from 2.
      append initial line to <dtab> assigning <dtab_line>.
      do.
        assign component sy-index of structure <dtab_line> to <dtab_f>.
        if sy-subrc <> 0.
          exit. " If reached end of dest table - exit, ignore excess grid fields
        endif.
        read table <src_line> index sy-index assigning <f>.
        if sy-subrc <> 0.
          exit. " If reached and of src line, the rest of AB line is empty
        endif.
        <dtab_f> = |{ <f> }|.
      enddo.
    endloop.

  endmethod.


  method index_to_col.

    data rem type i.
    data c_idx type i.
    data c_code type x length 2.
    data c_val type sychar02.

    if i_index <= 0.
      zcx_text2tab_error=>raise(
        msg = 'index of AB table must be >0'
        code = 'I0' ).
    endif.

    rem = i_index.

    while rem > 0.
      c_idx = rem mod 26.
      if c_idx = 0.
        c_idx = 26.
        rem = rem - 1. " shift to get non round div
      endif.
      c_code = c_idx + cl_abap_conv_out_ce=>uccp( 'A' ) - 1.
      c_val = cl_abap_conv_in_ce=>uccp( c_code ).
      r_col = c_val && r_col.
      rem = rem div 26.
    endwhile.

  endmethod.


  method typed_to_grid.

    " Use ZCL_TEXT2TAB_SERIALIZER->SERIALIZE_FIELD ?
    rt_grid = typeless_to_grid(
      i_typeless_tab = i_typed_tab
      i_with_header = i_with_header ).

  endmethod.


  method typeless_to_ab.

    field-symbols <line> type any.
    field-symbols <f> type any.

    data lo_ttype type ref to cl_abap_tabledescr.
    data lo_stype type ref to cl_abap_structdescr.
    field-symbols <comp> like line of lo_stype->components.

    field-symbols <dtab> type standard table.
    field-symbols <dtab_line> type any.
    field-symbols <dtab_f> type any.

    lo_ttype ?= cl_abap_typedescr=>describe_by_data( i_typeless_tab ).
    if lo_ttype->get_table_line_type( )->kind <> cl_abap_typedescr=>kind_struct.
      zcx_text2tab_error=>raise(
        msg  = 'Table of structures expected'
        code = 'TO' ).
    endif.
    lo_stype ?= lo_ttype->get_table_line_type( ).

    r_dref = create_ab( lines( lo_stype->components ) ).
    assign r_dref->* to <dtab>.

    if i_with_header = abap_true.
      append initial line to <dtab> assigning <dtab_line>.
      loop at lo_stype->components assigning <comp>.
        assign component sy-tabix of structure <dtab_line> to <dtab_f>.
        <dtab_f> = <comp>-name.
      endloop.
    endif.

    loop at i_typeless_tab assigning <line>.
      append initial line to <dtab> assigning <dtab_line>.
      do.
        assign component sy-index of structure <line> to <f>.
        if sy-subrc <> 0.
          exit.
        endif.
        assign component sy-index of structure <dtab_line> to <dtab_f>.
        if sy-subrc <> 0.
          exit.
        endif.
        <dtab_f> = |{ <f> }|.
      enddo.
    endloop.

  endmethod.


  method typeless_to_grid.

    data str type string.
    field-symbols <dst_line> type string_table.
    field-symbols <line> type any.
    field-symbols <f> type any.

    data lo_ttype type ref to cl_abap_tabledescr.
    data lo_stype type ref to cl_abap_structdescr.
    field-symbols <comp> like line of lo_stype->components.

    lo_ttype ?= cl_abap_typedescr=>describe_by_data( i_typeless_tab ).
    if lo_ttype->get_table_line_type( )->kind <> cl_abap_typedescr=>kind_struct.
      zcx_text2tab_error=>raise(
        msg  = 'Table of structures expected'
        code = 'TO' ).
    endif.

    if i_with_header = abap_true.
      lo_stype ?= lo_ttype->get_table_line_type( ).
      append initial line to rt_grid assigning <dst_line>.
      loop at lo_stype->components assigning <comp>.
        append <comp>-name to <dst_line>.
      endloop.
    endif.

    loop at i_typeless_tab assigning <line>.
      append initial line to rt_grid assigning <dst_line>.
      do.
        assign component sy-index of structure <line> to <f>.
        if sy-subrc <> 0.
          exit.
        endif.
        str = |{ <f> }|.
        append str to <dst_line>.
      enddo.
    endloop.

  endmethod.
ENDCLASS.
