*/--------------------------------------------------------------------------------\
*| This file is part of abap data parser                                          |
*|                                                                                |
*| The MIT License (MIT)                                                          |
*|                                                                                |
*| Copyright (c) 2016 SBCG Team (www.sbcg.com.ua), Alexander Tsybulsky            |
*|                                                                                |
*| Permission is hereby granted, free of charge, to any person obtaining a copy   |
*| of this software and associated documentation files (the "Software"), to deal  |
*| in the Software without restriction, including without limitation the rights   |
*| to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      |
*| copies of the Software, and to permit persons to whom the Software is          |
*| furnished to do so, subject to the following conditions:                       |
*|                                                                                |
*| The above copyright notice and this permission notice shall be included in all |
*| copies or substantial portions of the Software.                                |
*|                                                                                |
*| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     |
*| IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       |
*| FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    |
*| AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         |
*| LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  |
*| OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  |
*| SOFTWARE.                                                                      |
*\--------------------------------------------------------------------------------/
*/--------------------------------------------------------------------------------\
*| Leading developers : Alexander Tsybulsky (atsybulsky@sbcg.com.ua)              |
*|                      Svetlana Shlapak    (sshlapak@sbcg.com.ua)                |
*|--------------------------------------------------------------------------------|
*| project homepage: https://github.com/sbcgua/abap_data_parser                   |
*\--------------------------------------------------------------------------------/

type-pools abap.

**********************************************************************
* Exception class
**********************************************************************

class lcx_data_parser_error definition inheriting from cx_static_check final.
  public section.
    interfaces if_t100_message.

    data methname type string read-only ##NEEDED.
    data msg      type string read-only ##NEEDED.
    data code     type char2  read-only.

    data struc    type string read-only ##NEEDED. " structure type name
    data field    type string read-only ##NEEDED. " field name
    data line     type i      read-only ##NEEDED. " text line number
    data location type string read-only.          " full location of error STRUC:FIELD@LINE

    methods constructor
      importing methname type sys_calls-eventname
                msg      type string
                code     type char2
                struc    type string optional
                field    type string optional
                line     type i optional.

endclass.

class lcx_data_parser_error implementation.

  method constructor.
    super->constructor( ).

    me->methname = |[PARSER->{ methname }]|.
    me->msg      = msg.
    me->code     = code.
    me->struc    = struc.
    me->field    = field.
    me->line     = line.

    if struc is not initial. " Format location
      me->location = struc.
      if field is not initial.
        me->location = |{ me->location }-{ field }|.
      endif.
      if line is not initial.
        me->location = |{ me->location }@{ line }|.
      endif.
    endif.

    me->if_t100_message~t100key-msgid = 'SY'. " & & & &
    me->if_t100_message~t100key-msgno = '499'.
    me->if_t100_message~t100key-attr1 = 'METHNAME'.
    me->if_t100_message~t100key-attr2 = 'MSG'.
    me->if_t100_message~t100key-attr3 = 'LOCATION'.
  endmethod.

endclass. "lcx_data_parser_error

**********************************************************************
* Parser class
**********************************************************************

class lcl_data_parser definition final create private
  friends lcl_test_data_parser.

  public section.

    constants version type string value 'v1.1.1' ##NEEDED.

    constants c_tab   like cl_abap_char_utilities=>horizontal_tab
                        value cl_abap_char_utilities=>horizontal_tab.
    constants c_crlf  like cl_abap_char_utilities=>cr_lf
                        value cl_abap_char_utilities=>cr_lf.

    types:
      tt_string type standard table of string.

    class-methods create
      importing
        i_pattern        type any " target structure or table
        i_amount_format  type char2 optional
      returning
        value(ro_parser) type ref to lcl_data_parser
      raising
        lcx_data_parser_error.

    methods parse
      importing
        i_data      type string
        i_strict    type abap_bool default abap_true
        i_has_head  type abap_bool default abap_true
      exporting
        e_container   type any
        e_head_fields type tt_string
      raising
        lcx_data_parser_error.

  private section.

    data mv_amount_format type char2.
    data mo_struc_descr   type ref to cl_abap_structdescr.
    data mv_current_field type string.
    data mv_line_index    type sy-tabix.
    data mt_head_fields   type tt_string.

    class-methods get_safe_struc_descr
      importing
        i_pattern                     type any
      returning value(ro_struc_descr) type ref to cl_abap_structdescr
      raising
        lcx_data_parser_error.

    methods map_head_structure
      importing
        i_header              type string
        i_strict              type abap_bool
      returning value(rt_map) type int4_table
      raising
        lcx_data_parser_error.

    methods parse_data
      importing
        it_data        type tt_string
        it_map         type int4_table
      exporting
        e_container    type any
      raising
        lcx_data_parser_error.

    methods parse_line
      importing
        i_dataline     type string
        it_map         type int4_table
      exporting
        es_container   type any
      raising
        lcx_data_parser_error.

    methods parse_field
      importing
        is_component type abap_compdescr
        i_value      type string
      exporting
        e_field      type any
      raising
        lcx_data_parser_error.

    methods parse_float
      importing
        i_value      type string
        i_decimals   type abap_compdescr-decimals
      exporting
        e_field      type any
      raising
        lcx_data_parser_error.

    methods apply_conv_exit
      importing
        i_value    type string
        i_convexit type string
      exporting
        e_field    type any
      raising
        lcx_data_parser_error.

    methods raise_error
      importing msg   type string
                code  type char2 optional
      raising   lcx_data_parser_error.

endclass.

**********************************************************************
* IMPLEMENTATION
**********************************************************************

class lcl_data_parser implementation.

  method create.

    data lo_parser  type ref to lcl_data_parser.
    create object lo_parser.

    lo_parser->mo_struc_descr   = get_safe_struc_descr( i_pattern ).
    lo_parser->mv_amount_format = ' ,'. " Defaults

    " Not empty param and not empty decimal separator
    if not ( i_amount_format is initial or i_amount_format+1(1) is initial ).
      lo_parser->mv_amount_format = i_amount_format.
    endif.

    ro_parser = lo_parser.

  endmethod.  "create

  method parse.
    data:
          lt_data      type tt_string,
          lt_map       type int4_table,
          ls_component type abap_compdescr,
          l_header_str type string.

    clear: e_container, e_head_fields.
    clear: mv_line_index, mt_head_fields.

    " Validate params
    if i_has_head = abap_false and i_strict = abap_false.
      raise_error( msg = 'Header line mandatory for non-strict mode' code = 'WP' ). "#EC NOTEXT
    endif.

    " Check container type
    if mo_struc_descr->absolute_name <> get_safe_struc_descr( e_container )->absolute_name.
      raise_error( msg = 'Container type does not fit pattern' code = 'TE' ). "#EC NOTEXT
    endif.

    split i_data at c_crlf into table lt_data.

    " Read and process header line
    if i_has_head = abap_true.
      read table lt_data into l_header_str index 1.
      if sy-subrc <> 0.
        raise_error( msg = 'Data empty' code = 'DE' ). "#EC NOTEXT
      endif.
      if l_header_str is initial.
        raise_error( msg = 'Header line is empty'  code = 'HE' ). "#EC NOTEXT
      endif.

      lt_map = me->map_head_structure( i_header = to_upper( l_header_str )
                                       i_strict = i_strict ).
      delete lt_data index 1.
    else.
      loop at mo_struc_descr->components into ls_component.
        append sy-tabix to lt_map.
        append ls_component-name to mt_head_fields.
      endloop.
    endif.

    " Do parsing
    parse_data( exporting it_data     = lt_data
                          it_map      = lt_map
                importing e_container = e_container ).

    e_head_fields = mt_head_fields.

  endmethod.  "parse

  method get_safe_struc_descr.
    data:
          lo_type_descr  type ref to cl_abap_typedescr,
          lo_table_descr type ref to cl_abap_tabledescr.

    " Identify structure type
    lo_type_descr = cl_abap_typedescr=>describe_by_data( i_pattern ).
    case lo_type_descr->kind.
      when 'T'. " Table
        lo_table_descr ?= lo_type_descr.
        ro_struc_descr ?= lo_table_descr->get_table_line_type( ).
      when 'S'. " Structure
        ro_struc_descr ?= lo_type_descr.
      when others. " Not a table or structure ?
        raise exception type lcx_data_parser_error
          exporting
            methname = 'GET_SAFE_STRUC_DESCR'
            msg      = 'Table or structure patterns only' "#EC NOTEXT
            code     = 'PE'.
    endcase.

  endmethod.  "get_safe_struc_descr

  method map_head_structure.
    data:
          lt_fields    type tt_string,
          l_field_cnt  type i,
          l_mandt_cnt  type i,
          l_tab_cnt    type i,
          lt_dupcheck  type tt_string.

    field-symbols <field> type string.

    split i_header at c_tab into table lt_fields.
    l_field_cnt  = lines( lt_fields ).

    " Check if the line ends with TAB
    find all occurrences of c_tab in i_header match count l_tab_cnt.
    if l_tab_cnt = l_field_cnt. " Line ends with TAB, last empty field is not added to table, see help for 'split'
      raise_error( msg = 'Empty field at the end' code = 'EE' ).   "#EC NOTEXT
    endif.

    " Compare number of fields, check structure similarity
    if i_strict = abap_true.
      read table mo_struc_descr->components with key name = 'MANDT' transporting no fields.
      if sy-subrc is initial. " Found in structure components
        read table lt_fields with key table_line = 'MANDT' transporting no fields.
        if sy-subrc is not initial. " But not found in the file
          l_mandt_cnt = 1. " MANDT field may be skipped
        endif.
      endif.

      if l_field_cnt + l_mandt_cnt <> lines( mo_struc_descr->components ).
        raise_error( msg = 'Different columns number' code = 'CN' ).   "#EC NOTEXT
      endif.
    endif.

    " Check duplicate field names in incoming structure
    lt_dupcheck[] = lt_fields[].
    sort lt_dupcheck[].
    delete adjacent duplicates from lt_dupcheck[].
    if lines( lt_dupcheck ) <> l_field_cnt.
      raise_error( msg = 'Duplicate field names found' code = 'DN' ).   "#EC NOTEXT
    endif.

    " Compare columns names and make map
    loop at lt_fields assigning <field>.
      if <field> is initial. " Check empty fields
        raise_error( msg = 'Empty field name found' code = 'EN' ).   "#EC NOTEXT
      endif.

      read table mo_struc_descr->components with key name = <field> transporting no fields.
      if sy-subrc is initial.
        append sy-tabix to rt_map.
      else.
        raise_error( msg = |Field { <field> } not found in structure| code = 'MC'). "#EC NOTEXT
      endif.
    endloop.

    mt_head_fields = lt_fields. " Save field list to return to the caller

  endmethod.  "map_head_structure

  method parse_data.
    data:
          l_container_kind like cl_abap_typedescr=>kind,
          ref_tab_line     type ref to data.

    field-symbols:
                   <dataline> type string,
                   <table>    type any table,
                   <record>   type any.

    clear e_container.

    " Identify container type and Create temp container record
    l_container_kind = cl_abap_typedescr=>describe_by_data( e_container )->kind.
    create data ref_tab_line type handle mo_struc_descr.
    assign ref_tab_line->* to <record>.
    if l_container_kind = 'T'. " Table
      assign e_container   to <table>.
    endif.

    " Main parsing loop
    loop at it_data assigning <dataline>.
      mv_line_index = sy-tabix.

      if <dataline> is initial. " Check empty lines
        check mv_line_index < lines( it_data ). " Last line of a file may be empty, others - not
        raise_error( msg = 'Empty line cannot be parsed'  code = 'LE' ). "#EC NOTEXT
      endif.

      me->parse_line( exporting i_dataline     = <dataline>
                                it_map         = it_map
                      importing es_container   = <record> ).

      if l_container_kind = 'T'. " Table
        insert <record> into table <table>.
      else.                      " Structure
        e_container = <record>.
        exit. " Only first line goes to structure and then exits
      endif.

    endloop.

  endmethod.  "parse_data

  method parse_line.
    data:
          lt_fields      type table of string,
          l_tab_cnt      type i,
          l_field_value  type string,
          ls_component   type abap_compdescr,
          l_index        type int4.

    field-symbols <field> type any.

    clear es_container.
    split i_dataline at c_tab into table lt_fields.

    " Count TABs, if line ends with TAB last empty field is not added to table, see help for 'split'
    find all occurrences of c_tab in i_dataline match count l_tab_cnt.
    l_tab_cnt = l_tab_cnt + 1. " Number of fields in the line

    " Check field number is the same as in header
    if l_tab_cnt > lines( it_map ).
      raise_error( msg = 'More fields than in header' code = '>H' ). "#EC NOTEXT
    elseif l_tab_cnt < lines( it_map ).
      raise_error( msg = 'Less fields than in header' code = '<H' ). "#EC NOTEXT
    endif.

    " Move data to table line
    loop at lt_fields into l_field_value.
      read table it_map                     into l_index      index sy-tabix. " Read map
      read table mo_struc_descr->components into ls_component index l_index.  " Get component
      if sy-subrc is not initial.
        raise_error( 'No component found?!' ). "#EC NOTEXT
      endif.

      check ls_component-name ne 'MANDT'.   " Skip client fields
      mv_current_field = ls_component-name. " For error handling

      unassign <field>.
      assign component ls_component-name of structure es_container to <field>.
      if <field> is not assigned.
        raise_error( 'Field assign failed?!' ). "#EC NOTEXT
      endif.

      me->parse_field( exporting is_component = ls_component
                                 i_value      = l_field_value
                       importing e_field      = <field> ).
      clear mv_current_field. " For error handling - field is not processed any more
    endloop.

  endmethod.


  method parse_field.
    data: l_mask type string,
          l_len  type i.

    clear e_field.

    case is_component-type_kind.
      when cl_abap_typedescr=>typekind_date. " Date
        call function 'CONVERT_DATE_TO_INTERNAL'
          exporting
            date_external            = i_value
            accept_initial_date      = 'X'
          importing
            date_internal   = e_field
          exceptions
            date_external_is_invalid = 4.

      when cl_abap_typedescr=>typekind_char. " Char + Alpha
        describe field e_field length l_len in character mode.
        if l_len < strlen( i_value ).
          raise_error( msg = 'Value is longer than field' code = 'FS' ). "#EC NOTEXT
        endif.

        describe field e_field edit mask l_mask.
        if l_mask is initial.
          e_field = i_value.
        else.
          shift l_mask left deleting leading '='.
          me->apply_conv_exit( exporting i_value    = i_value
                                         i_convexit = l_mask
                               importing e_field    = e_field ).
        endif.

      when cl_abap_typedescr=>typekind_string. " String
        e_field = i_value.

      when cl_abap_typedescr=>typekind_packed. " Amount
        parse_float( exporting  i_value    = i_value
                                i_decimals = is_component-decimals
                     importing  e_field    = e_field ).

      when cl_abap_typedescr=>typekind_float. " Float
        parse_float( exporting  i_value    = i_value
                                i_decimals = 34 " Abap decimal digit limit ?
                     importing  e_field    = e_field ).

      when cl_abap_typedescr=>typekind_int. " Integer
        if i_value co '0123456789'.
          e_field = i_value.
        else.
          sy-subrc = 4.
        endif.

      when cl_abap_typedescr=>typekind_num. " Numchar
        describe field e_field length l_len in character mode.
        if l_len < strlen( i_value ).
          raise_error( msg = 'Value is longer than field' code = 'FS' ). "#EC NOTEXT
        endif.

        if i_value co '0123456789'.
          e_field = i_value.
        else.
          sy-subrc = 4.
        endif.

      when cl_abap_typedescr=>typekind_hex. " Raw
        describe field e_field length l_len in byte mode.
        if l_len < strlen( i_value ) / 2 + strlen( i_value ) mod 2. " 2 hex-char per byte
          raise_error( msg = 'Value is longer than field' code = 'FS' ). "#EC NOTEXT
        endif.

        try .
          e_field = i_value.
        catch cx_sy_conversion_no_raw cx_sy_conversion_error.
          sy-subrc = 4.
        endtry.

      when others.
        raise_error( msg = 'Unsupported field type' code = 'UT' ). "#EC NOTEXT

    endcase.

    if sy-subrc is not initial.
      raise_error( msg = 'Field parsing failed' code = 'PF' ). "#EC NOTEXT
    endif.

  endmethod.  "parse_field

  method parse_float.
    data:
          l_decimal_sep  type c,
          l_thousand_sep type c,
          l_tmp          type string,
          l_regex        type string.

    l_thousand_sep = mv_amount_format+0(1).
    l_decimal_sep  = mv_amount_format+1(1).

    try .
      e_field = i_value. " Try native format first - xxxx.xx
    catch cx_sy_arithmetic_error cx_sy_conversion_error.

      l_tmp   = i_value.
      l_regex = '^-?\d{1,3}(T\d{3})*(\D\d{1,C})?$'. "#EC NOTEXT
      condense l_tmp no-gaps.
      replace 'C' in l_regex with |{ i_decimals }|.

      " Validate number
      find first occurrence of l_thousand_sep in l_tmp.
      if sy-subrc is initial. " Found
        replace 'T' in l_regex with l_thousand_sep.
      else.
        replace 'T' in l_regex with ''.
      endif.

      replace 'D' in l_regex with l_decimal_sep.
      find all occurrences of regex l_regex in l_tmp match count sy-tabix.

      if sy-tabix = 1.
        if not l_thousand_sep is initial.  " Remove thousand separators
          replace all occurrences of l_thousand_sep in l_tmp with ''.
        endif.

        if l_decimal_sep <> '.'.           " Replace decimal separator
          replace l_decimal_sep in l_tmp with '.'.
        endif.

        try. " Try converting again
          e_field = l_tmp.
        catch cx_sy_arithmetic_error cx_sy_conversion_error.
          raise_error( msg = 'Float number parsing failed' code = 'PF' ). "#EC NOTEXT
        endtry.
      else. " Not matched
        raise_error( msg = 'Float number parsing failed' code = 'PF' ). "#EC NOTEXT
      endif.
    endtry.

  endmethod.  "parse_float

  method apply_conv_exit.

    data l_fm_name type rs38l_fnam value 'CONVERSION_EXIT_XXXXX_INPUT'.

    replace first occurrence of 'XXXXX' in l_fm_name with i_convexit.

    call function 'FUNCTION_EXISTS'
      exporting
        funcname           = l_fm_name
      exceptions
        function_not_exist = 1
        others             = 2.

    if sy-subrc <> 0.
      raise_error( msg = 'Conversion exit not found' code = 'EM' ). "#EC NOTEXT
    endif.

    call function l_fm_name
      exporting
        input  = i_value
      importing
        output = e_field
      exceptions
        others = 1.

    if sy-subrc <> 0.
      raise_error( msg = 'Conversion exit failed' code = 'EF' ). "#EC NOTEXT
    endif.

  endmethod.  "apply_conv_exit

  method raise_error.
    data: sys_call    type sys_calls,
          sys_stack   type sys_callst,
          l_struc     type string.

    call function 'SYSTEM_CALLSTACK' " Get stack information
      exporting
        max_level    = 2
      importing
        et_callstack = sys_stack.

    read table sys_stack into sys_call index 2.

    if mo_struc_descr is bound.
      l_struc = mo_struc_descr->get_relative_name( ).
    endif.

    raise exception type lcx_data_parser_error
      exporting
        methname = sys_call-eventname
        msg      = msg
        code     = code
        struc    = l_struc
        field    = mv_current_field
        line     = mv_line_index.

  endmethod.  "raise_error

endclass.