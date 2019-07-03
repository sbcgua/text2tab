class ZCL_TEXT2TAB_PARSER definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  types:
    begin of ty_field_name_map,
      from type char40,
      to   type abap_compname,
    end of ty_field_name_map .
  types:
    tt_field_name_map type standard table of ty_field_name_map with key from .
  types:
    th_field_name_map type hashed table of ty_field_name_map with unique key from .

  constants C_TAB like CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB value CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB. "#EC NOTEXT
  constants C_CRLF like CL_ABAP_CHAR_UTILITIES=>CR_LF value CL_ABAP_CHAR_UTILITIES=>CR_LF. "#EC NOTEXT
  constants C_LF like CL_ABAP_CHAR_UTILITIES=>NEWLINE value CL_ABAP_CHAR_UTILITIES=>NEWLINE. "#EC NOTEXT

  class-methods CREATE
    importing
      !I_PATTERN type ANY         " target structure or table
      !I_AMOUNT_FORMAT type CHAR2 optional
      !I_DATE_FORMAT type CHAR4 optional
      !I_BEGIN_COMMENT type CHAR1 optional
    returning
      value(RO_PARSER) type ref to ZCL_TEXT2TAB_PARSER
    raising
      ZCX_TEXT2TAB_ERROR .
  class-methods CREATE_TYPELESS
    returning
      value(RO_PARSER) type ref to ZCL_TEXT2TAB_PARSER
    raising
      ZCX_TEXT2TAB_ERROR .
  methods PARSE
    importing
      !I_DATA type STRING
      !I_STRICT type ABAP_BOOL default ABAP_TRUE
      !I_HAS_HEAD type ABAP_BOOL default ABAP_TRUE
      !I_RENAME_FIELDS type ANY optional
    exporting
      !E_CONTAINER type ANY
      !E_HEAD_FIELDS type STRING_TABLE
    raising
      ZCX_TEXT2TAB_ERROR .
  class-methods CHECK_VERSION_FITS
    importing
      !I_REQUIRED_VERSION type STRING
    returning
      value(R_FITS) type ABAP_BOOL .
protected section.
private section.

  data MV_AMOUNT_FORMAT type CHAR2 .
  data MV_DATE_FORMAT type CHAR4 .
  data MO_STRUC_DESCR type ref to CL_ABAP_STRUCTDESCR .
  data MV_CURRENT_FIELD type STRING .
  data MV_LINE_INDEX type SY-TABIX .
  data MV_IS_TYPELESS type ABAP_BOOL .
  data MV_BEGIN_COMMENT type CHAR1 .
  class ZCL_TEXT2TAB_UTILS definition load .
  data MT_COMPONENTS type ZCL_TEXT2TAB_UTILS=>TT_COMP_DESCR .

  class-methods ADOPT_RENAMES
    importing
      !I_RENAME_FIELDS type ANY
    returning
      value(R_RENAME_MAP) type TT_FIELD_NAME_MAP
    raising
      ZCX_TEXT2TAB_ERROR .
  methods PARSE_TYPEFULL
    importing
      !I_DATA type STRING
      !I_STRICT type ABAP_BOOL default ABAP_TRUE
      !I_HAS_HEAD type ABAP_BOOL default ABAP_TRUE
      !I_RENAME_MAP type TH_FIELD_NAME_MAP
    exporting
      !E_CONTAINER type ANY
      !E_HEAD_FIELDS type STRING_TABLE
    raising
      ZCX_TEXT2TAB_ERROR .
  methods PARSE_TYPELESS
    importing
      !I_DATA type STRING
      !I_RENAME_MAP type TH_FIELD_NAME_MAP
    exporting
      !E_CONTAINER type ref to DATA
      !E_HEAD_FIELDS type STRING_TABLE
    raising
      ZCX_TEXT2TAB_ERROR .
  methods PARSE_HEAD_LINE
    importing
      !I_STRICT type ABAP_BOOL
      !I_RENAME_MAP type TH_FIELD_NAME_MAP
    changing
      !CT_DATA type STRING_TABLE
      !CT_MAP type INT4_TABLE
      !CT_HEAD_FIELDS type STRING_TABLE
    raising
      ZCX_TEXT2TAB_ERROR .
  class-methods GET_SAFE_STRUC_DESCR
    importing
      !I_PATTERN type ANY
    returning
      value(RO_STRUC_DESCR) type ref to CL_ABAP_STRUCTDESCR
    raising
      ZCX_TEXT2TAB_ERROR .
  methods MAP_HEAD_STRUCTURE
    importing
      !I_HEADER type STRING
      !I_STRICT type ABAP_BOOL
      !I_RENAME_MAP type TH_FIELD_NAME_MAP
    exporting
      !ET_MAP type INT4_TABLE
      !ET_HEAD_FIELDS type STRING_TABLE
    raising
      ZCX_TEXT2TAB_ERROR .
  methods PARSE_DATA
    importing
      !IT_DATA type STRING_TABLE
      !IT_MAP type INT4_TABLE
    exporting
      !E_CONTAINER type ANY
    raising
      ZCX_TEXT2TAB_ERROR .
  methods PARSE_LINE
    importing
      !I_DATALINE type STRING
      !IT_MAP type INT4_TABLE
    exporting
      !ES_CONTAINER type ANY
    raising
      ZCX_TEXT2TAB_ERROR .
  methods PARSE_FIELD
    importing
      !IS_COMPONENT type ZCL_TEXT2TAB_UTILS=>TY_COMP_DESCR
      !I_VALUE type STRING
    exporting
      !E_FIELD type ANY
    raising
      ZCX_TEXT2TAB_ERROR .
  methods PARSE_FLOAT
    importing
      !I_VALUE type STRING
      !I_DECIMALS type ABAP_COMPDESCR-DECIMALS
    exporting
      !E_FIELD type ANY
    raising
      ZCX_TEXT2TAB_ERROR .
  methods PARSE_DATE
    importing
      !I_VALUE type STRING
    exporting
      !E_FIELD type D
    raising
      ZCX_TEXT2TAB_ERROR .
  methods APPLY_CONV_EXIT
    importing
      !I_VALUE type STRING
      !I_CONVEXIT type ABAP_EDITMASK
    exporting
      !E_FIELD type ANY
    raising
      ZCX_TEXT2TAB_ERROR .
  methods RAISE_ERROR
    importing
      !MSG type STRING
      !CODE type zcx_text2tab_error=>ty_rc optional
    raising
      ZCX_TEXT2TAB_ERROR .
  class-methods BREAK_TO_LINES
    importing
      !I_TEXT type STRING
      !I_BEGIN_COMMENT type CHAR1
    returning
      value(RT_TAB) type STRING_TABLE .
ENDCLASS.



CLASS ZCL_TEXT2TAB_PARSER IMPLEMENTATION.


method ADOPT_RENAMES.
  data lo_type type ref to cl_abap_typedescr.
  data lo_ref_type type ref to cl_abap_typedescr.
  data ls_rename like line of r_rename_map.

  if i_rename_fields is initial.
    return.
  endif.

  lo_type = cl_abap_typedescr=>describe_by_data( i_rename_fields ).
  lo_ref_type = cl_abap_typedescr=>describe_by_data( r_rename_map ).

  if lo_type->type_kind = cl_abap_typedescr=>typekind_table and lo_type->absolute_name = lo_ref_type->absolute_name.
    field-symbols <tab> type standard table.
    assign i_rename_fields to <tab>.
    loop at <tab> into ls_rename.
      ls_rename-from = to_upper( ls_rename-from ).
      ls_rename-to   = to_upper( ls_rename-to ).
      append ls_rename to r_rename_map.
    endloop.
  elseif lo_type->type_kind = cl_abap_typedescr=>typekind_char or lo_type->type_kind = cl_abap_typedescr=>typekind_string.
    data lt_renames type string_table.
    field-symbols <str> type string.
    split i_rename_fields at ';' into table lt_renames.
    delete lt_renames where table_line is initial.
    loop at lt_renames assigning <str>.
      clear ls_rename.
      <str> = to_upper( <str> ).
      split <str> at ':' into ls_rename-from ls_rename-to.
      if ls_rename-from is initial or ls_rename-to is initial.
        zcx_text2tab_error=>raise(
          methname = 'adopt_renames'
          msg      = 'Wrong rename pair'
          code     = 'WR' ). "#EC NOTEXT
      endif.
      append ls_rename to r_rename_map.
    endloop.
  else.
    zcx_text2tab_error=>raise(
      methname = 'adopt_renames'
      msg      = 'Wrong rename fields type'
      code     = 'WY' ). "#EC NOTEXT
  endif.

endmethod.


method APPLY_CONV_EXIT.

  data l_fm_name type rs38l_fnam value 'CONVERSION_EXIT_XXXXX_INPUT'.

  replace first occurrence of 'XXXXX' in l_fm_name with i_convexit.
  if zcl_text2tab_utils=>function_exists( l_fm_name ) = abap_false.
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


method break_to_lines.
  data:
    l_found type i,
    l_break type string value c_crlf.
  field-symbols: <line> type string.

  " Detect line break
  l_found = find( val = i_text sub = c_crlf ).
  if l_found < 0.
    l_found = find( val = i_text sub = c_lf ).
    if l_found >= 0.
      l_break = c_lf.
    endif.
  endif.

  split i_text at l_break into table rt_tab.

  if i_begin_comment <> space.
    loop at rt_tab assigning <line>.
      try.
          if <line>+0(1) = i_begin_comment.
            delete rt_tab index sy-tabix.
          endif.
        catch cx_sy_range_out_of_bounds.
          " if the row only consist of a linefeed. Some text editors add always a line feed at the end of the document
          delete rt_tab index sy-tabix.
      endtry.
    endloop.
  endif.

endmethod.


method CHECK_VERSION_FITS.

  r_fits = zcl_text2tab_utils=>check_version_fits(
    i_current_version  = zif_text2tab_constants=>version
    i_required_version = i_required_version ).

endmethod.


method CREATE.

  create object ro_parser.

  ro_parser->mo_struc_descr   = get_safe_struc_descr( i_pattern ).
  ro_parser->mt_components    = zcl_text2tab_utils=>describe_struct( ro_parser->mo_struc_descr ).
  ro_parser->mv_amount_format = ' ,'.   " Defaults
  ro_parser->mv_date_format   = 'DMY.'. " Defaults

  " Not empty param and not empty decimal separator
  if not ( i_amount_format is initial or i_amount_format+1(1) is initial ).
    ro_parser->mv_amount_format = i_amount_format.
  endif.

  " Not empty param and not empty decimal separator
  if i_date_format is not initial.
    zcl_text2tab_utils=>validate_date_format_spec( i_date_format ).
    ro_parser->mv_date_format = i_date_format.
  endif.

  ro_parser->mv_begin_comment = i_begin_comment.

endmethod.  "create


method CREATE_TYPELESS.
  create object ro_parser.
  ro_parser->mv_is_typeless = abap_true.
endmethod.


method GET_SAFE_STRUC_DESCR.

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
      raise exception type zcx_text2tab_error
        exporting
          methname = 'GET_SAFE_STRUC_DESCR'
          msg      = 'Table or structure patterns only' "#EC NOTEXT
          code     = 'PE'.
  endcase.

endmethod.  "get_safe_struc_descr


method MAP_HEAD_STRUCTURE.
  data:
        l_field_cnt  type i,
        l_mandt_cnt  type i,
        l_tab_cnt    type i,
        lt_dupcheck  type string_table.

  clear: et_map, et_head_fields.
  assert not ( i_strict = abap_true and mv_is_typeless = abap_true ).

  field-symbols <field> type string.

  split i_header at c_tab into table et_head_fields.
  l_field_cnt = lines( et_head_fields ).

  " Check if the line ends with TAB
  find all occurrences of c_tab in i_header match count l_tab_cnt.
  if l_tab_cnt = l_field_cnt. " Line ends with TAB, last empty field is not added to table, see help for 'split'
    raise_error( msg = 'Empty field at the end' code = 'EE' ).   "#EC NOTEXT
  endif.

  " Compare number of fields, check structure similarity
  if i_strict = abap_true.
    read table mt_components with key name = 'MANDT' transporting no fields.
    if sy-subrc is initial. " Found in structure components
      read table et_head_fields with key table_line = 'MANDT' transporting no fields.
      if sy-subrc is not initial. " But not found in the file
        l_mandt_cnt = 1. " MANDT field may be skipped
      endif.
    endif.

    if l_field_cnt + l_mandt_cnt <> lines( mt_components ).
      raise_error( msg = 'Different columns number' code = 'CN' ).   "#EC NOTEXT
    endif.
  endif.

  " Check duplicate field names in incoming structure
  lt_dupcheck = et_head_fields.
  sort lt_dupcheck.
  delete adjacent duplicates from lt_dupcheck.
  if lines( lt_dupcheck ) <> l_field_cnt.
    raise_error( msg = 'Duplicate field names found' code = 'DN' ).   "#EC NOTEXT
  endif.

  " Check duplicates after rename
  data ls_rename like line of i_rename_map.
  if i_rename_map is not initial.
    loop at et_head_fields assigning <field>.
      read table i_rename_map with key from = <field> into ls_rename.
      check sy-subrc is initial.
      <field> = ls_rename-to.
    endloop.

    lt_dupcheck = et_head_fields.
    sort lt_dupcheck.
    delete adjacent duplicates from lt_dupcheck.
    if lines( lt_dupcheck ) <> l_field_cnt.
      raise_error( msg = 'Duplicate field names found after rename' code = 'DR' ).   "#EC NOTEXT
    endif.
  endif.

  " Compare columns names and make map
  loop at et_head_fields assigning <field>.
    if <field> is initial. " Check empty fields
      raise_error( msg = 'Empty field name found' code = 'EN' ).   "#EC NOTEXT
    endif.
    " ~ following CL_ABAP_STRUCTDESCR->CHECK_COMPONENT_TABLE
    if strlen( <field> ) > abap_max_comp_name_ln or <field> cn 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789'.
      raise_error( msg = 'Incorrect field name (long or special chars used)' code = 'WE' ). "#EC NOTEXT
    endif.
    if mv_is_typeless = abap_false.
      read table mt_components with key name = <field> transporting no fields.
      if sy-subrc is initial.
        append sy-tabix to et_map.
      else.
        raise_error( msg = |Field { <field> } not found in structure| code = 'MC' ). "#EC NOTEXT
      endif.
    else.
      append sy-tabix to et_map. " direct map
    endif.
  endloop.

endmethod.  "map_head_structure


method PARSE.

  data lt_rename_map type th_field_name_map.
  lt_rename_map = adopt_renames( i_rename_fields ).

  if mv_is_typeless = abap_true.
    if cl_abap_typedescr=>describe_by_data( e_container )->type_kind <> cl_abap_typedescr=>typekind_dref.
      raise_error( msg = 'Typeless parsing require dref as the container'  code = 'DR' ). "#EC NOTEXT
    endif.

    parse_typeless(
      exporting
        i_data       = i_data
        i_rename_map = lt_rename_map
      importing
        e_container   = e_container
        e_head_fields = e_head_fields ).
  else.
    parse_typefull(
      exporting
        i_data       = i_data
        i_has_head   = i_has_head
        i_strict     = i_strict
        i_rename_map = lt_rename_map
      importing
        e_container   = e_container
        e_head_fields = e_head_fields ).
  endif.

endmethod.  " parse


method PARSE_DATA.

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

    me->parse_line(
      exporting
        i_dataline     = <dataline>
        it_map         = it_map
      importing
        es_container   = <record> ).

    if l_container_kind = 'T'. " Table
      insert <record> into table <table>.
    else.                      " Structure
      e_container = <record>.
      exit. " Only first line goes to structure and then exits
    endif.

  endloop.

endmethod.  "parse_data


method PARSE_DATE.

  data: l_cursor  type i,
        l_iter    type i,
        l_part    type c,
        l_size    type i,
        l_offs    type i,
        l_home    type i,
        l_pad     type i,
        l_stencil type numc4,
        l_rawdate type char8,
        l_charset type char11 value '0123456789',
        l_sep     type c.

  clear e_field.
  l_sep           = mv_date_format+3(1).
  l_charset+10(1) = l_sep.

  if i_value is initial or i_value co ` `. " Empty string -> empty date
    return.
  endif.

  if not i_value co l_charset.  " Check wrong symbols
    raise_error( msg = 'Date contains invalid symbols' code = 'DY' ). "#EC NOTEXT
  endif.

  " Not separated date must be 8 chars, separated not more than 10
  if l_sep <> space and strlen( i_value ) > 10  or l_sep = space and strlen( i_value ) <> 8.
    raise_error( msg = 'Incorrect date length' code = 'DL' ). "#EC NOTEXT
  endif.

  do 3 times.
    l_iter = sy-index - 1.
    l_part = mv_date_format+l_iter(1).

    case l_part.
      when 'D'.
        l_size = 2.
        l_home = 6.
      when 'M'.
        l_size = 2.
        l_home = 4.
      when 'Y'.
        l_size = 4.
        l_home = 0.
      when others.
        raise_error( msg = 'Wrong date format' ). "#EC NOTEXT
    endcase.

    if l_sep is initial. " No seps
      l_rawdate+l_home(l_size) = i_value+l_cursor(l_size).
      l_cursor                 = l_cursor + l_size.
    else.
      if l_iter = 2. " Last part
        l_offs = strlen( i_value+l_cursor ).
      else.
        find first occurrence of l_sep in i_value+l_cursor match offset l_offs.
      endif.
      if sy-subrc <> 0.
        raise_error( msg = 'Date separator is missing' code = 'DS' ). "#EC NOTEXT
      endif.
      if l_offs > l_size.
        raise_error( msg = 'Too long date part' code = 'DP' ). "#EC NOTEXT
      endif.
      l_stencil                = i_value+l_cursor(l_offs).
      l_pad                    = 4 - l_size. " Offset within stencil
      l_rawdate+l_home(l_size) = l_stencil+l_pad(l_size).
      l_cursor                 = l_cursor + l_offs + 1. " Including separator
    endif.

  enddo.

  " Native convert
  try.
    cl_abap_datfm=>conv_date_ext_to_int(
      exporting
        im_datext   = l_rawdate
        im_datfmdes = '4' " YYYY.MM.DD
      importing
        ex_datint   = e_field ).
    catch cx_abap_datfm.
      raise_error( msg = 'Date format unknown' code = 'DU' ). "#EC NOTEXT
  endtry.

endmethod.  "parse_date


method PARSE_FIELD.

  data: l_unquoted type string,
        l_len      type i.

  clear e_field.

  " Unquote field
  l_len = strlen( i_value ).
  if l_len >= 2
     and substring( val = i_value off = 0         len = 1 ) = '"'
     and substring( val = i_value off = l_len - 1 len = 1 ) = '"'.
    l_unquoted = substring( val = i_value off = 1 len = l_len - 2 ).
  else.
    l_unquoted = i_value.
  endif.
  clear l_len.

  " Parse depending on output type
  case is_component-type_kind.
    when cl_abap_typedescr=>typekind_date. " Date
      parse_date( exporting  i_value = l_unquoted
                  importing  e_field = e_field ).

    when cl_abap_typedescr=>typekind_char. " Char + Alpha
      if is_component-output_length < strlen( l_unquoted ).
        raise_error( msg = 'Value is longer than field' code = 'FS' ). "#EC NOTEXT
      endif.

      if is_component-edit_mask is initial.
        e_field = l_unquoted.
      else.
        me->apply_conv_exit(
          exporting
            i_value    = l_unquoted
            i_convexit = is_component-edit_mask
          importing
            e_field    = e_field ).
      endif.

    when cl_abap_typedescr=>typekind_string. " String
      e_field = l_unquoted.

    when cl_abap_typedescr=>typekind_packed. " Amount
      parse_float(
        exporting
          i_value    = l_unquoted
          i_decimals = is_component-decimals
        importing
          e_field    = e_field ).

    when cl_abap_typedescr=>typekind_float. " Float
      parse_float(
        exporting
          i_value    = l_unquoted
          i_decimals = 34 " Abap decimal digit limit ?
        importing
          e_field    = e_field ).

    when cl_abap_typedescr=>typekind_int
      or cl_abap_typedescr=>typekind_int1
      or cl_abap_typedescr=>typekind_int2. " Integer
      if l_unquoted co '0123456789'.
        e_field = l_unquoted.
      else.
        sy-subrc = 4.
      endif.

    when cl_abap_typedescr=>typekind_time. " Time
      e_field = l_unquoted.

    when cl_abap_typedescr=>typekind_num. " Numchar
      if is_component-output_length < strlen( l_unquoted ).
        raise_error( msg = 'Value is longer than field' code = 'FS' ). "#EC NOTEXT
      endif.

      if l_unquoted co '0123456789'.
        e_field = l_unquoted.
      else.
        sy-subrc = 4.
      endif.

    when cl_abap_typedescr=>typekind_hex. " Raw
      if is_component-length < strlen( l_unquoted ) / 2 + strlen( l_unquoted ) mod 2. " 2 hex-char per byte
        raise_error( msg = 'Value is longer than field' code = 'FS' ). "#EC NOTEXT
      endif.

      try .
        e_field = l_unquoted.
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


method PARSE_FLOAT.

  data:
        l_decimal_sep  type c,
        l_thousand_sep type c,
        l_tmp          type string,
        l_regex        type string.

  l_thousand_sep = mv_amount_format+0(1).
  l_decimal_sep  = mv_amount_format+1(1).
  clear e_field.

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


method PARSE_HEAD_LINE.
  data l_header_str type string.

  read table ct_data into l_header_str index 1.
  if sy-subrc <> 0.
    raise_error( msg = 'Data empty' code = 'DE' ). "#EC NOTEXT
  endif.
  if l_header_str is initial.
    raise_error( msg = 'Header line is empty'  code = 'HE' ). "#EC NOTEXT
  endif.

  me->map_head_structure(
    exporting
      i_rename_map = i_rename_map
      i_header = to_upper( l_header_str )
      i_strict = i_strict
    importing
      et_map         = ct_map
      et_head_fields = ct_head_fields ).

  delete ct_data index 1.

endmethod.


method PARSE_LINE.

  data:
        lt_fields      type table of string,
        l_tab_cnt      type i,
        l_field_value  type string,
        ls_component   like line of mt_components,
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
    read table it_map        into l_index      index sy-tabix. " Read map
    read table mt_components into ls_component index l_index.  " Get component
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

    if mv_is_typeless = abap_true.
      <field> = l_field_value.
    else.
      me->parse_field(
        exporting
          is_component = ls_component
          i_value      = l_field_value
        importing
          e_field      = <field> ).
    endif.

    clear mv_current_field. " For error handling - field is not processed any more
  endloop.

endmethod.


method PARSE_TYPEFULL.

  data:
        lt_data      type string_table,
        lt_map       type int4_table,
        ls_component like line of mt_components.

  clear: e_container, e_head_fields.
  clear: mv_line_index.

  " Validate params
  if i_has_head = abap_false and i_strict = abap_false.
    raise_error( msg = 'Header line mandatory for non-strict mode' code = 'WP' ). "#EC NOTEXT
  endif.

  " Check container type
  if mo_struc_descr->absolute_name <> get_safe_struc_descr( e_container )->absolute_name.
    raise_error( msg = 'Container type does not fit pattern' code = 'TE' ). "#EC NOTEXT
  endif.

  lt_data = break_to_lines( i_text = i_data i_begin_comment = mv_begin_comment ).

  " Read and process header line
  if i_has_head = abap_true.
    parse_head_line(
      exporting
        i_rename_map = i_rename_map
        i_strict     = i_strict
      changing
        ct_data        = lt_data
        ct_head_fields = e_head_fields
        ct_map         = lt_map ).
  else.
    loop at mt_components into ls_component.
      append sy-tabix to lt_map.
      append ls_component-name to e_head_fields.
    endloop.
  endif.

  " Do parsing
  parse_data(
    exporting
      it_data     = lt_data
      it_map      = lt_map
    importing
      e_container = e_container ).

endmethod.  "parse_typefull


  METHOD PARSE_TYPELESS.
    data lt_data type string_table.
    data lt_map type int4_table.
    field-symbols <f> like line of e_head_fields.

    lt_data = break_to_lines( i_text = i_data i_begin_comment = mv_begin_comment ).

    " Read and process header line
    parse_head_line(
      exporting
        i_rename_map   = i_rename_map
        i_strict       = abap_false
      changing
        ct_data        = lt_data
        ct_head_fields = e_head_fields
        ct_map         = lt_map ).

    " Create dynamic structure
    data lt_components type abap_component_tab.
    data ls_comp like line of lt_components.
    data ld_struc type ref to cl_abap_structdescr.
    data ld_table type ref to cl_abap_tabledescr.

    ls_comp-type ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
    loop at e_head_fields assigning <f>.
      ls_comp-name = <f>.
      append ls_comp to lt_components.
    endloop.

    ld_struc = cl_abap_structdescr=>create( lt_components ).
    ld_table = cl_abap_tabledescr=>create( ld_struc ).
    create data e_container type handle ld_table.

    " parse remaining data into the structure
    field-symbols <tab> type any.
    assign e_container->* to <tab>.
    mo_struc_descr = ld_struc. "TODO: hack, maybe improve
    mt_components = zcl_text2tab_utils=>describe_struct( mo_struc_descr ).
    parse_data(
      exporting
        it_data = lt_data
        it_map  = lt_map
      importing
        e_container = <tab> ).

  ENDMETHOD.


method RAISE_ERROR.

  data: sys_call    type sys_calls,
        sys_stack   type sys_callst,
        l_location  type string,
        l_struc     type string.

  call function 'SYSTEM_CALLSTACK' " Get stack information
    exporting
      max_level    = 2
    importing
      et_callstack = sys_stack.

  read table sys_stack into sys_call index 2.

  if mo_struc_descr is bound.
    l_struc = mo_struc_descr->get_relative_name( ).
    if l_struc is not initial. " Format location
      l_location = l_struc.
      if mv_current_field is not initial.
        l_location = |{ l_location }-{ mv_current_field }|.
      endif.
      if mv_line_index is not initial.
        l_location = |{ l_location }@{ mv_line_index }|.
      endif.
    endif.
  endif.

  raise exception type zcx_text2tab_error
    exporting
      methname  = |{ sys_call-eventname }|
      msg       = msg
      code      = code
      field     = mv_current_field
      line      = mv_line_index
      structure = l_struc
      location  = l_location.

endmethod.  "raise_error
ENDCLASS.
