class ZCL_TEXT2TAB_SERIALIZER definition
  public
  final
  create public .

public section.

  type-pools ABAP .
  class CL_ABAP_CHAR_UTILITIES definition load .
  constants HOMEPAGE type STRING value 'https://github.com/sbcgua/abap_data_parser'. "#EC NOTEXT
  constants C_CRLF like CL_ABAP_CHAR_UTILITIES=>CR_LF value CL_ABAP_CHAR_UTILITIES=>CR_LF. "#EC NOTEXT
  constants C_LF like CL_ABAP_CHAR_UTILITIES=>NEWLINE value CL_ABAP_CHAR_UTILITIES=>NEWLINE. "#EC NOTEXT
  constants C_TAB like CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB value CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB. "#EC NOTEXT

  methods SERIALIZE
    importing
      !I_DATA type ANY
    returning
      value(R_STRING) type STRING
    raising
      ZCX_TEXT2TAB_ERROR .
  class-methods CREATE
    importing
      !I_DECIMAL_SEP type CHAR1 optional
      !I_DATE_FORMAT type CHAR4 optional
      !I_MAX_FRAC_DIGITS type i optional
      !I_USE_LF type ABAP_BOOL default ABAP_FALSE
    returning
      value(RO_SERIALIZER) type ref to ZCL_TEXT2TAB_SERIALIZER
    raising
      ZCX_TEXT2TAB_ERROR .
protected section.
private section.

  type-pools ABAP .
  class CL_ABAP_CHAR_UTILITIES definition load .
  data MV_DECIMAL_SEP type CHAR1 .
  data MV_DATE_FORMAT type CHAR4 .
  data MV_LINE_SEP type string .
  data MV_MAX_FRAC_DIGITS type i.

  methods serialize_field
    importing
      is_component type abap_compdescr
      i_value      type any
    returning
      value(r_out) type string
    raising
      zcx_text2tab_error .

  class-methods apply_conv_exit
    importing
      !i_in type any
      !i_convexit type string
    returning value(r_out) type string.

  class-methods serialize_date
    importing
      !i_date type datum
      !iv_date_format type char4
    returning value(r_out) type string.

  class-methods validate_components
    importing
      id_struc type ref to cl_abap_structdescr
    raising
      zcx_text2tab_error .

  class-methods serialize_header
    importing
      id_struc type ref to cl_abap_structdescr
    changing
      ct_lines type string_table.

  methods serialize_data
    importing
      id_struc type ref to cl_abap_structdescr
      i_data type any table
    changing
      ct_lines type string_table
    raising
      zcx_text2tab_error .

ENDCLASS.



CLASS ZCL_TEXT2TAB_SERIALIZER IMPLEMENTATION.


method apply_conv_exit.

  data l_fm_name type rs38l_fnam value 'CONVERSION_EXIT_XXXXX_OUTPUT'.
  replace first occurrence of 'XXXXX' in l_fm_name with i_convexit.

  call function 'FUNCTION_EXISTS'
    exporting
      funcname           = l_fm_name
    exceptions
      function_not_exist = 1
      others             = 2.

  if sy-subrc <> 0.
    return.
  endif.

  data l_tmp type char40. " Potential bug, but string is padded at the end
  call function l_fm_name
    exporting
      input  = i_in
    importing
      output = l_tmp
    exceptions
      others = 1.

  if sy-subrc <> 0.
    return.
  endif.

  r_out = l_tmp.

endmethod.  "apply_conv_exit


method create.
  create object ro_serializer.

  if i_use_lf = abap_true.
    ro_serializer->mv_line_sep = c_lf.
  else.
    ro_serializer->mv_line_sep = c_crlf.
  endif.

  " Defaults
  ro_serializer->mv_decimal_sep     = '.'.
  ro_serializer->mv_date_format     = 'DMY.'.
  ro_serializer->mv_max_frac_digits = 5.

  " Not empty param and not empty decimal separator
  if i_decimal_sep ca '.,'.
    ro_serializer->mv_decimal_sep = i_decimal_sep.
  endif.

  if i_max_frac_digits is supplied and i_max_frac_digits between 0 and 22. " 22 ? constant from FLTP_CHAR_CONVERSION
    ro_serializer->mv_max_frac_digits = i_max_frac_digits.
  endif.

  " Not empty param and not empty decimal separator
  if i_date_format is not initial.
    zcl_text2tab_utils=>validate_date_format_spec( i_date_format ).
    ro_serializer->mv_date_format = i_date_format.
  endif.
endmethod.


method serialize.

  " Detect types
  data ld_type type ref to cl_abap_typedescr.
  ld_type = cl_abap_typedescr=>describe_by_data( i_data ).
  if not ld_type->kind ca 'ST'. " Structure or table
    zcx_text2tab_error=>raise(
      msg  = 'i_data must be a Structure or a table'
      code = 'ST' ). "#EC NOTEXT
  endif.

  data ld_struc type ref to cl_abap_structdescr.
  data ld_table type ref to cl_abap_tabledescr.
  data lr_datatab type ref to data.
  field-symbols <data> type any table.

  if ld_type->kind = cl_abap_typedescr=>kind_struct.
    ld_struc ?= ld_type.
    ld_table = cl_abap_tabledescr=>create( ld_struc ).
    create data lr_datatab type handle ld_table.
    assign lr_datatab->* to <data>.
    insert i_data into table <data>.
  else.
    ld_table ?= ld_type.
    ld_struc ?= ld_table->get_table_line_type( ).
    assign i_data to <data>.
  endif.

  validate_components( ld_struc ).

  " serialize header / collect in string table
  data lt_lines  type string_table.
  serialize_header(
    exporting
      id_struc = ld_struc
    changing
      ct_lines = lt_lines ).

  " serialize data
  serialize_data(
    exporting
      id_struc = ld_struc
      i_data   = <data>
    changing
      ct_lines = lt_lines ).

  r_string = concat_lines_of( table = lt_lines sep = mv_line_sep ).

endmethod.


method serialize_data.
  data lt_fields type string_table.
  data lv_buf type string.

  field-symbols <c>      like line of id_struc->components.
  field-symbols <record> type any.
  field-symbols <field>  type any.

  loop at i_data assigning <record>.
    clear lt_fields.
    loop at id_struc->components assigning <c>.
      assign component sy-tabix of structure <record> to <field>.
      lv_buf = serialize_field(
        is_component = <c>
        i_value      = <field> ).
      append lv_buf to lt_fields.
    endloop.
    lv_buf = concat_lines_of( table = lt_fields sep = c_tab ).
    append lv_buf to ct_lines.
  endloop.

endmethod.


method serialize_date.
  data:
        l_iter    type i,
        l_part    type c,
        l_sep     type c.

  if i_date is initial. " Empty date -> empty string
    return.
  endif.
  l_sep = iv_date_format+3(1).

  do 3 times.
    l_iter = sy-index - 1.
    l_part = iv_date_format+l_iter(1).

    case l_part.
      when 'D'.
        r_out = r_out && i_date+6(2).
      when 'M'.
        r_out = r_out && i_date+4(2).
      when 'Y'.
        r_out = r_out && i_date+0(4).
      when others.
        " assuming date format was validated ...
    endcase.

    if l_sep is not initial and l_iter < 2.
      r_out = r_out && l_sep.
    endif.
  enddo.
endmethod.


method serialize_field.
  data:
        l_tmp type char40,
        l_mask type string.

  case is_component-type_kind.

    when cl_abap_typedescr=>typekind_char. " Char + Alpha
      describe field i_value edit mask l_mask.
      if l_mask is initial.
        r_out = i_value.
      else.
        shift l_mask left deleting leading '='.
        r_out = apply_conv_exit(
          i_in = i_value
          i_convexit = l_mask ).
        if sy-subrc is not initial.
          zcx_text2tab_error=>raise( msg = 'convexit failed' code = 'CF' ). "#EC NOTEXT
        endif.
      endif.

    when cl_abap_typedescr=>typekind_date. " Date
      r_out = serialize_date(
        i_date         = i_value
        iv_date_format = mv_date_format ).

    when cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1 OR cl_abap_typedescr=>typekind_int2 . " Integer
      l_tmp = i_value.
      shift l_tmp left deleting leading space. " In case it is the last one
      r_out = l_tmp.

    when cl_abap_typedescr=>typekind_packed. " Amount
      l_tmp = i_value.
      shift l_tmp left deleting leading space. " strange space at the end if move to string
      if mv_decimal_sep <> '.'.
        replace first occurrence of '.' in l_tmp with mv_decimal_sep.
      endif.
      r_out = l_tmp.

    when cl_abap_typedescr=>typekind_float. " Float
      " Hmmmm ... improve ?
      write i_value to l_tmp right-justified exponent 0 decimals mv_max_frac_digits.
      shift l_tmp right deleting trailing '0'.
      replace first occurrence of ',' in l_tmp with '.'. " Can be ',' depending on user fmt ?
      shift l_tmp right deleting trailing '.'. " In case it is the last one
      shift l_tmp left deleting leading space. " In case it is the last one
      if mv_decimal_sep <> '.'.
        replace first occurrence of '.' in l_tmp with mv_decimal_sep.
      endif.
      r_out = l_tmp.

    when others.
      r_out = i_value.

  endcase.

endmethod.


method serialize_header.
  data lt_fields type string_table.
  field-symbols <c> like line of id_struc->components.

  loop at id_struc->components assigning <c>.
    append <c>-name to lt_fields.
  endloop.

  data lv_buf type string.
  lv_buf = concat_lines_of( table = lt_fields sep = zcl_text2tab_serializer=>c_tab ).
  append lv_buf to ct_lines.

endmethod.


method validate_components.
  " check if all components are elementary
  field-symbols <c> like line of id_struc->components.
  loop at id_struc->components assigning <c>.
    if id_struc->get_component_type( <c>-name )->kind <> cl_abap_typedescr=>kind_elem.
      zcx_text2tab_error=>raise(
        msg  = 'i_data line should contain only fields of elementary types'
        code = 'ET' ). "#EC NOTEXT
    endif.
  endloop.

endmethod.
ENDCLASS.
