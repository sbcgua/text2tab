class ZCL_TEXT2TAB_SERIALIZER definition
  public
  final
  create public .

public section.

  type-pools ABAP .
  class CL_ABAP_CHAR_UTILITIES definition load .
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

  data MV_DECIMAL_SEP type CHAR1 .
  data MV_DATE_FORMAT type CHAR4 .
  data MV_LINE_SEP type STRING .
  data MV_MAX_FRAC_DIGITS type I .
  data MV_CURRENT_FIELD type STRING .
  data MV_LINE_INDEX type SY-TABIX .

  class ZCL_TEXT2TAB_UTILS definition load .
  methods SERIALIZE_FIELD
    importing
      !IS_COMPONENT type ZCL_TEXT2TAB_UTILS=>TY_COMP_DESCR
      !I_VALUE type ANY
    returning
      value(R_OUT) type STRING
    raising
      ZCX_TEXT2TAB_ERROR .
  class-methods APPLY_CONV_EXIT
    importing
      !I_IN type ANY
      !I_CONVEXIT type ABAP_EDITMASK
    returning
      value(R_OUT) type STRING
    exceptions
      CONV_FAILED .
  class-methods SERIALIZE_DATE
    importing
      !I_DATE type DATUM
      !IV_DATE_FORMAT type CHAR4
    returning
      value(R_OUT) type STRING .
  class-methods VALIDATE_COMPONENTS
    importing
      !ID_STRUC type ref to CL_ABAP_STRUCTDESCR
    raising
      ZCX_TEXT2TAB_ERROR .
  class-methods SERIALIZE_HEADER
    importing
      !ID_STRUC type ref to CL_ABAP_STRUCTDESCR
    changing
      !CT_LINES type STRING_TABLE .
  methods SERIALIZE_DATA
    importing
      !ID_STRUC type ref to CL_ABAP_STRUCTDESCR
      !I_DATA type ANY TABLE
    changing
      !CT_LINES type STRING_TABLE
    raising
      ZCX_TEXT2TAB_ERROR .
ENDCLASS.



CLASS ZCL_TEXT2TAB_SERIALIZER IMPLEMENTATION.


method apply_conv_exit.

  data l_fm_name type rs38l_fnam value 'CONVERSION_EXIT_XXXXX_OUTPUT'.

  replace first occurrence of 'XXXXX' in l_fm_name with i_convexit.
  if zcl_text2tab_utils=>function_exists( l_fm_name ) = abap_false.
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
    raise conv_failed.
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

  field-symbols <record> type any.
  field-symbols <field>  type any.

  data lt_components type zcl_text2tab_utils=>tt_comp_descr.
  field-symbols <c> like line of lt_components.
  lt_components = zcl_text2tab_utils=>describe_struct( id_struc ).

  " Serialization loop
  loop at i_data assigning <record>.
    mv_line_index = sy-tabix.
    clear lt_fields.
    loop at lt_components assigning <c>.
      assign component sy-tabix of structure <record> to <field>.
      mv_current_field = <c>-name.
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

  if i_date is initial or i_date = ''. " 00.. is initial, '' is not !
    return. " Empty date -> empty string
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
        l_tmp type char40.

  case is_component-type_kind.

    when cl_abap_typedescr=>typekind_char. " Char + Alpha
      if is_component-edit_mask is initial.
        r_out = i_value.
      else.
        apply_conv_exit(
          exporting
            i_in       = i_value
            i_convexit = is_component-edit_mask
          receiving
            r_out = r_out
          exceptions others = 1 ).
        if sy-subrc is not initial.
          zcx_text2tab_error=>raise(
            msg      = |convexit failed for "{ i_value }"|
            location = |{ mv_current_field }@{ mv_line_index }|
            code     = 'CF' ). "#EC NOTEXT
        endif.
      endif.

    when cl_abap_typedescr=>typekind_date. " Date
      r_out = serialize_date(
        i_date         = i_value
        iv_date_format = mv_date_format ).

    when cl_abap_typedescr=>typekind_int
      or cl_abap_typedescr=>typekind_int1
      or cl_abap_typedescr=>typekind_int2. " Integer
      l_tmp = i_value.
      shift l_tmp left deleting leading space. " In case it is the last one
      r_out = l_tmp.

    when cl_abap_typedescr=>typekind_packed. " Amount
      l_tmp = i_value.
      shift l_tmp left deleting leading space. " strange space at the end if move to string
      if mv_decimal_sep <> '.'.
        replace first occurrence of '.' in l_tmp with mv_decimal_sep.
      endif.
      replace first occurrence of '-' in l_tmp with ''.
      if sy-subrc = 0.
        concatenate '-' l_tmp into l_tmp.
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
