interface zif_text2tab
  public.

  types ty_begin_comment type c length 1.

  constants c_auto_detect_by_space type ty_begin_comment value cl_abap_char_utilities=>maxchar.

  constants version type string value 'v2.4.0'. "#EC NOTEXT
  constants origin  type string value 'https://github.com/sbcgua/text2tab'. "#EC NOTEXT
  constants license type string value 'MIT'. "#EC NOTEXT

  " Parser related
  types:
    tt_field_map type standard table of i with default key.
  types:
    ty_amount_format type c length 2.
  types:
    ty_date_format type c length 4.

  " Serializer related
  types:
    ty_decimal_sep type c length 1.

  types:
    ty_header_type type c length 1.

  constants:
    begin of c_header,
      technical_names type ty_header_type value 'T',
      descriptions    type ty_header_type value 'D',
    end of c_header.

  types:
    tt_fields_list type standard table of abap_compname with default key.
  types:
    ts_fields_list type sorted table of abap_compname with unique key table_line.

  " Utility types

  types:
    begin of ty_field_name_map,
      from type string,
      to   type abap_compname,
    end of ty_field_name_map.
  types:
    tt_field_name_map type standard table of ty_field_name_map with key from.
  types:
    th_field_name_map type hashed table of ty_field_name_map with unique key from.

  types:
    begin of ty_comp_descr.
      include type abap_compdescr.
  types:
      edit_mask type abap_editmask,
      output_length type i,
      ignore type abap_bool,
      description type string,
    end of ty_comp_descr.
  types:
    tt_comp_descr type standard table of ty_comp_descr with default key.
  types:
    begin of ty_deep_address,
      location  type string,
      key_field type abap_compname, " at source table
      ref_field type abap_compname, " at target (currently processed) table
      key_value type string,
    end of ty_deep_address.

endinterface.
