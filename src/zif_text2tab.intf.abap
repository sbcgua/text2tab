interface zif_text2tab
  public.

  types ty_begin_comment type c length 1.

  constants c_auto_detect_by_space type ty_begin_comment value cl_abap_char_utilities=>maxchar.

  constants version type string value 'v2.3.4'. "#EC NOTEXT
  constants origin  type string value 'https://github.com/sbcgua/text2tab'. "#EC NOTEXT
  constants license type string value 'MIT'. "#EC NOTEXT

endinterface.
