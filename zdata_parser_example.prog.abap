*&---------------------------------------------------------------------*
*& ABAP DATA PARSER
*&   project homepage: https://github.com/sbcgua/abap_data_parser
*&---------------------------------------------------------------------*

report zdata_parser_example.

start-of-selection.

types:
  begin of ty_my,
    num  type i,
    word type char8,
  end of ty_my,
  tt_my type standard table of ty_my.

data lt_container type tt_my.
data lv_text type string.

lv_text = 'NUM\tWORD\n1\tHello\n2\tWorld'.
replace all occurrences of '\t' in lv_text with cl_abap_char_utilities=>horizontal_tab.
replace all occurrences of '\n' in lv_text with cl_abap_char_utilities=>cr_lf.

zcl_data_parser=>create(
  i_pattern       = lt_container
  i_amount_format = ''
)->parse(
  exporting
    i_data      = lv_text
    i_strict    = abap_true
    i_has_head  = abap_true
  importing
    e_container = lt_container ).

field-symbols <i> like line of lt_container.
loop at lt_container assigning <i>.
  write: / <i>-num, <i>-word.
endloop.
