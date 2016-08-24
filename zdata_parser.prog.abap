*&---------------------------------------------------------------------*
*& ABAP DATA PARSER
*&   project homepage: https://github.com/sbcgua/abap_data_parser
*&---------------------------------------------------------------------*

report zdata_parser.

class lcl_test_data_parser definition deferred.

include zdata_parser_class.
include zdata_parser_tests.

start-of-selection.

* Help banner

write: / 'Abap data parser is an utility to parse TAB-delimited text into an internal table of an arbitrary'.
write: / 'flat structure. It support "unstrict" mode which allows to skip fields in the source data (for the'.
write: / 'case when only certain fields are being loaded). It supports "header" raw as the first line in the'.
write: / 'text - in this case field order in the text may differ from the internal abap structure field order.'.
write: / 'It also supports loading into a structure (the first data line of the text is parsed).'.
write: /.
write: / 'The tool is open source and distributed under MIT license.'.
write: / 'The home page is https://github.com/sbcgua/abap_data_parser.'.
write: /.
write: / 'To utilize data_parser please include zdata_parser_class and zdata_parser_tests to your program.'.
write: / 'Alternatively, you may also copy content of zdata_parser_class to your program and skip tests'.
write: / '(please keep the license text, however). '.
write: /.
write: /(100) 'An example of usage:' color = 4.
write: /.
write: / '  data lt_container type my_table_type.'.
write: /.
write: / '  lcl_data_parser=>create( lt_container )->parse('.
write: / '    exporting i_data      = my_get_some_raw_text_data( )'.
write: / '    importing e_container = lt_container ).'.
write: /.
write: /(100) '... or a more complex one ...' color = 4.
write: /.
write: / '  lcl_data_parser=>create('.
write: / '      i_pattern       = lt_container',               at 52 '" table or structure'                      color = 2 inverse.
write: / '      i_amount_format = '' .''',                     at 52 '" specify thousand and decimal delimiters' color = 2 inverse.
write: / '    )->parse( '.
write: / '      exporting'.
write: / '        i_data      = my_get_some_raw_text_data( )', at 52 '" string of CRLF separated rows of TAB separated fields' color = 2 inverse.
write: / '        i_strict    = abap_false',                   at 52 '" text may contain not all of fields defined in struct'  color = 2 inverse.
write: / '        i_has_head  = abap_true',                    at 52 '" headers in the first line of the text'                 color = 2 inverse.
write: / '      importing'.
write: / '        e_container = lt_container ).',              at 52 '" table or structure (first data line from text)'        color = 2 inverse.