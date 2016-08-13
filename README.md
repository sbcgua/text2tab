# Abap data parser

TAB-delimited text parser for ABAP

## Synopsis

Abap data parser is an utility to parse TAB-delimited text into an internal table of an arbitrary flat structure. It support "unstrict" mode which allows to skip fields in the source data (for the case when only certain fields are being loaded). It supports "header" specification as the first line in the text - in this case field order in the text may differ from the internal abap structure field order. It also supports loading into a structure (the first data line of the text is parsed). 

You can install the whole code using [abapGit](https://github.com/larshp/abapGit) tool. To utilize data_parser please include `zdata_parser_class` and `zdata_parser_tests` to your program. 

Alternatively, you can also copy content of `zdata_parser_class.abap` to your program (please keep the license text). If you don't want to move unit tests just remove the `friends` reference in the class definition.

The tool is open source and distributed under MIT license. It was initially created as a part of another project - [mockup loader](https://github.com/sbcgua/mockup_loader) - but then separated as an independent multiusage tool.

## Example of usage

Source text file (CRLF as a line delimiter, TAB as a field delimiter)
```
FIELD1   FIELD2
ALEX     01.01.1990
JOHN     02.02.1995
LARA     03.03.2000
```
Simple parsing code
```abap

types: begin of my_table_type,
         field1 type char10,
         field2 type datum,
       end of my_table_type.

data lt_container type my_table_type.

lcl_data_parser=>create( lt_container )->parse(
  exporting i_data      = my_get_some_raw_text_data( )
  importing e_container = lt_container ).
```

... or a more complex one ...

```abap
lcl_data_parser=>create(
    i_pattern       = lt_container          " table or structure
    i_amount_format = ' .'                  " specify thousand and decimal delimiters
  )->parse( 
    exporting 
      i_data      = my_get_some_raw_text_data( )
      i_strict    = abap_false            " text may contain not all fields
      i_has_head  = abap_true             " headers in the first line of the text
    importing 
      e_container = lt_container ).       " table or structure (first data line from text)
```
Of course, you can keep the object reference returned by `create()` and use it to parse more data of the same pattern.
