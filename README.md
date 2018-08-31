# Abap data parser

TAB-delimited text parser for ABAP  
v2.0.0 ([changelog](./changelog.txt))

## Synopsis

Abap data parser is an utility to parse TAB-delimited text into an internal table of an arbitrary flat structure. It support "unstrict" mode which allows to skip fields in the source data (for the case when only certain fields are being loaded). It supports "header" specification as the first line in the text - in this case field order in the text may differ from the internal abap structure field order. It also supports loading into a structure (the first data line of the text is parsed). 

## Installation

You can install the whole code using [abapGit](https://github.com/larshp/abapGit) tool.

Alternatively, you can also copy content of `zcl_data_parser.clas.abap` to your program (please keep the homepage and license text).

The tool is open source and distributed under MIT license. It was initially created as a part of another project - [mockup loader](https://github.com/sbcgua/mockup_loader) - but then separated as an independent multiusage tool.

## Example of usage

Source text file (CRLF as a line delimiter, TAB as a field delimiter)
```
NAME     BIRTHDATE
ALEX     01.01.1990
JOHN     02.02.1995
LARA     03.03.2000
```
Simple parsing code
```abap
types: begin of my_table_type,
         name      type char10,
         birthdate type datum,
       end of my_table_type.

data lt_container type my_table_type.

zcl_data_parser=>create( lt_container )->parse(
  exporting i_data      = my_get_some_raw_text_data( )
  importing e_container = lt_container ).
```

... or a more complex one ...

```abap
types: begin of my_table_type,
         name      type char10,
         city      type char40,             " <<< New field, but the text still contains just 2 !
         birthdate type datum,
       end of my_table_type.

...

zcl_data_parser=>create(
    i_pattern       = lt_container          " table or structure
    i_amount_format = ' .'                  " specify thousand and decimal delimiters
  )->parse( 
    exporting 
      i_data      = my_get_some_raw_text_data( )
      i_strict    = abap_false            " text may contain not all fields (city field will be skipped)
      i_has_head  = abap_true             " headers in the first line of the text
    importing 
      e_container = lt_container ).       " table or structure (first data line from text)
```
Of course, you can keep the object reference returned by `create()` and use it to parse more data of the same pattern.

## Typeless parsing

You can also create an instance that does not validate type against some existing type structure. Instead it generates the table dynamically, where each field if the line is unconverted string.

```abap
data:
  lr_data   type ref to data,
  lt_fields type string_table.

zcl_data_parser=>create_typeless( )->parse( 
    exporting 
      i_data      = my_get_some_raw_text_data( )
    importing 
      e_head_fields = lt_fields  " Contain the list of field names !
      e_container   = lr_data ). " The container is created inside the parser
```

## Error message redefinition

The exception class - `zcx_data_parser_error` - exposes `struc`, `field`, `line` and `msg` attributes (and some others). They can be used to reformat the message text if needed. For example:

```abap
  ...
  catch zcx_data_parser_error into lx. " Reformat to -> Import error at line LINE, field 'FIELD': MSG
    
    l_error_msg = 'Import error'.
    if lx->line is not initial.
      l_error_msg = |{ l_error_msg } at line { lx->line }|.
    endif.
    if lx->field is not initial.
      l_error_msg = |{ l_error_msg }, field '{ lx->field }'|.
    endif.
    l_error_msg = |{ l_error_msg }: { lx->msg }|.
    
    raise exception type lcx_my_program_error
      exporting msg = l_error_msg.
  endtry.
```
