---
sidebar_position: 1
---

# Introduction

Text2tab is an utility to parse TAB-delimited text into an internal table of an arbitrary flat structure.

## Features

- supports *non-strict* mode which allows to skip fields in the source data (for the case when only certain fields are being loaded).
- supports *corresponding* parsing - filling only those fields which are in target structure. Kind of opposite to *non-strict* feature above.
- supports *header* specification as the first line in the text - in this case field order in the text may differ from the internal abap structure field order.
- supports loading into a structure (the first data line of the text is parsed).
- supports *type-less* parsing, when the data is not checked against existing structure but dynamically create as a table with string fields.
- supports specifying date and amount formats
- supports on-the-fly field name remapping (e.g. field `FOO` in the parsed text move to `BAR` component of the target internal table)
- supports *deep* parsing - filling structure or table components in the target data container

And vice versa - **serialize** a flat table or structure to text.

- supports specifying date and amount formats, and line-break symbol

The package also contains 2 **examples**:

- `ZTEXT2TAB_EXAMPLE` - simple parsing code
- `ZTEXT2TAB_BACKUP_EXAMPLE` - example of DB table backup with serializer

## Example of usage

Source text file (CRLF as a line delimiter, TAB as a field delimiter)

```text
NAME     BIRTHDATE
Alex     01.01.1990
John     02.02.1995
Lara     03.03.2000
```

Simple parsing code

```abap
types:
  begin of my_table_type,
    name      type char10,
    birthdate type datum,
  end of my_table_type.

data lt_container type my_table_type.

zcl_text2tab_parser=>create( lt_container )->parse(
  exporting
    i_data      = get_raw_text_data_above( )
  importing
    e_container = lt_container ).
```

... or a more complex one ...

```abap
types:
  begin of my_table_type,
    name      type char10,
    city      type char40, " <<< New field, but the text still contains just 2 !
    birthdate type datum,
  end of my_table_type.

...

zcl_text2tab_parser=>create(
    i_pattern       = lt_container   " table or structure
    i_amount_format = ' .'           " specify thousand and decimal delimiters
  )->parse( 
    exporting 
      i_data      = get_some_raw_text_data( )
      i_strict    = abap_false       " text may contain not all fields ("city" field will be skipped)
      i_has_head  = abap_true        " headers in the first line of the text
    importing 
      e_container = lt_container ).  " table or structure (first data line from text)
```

You can keep the object reference returned by `create()` and use it to parse more data of the same pattern.
