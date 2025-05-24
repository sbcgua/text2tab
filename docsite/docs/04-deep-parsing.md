---
sidebar_position: 3
---

# Deep parsing

If you have a target data with deep fields - tables or structures - it is possible to fill them in one run. For this you have to create a data source provider implementing `zif_text2tab_deep_provider` and pass it to the parser. An example of implementation can be found in [mockup_loader](https://github.com/sbcgua/mockup_loader). But let's consider an simple example here.

Let's assume you have 2 linked tables - header and lines

```text
DOCUMENT
========
ID   DATE   ...
1    ...
2    ...

LINES
========
DOCID   LINEID   AMOUNT   ...
1       1        100.00   ...
1       2        123.00   ...
2       1        990.00   ...
```

Let's assume you have a target data of the following structure

```abap
types:
  begin of ty_line,
    docid  type numc10,
    lineid type numc3,
    " ...
  end of ty_line,
  tt_line type table of ty_line,
  begin of ty_document,
    id   type numc10,
    " ...
    lines type tt_line, " <<< DEEP FIELD, supposed to be filled with lines of the document
  end of ty_document,
  tt_documents type table of ty_document.

```

So you can run the parser as follows to parse all at once

```abap
zcl_text2tab_parser=>create( 
  i_pattern = lt_container 
  i_deep_provider = lo_implementation_of_zif_text2tab_deep_provider
)->parse(
  exporting
    i_data          = my_raw_text_data
  importing
    e_container = lt_container ).
```

So what is `lo_implementation_of_zif_text2tab_deep_provider` in this example? The parser does not know how to get additional data other than `my_raw_text_data`. The implementation of `zif_text2tab_deep_provider` should. Maybe you want to get it from a file, which is located somewhere near, or download from web, or just select from a DB table - this is up to your design and need. For example, the mentioned [mockup_loader](https://github.com/sbcgua/mockup_loader) uses the text value in place of "deep" field in order to find the data and parse it. The source file should contain a reference in place of "deep" field to help finding the source data. E.g.

```text
DOCUMENT
========
ID   DATE   ...   LINES
1    ...          lines.txt[docid=@id]
2    ...          lines.txt[docid=@id]
```

where mockup_loader interprets `lines.txt[docid=@id]` as *"go find lines.txt file, parse it, extract the lines, filter those where `docid` = `id` value of the current header record"*.

`zif_text2tab_deep_provider` requires just one method to implement - `select`.

```abap
methods select
  importing
    i_address type string " e.g. filename[key_field_name=123]
    i_cursor  type any    " reference to currently processed data line to fetch field values
  exporting
    e_container type any.
```

- `i_address` is the value of "deep" field in text data (in our example value of `LINES` field - `lines.txt[docid=@id]`)
- `i_cursor` is the current record with parsed field values (in our example: id, date, ...)
- `e_container` is the target component of the current record (in our example - `LINES` component of `ty_document`)

So the implementation of the interface must parse the address and get the "deep" data. You are not limited to the specific address format, however, if you want to follow the format described above (`<sourceref>[<sourcekeyfield>=<cursorkeyfield>]`), you can reuse `parse_deep_address` and `get_struc_field_value_by_name` methods implemented in `zcl_text2tab_utils`.
