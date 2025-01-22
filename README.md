# ABAP text2tab parser and serializer (ex. Abap data parser)

![abaplint](https://github.com/sbcgua/text2tab/workflows/abaplint/badge.svg)
![Version](https://img.shields.io/github/v/tag/sbcgua/text2tab.svg)

TAB-delimited text parser and serializer for ABAP  
*[changelog](./changelog.txt)*

## Synopsis

Text2tab is an utility to parse TAB-delimited text into an internal table of an arbitrary flat structure.

- supports "non-strict" mode which allows to skip fields in the source data (for the case when only certain fields are being loaded).
- supports "header" specification as the first line in the text - in this case field order in the text may differ from the internal abap structure field order.
- supports loading into a structure (the first data line of the text is parsed).
- supports *type-less* parsing, when the data is not checked against existing structure but dynamically create as a table with string fields.
- supports specifying date and amount formats
- supports on-the-fly field name remapping (e.g. field `FOO` in the parsed text move to `BAR` component of the target internal table)
- supports "deep" parsing - filling structure or table components in the target data container
- supports "corresponding" parsing - filling only those fields which are in target structure. Kind of opposite to "non-strict" feature above.

And vice versa - serialize flat table or structure to text.

- support specifying date and amount formats, and line-break symbol

The package also contains 2 **examples**:

- `ZTEXT2TAB_EXAMPLE` - simple parsing code
- `ZTEXT2TAB_BACKUP_EXAMPLE` - example of DB table backup with serializer

## Installation

You can install the whole code using [abapGit](https://github.com/abapGit/abapGit) tool.

The tool is open source and distributed under MIT license. It was initially created as a part of another project - [mockup loader](https://github.com/sbcgua/mockup_loader) - but then separated as an independent multi-usage tool.

The package contains 2 subpackages: `core` (the tool itself) and `examples` (example programs). The latter can be skipped for the deserialization (abapGit supports filename filtering - specify `*/examples/*` in the corresponding repo setting).

## Example of usage

Source text file (CRLF as a line delimiter, TAB as a field delimiter)

```text
NAME     BIRTHDATE
ALEX     01.01.1990
JOHN     02.02.1995
LARA     03.03.2000
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
    i_data      = my_get_some_raw_text_data( )
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
      i_data      = my_get_some_raw_text_data( )
      i_strict    = abap_false       " text may contain not all fields (city field will be skipped)
      i_has_head  = abap_true        " headers in the first line of the text
    importing 
      e_container = lt_container ).  " table or structure (first data line from text)
```

Of course, you can keep the object reference returned by `create()` and use it to parse more data of the same pattern.

## Field name remapping

Change target field name on-the-fly. (Useful e.g. for data migration)

```abap
* Input text to parse:
* FULL_NAME  BIRTHDATE
* ALEX       01.01.1990
* JOHN       02.02.1995

    data lt_map type zcl_text2tab_parser=>tt_field_name_map.
    field-symbols <map> like line of lt_map.
    append initial line to lt_map assigning <map>.
    <map>-from = 'Full_name'.
    <map>-to   = 'name'.

...

zcl_text2tab_parser=>create( lt_container )->parse(
  exporting
    i_rename_fields = lt_map " <<<<<<< FIELD MAP
    i_data          = my_get_some_raw_text_data( )
  importing
    e_container = lt_container ).

* Output data:
* NAME  BIRTHDATE
* ALEX  01.01.1990
* JOHN  02.02.1995
```

Renames can be also passed as a string in this format: `'field_to_rename:new_name;another_field:another_new_name'`. Coding convenience is important ;)

```abap
zcl_text2tab_parser=>create( lt_container )->parse(
  exporting
    i_rename_fields = 'Full_name:name'
    i_data          = my_get_some_raw_text_data( )
  importing
    e_container = lt_container ).

```

## Other parsing options and features

### Ignore non-flat fields

Sometimes your structure contain technical non-flat fields which are not supposed to be in source data but present in the target structure. In example, color settings for ALV. It would be cumbersome to create a separate structure just for data. You can specify `i_ignore_nonflat = abap_true` during parser creation so that non-flat components are ignored. The parser will not throw `Structure must be flat` error. However, it will still check that ignored fields are not in the source data.

E.g. target structure is `'DATE,CHAR,COLORCOL'`, where `colorcol` is a structure. The parser will accept and parse data like

```text
DATE        CHAR
01.01.2019  ABC
```

But will fail on

```text
DATE        CHAR       COLORCOL
01.01.2019  ABC        123
```

### Deep parsing

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

## Type-less parsing

You can also create an instance that does not validate type against some existing type structure. Instead it generates the table dynamically, where each field if the line is unconverted string.

```abap
data:
  lr_data   type ref to data,
  lt_fields type string_table.

zcl_text2tab_parser=>create_typeless( )->parse( 
  exporting 
    i_data      = my_get_some_raw_text_data( )
  importing 
    e_head_fields = lt_fields  " Contain the list of field names !
    e_container   = lr_data ). " The container is created inside the parser
```

## Corresponding parsing

It does actually what it states - move only those field which are the same, example is below.

Source text file

```text
NAME     BIRTHDATE
ALEX     01.01.1990
JOHN     02.02.1995
LARA     03.03.2000
```

```abap
types:
  begin of my_table_type,
    name      type char10, " The only corresponding name
    _other    type string, " "Birth date" is not here but "_other" is
  end of my_table_type.

data lt_container type my_table_type.

zcl_text2tab_parser=>create( lt_container )->parse(
  exporting
    i_data          = my_get_some_raw_text_data( )
    i_strict        = abap_false " corresponding is in fact non-strict, so must be false
    i_corresponding = abap_true
  importing
    e_container = lt_container ).
```

## Data formats

`create` accepts parameters that control expected data formatting:

- *i_amount_format* - a `char2` value, 1st char specifies thousand delimiter, 2nd - fractional delimiter. E.g. `' ,'` - for `1 234,56` and also `1234,56`, `',.'` would expect `1,234.56`.
- *i_date_format* - a `char4` value, where first 3 defines the order of day, month and year, and the last is the separator between them. E.g. `YMD` for `20180901`, `DMY-` for `01-09-2018`. Supported separators are `./-` and empty char.

## Serialization

To do serialization use `ZCL_TEXT2TAB_SERIALIZER` class. Flat tables and structures are supported. In case of a structure it is serialized as one-line table.

```abap
  data lo_serializer type ref to zcl_text2tab_serializer.
  lo_serializer = zcl_text2tab_serializer=>create(
    " the below params are all optional and have defaults inside
    i_decimal_sep     = ','
    i_date_format     = 'DMY-'
    i_max_frac_digits = 5         " For floats only ! not for decimals
    i_use_lf          = abap_true " to use LF as line-break (not CRLF)
  ).
  
  data lv_string type string.
  lv_string = lo_serializer->serialize( lt_some_table ).
```

### Serialize selected fields

The `serialize` method also accepts `i_fields_only` param - a explicit field list to serialize. You can also pass `i_keep_order = abap_true` to keep serilization order according to the fields only list (by default the order is takes from the data structure).

### Getting header row only

The serializer can also create a tab-delimited string with human readable field descriptions or technical names. E.g. `User Name \t Date \t Time` (c_header-descriptions) or `UNAME \t DATUM \t UZEIT` (c_header-tech_names). This may be useful to prepend the description before the technical fields. To get such a string use method `serialize_header`.

```abap
  lv_string = lo_serializer->serialize_header(
    i_header_type = lo_serializer->c_header-descriptions
    i_data = lt_some_table ).
  " OR
  lv_string = lo_serializer->serialize_header( 
    i_data = lt_some_table
    i_header_type = lo_serializer->c_header-descriptions
    i_lang = 'D'
    " i_keep_order = abap_true " to respect i_fields_only order
    i_fields_only = value#( ( 'UNAME' ) ( 'DATUM' ) ) ).
```

### Serialize as HTML

The feature allows serialization to html table `<table><tr>...`. In particular this form is natively supported when pasted to Excel sheet. And in particular it is possible to mark fields as text, so that no auto conversion happen to the values. E.g. "1.1" is auto converted to a date which is not intended.

```abap
  lv_string = lo_serializer->as_html( i_text_fields = value#( ( 'CODE' ) ) )->serialize( lt_some_table ).
```

*To see the impact of modes, check `ZTEXT2TAB_BACKUP_EXAMPLE` - the selection screen allows control over the html/text modes. As well it allows copying the result to the clipboard for pasting to Excel after.*

## Comments

Since version 2.2.0 the text file can contain comment lines. A comment lines begins with one specific char, which is supplied in the factory method ```create```.
A sample text-file:

```text
* comment line: expected birthdays in test class ...
NAME    BIRTHDAY
JOHN    01.01.1990
```

Now we should call the factory method like this and the first line is interpreted as a comment:

```abap
zcl_text2tab_parser=>create( i_pattern = ls_birthday i_begin_comment = '*' ).
```

The char '*' must have the first position in the text line.  Otherwise it isn't interpreted as a comment.

The comment (or rather the description) line can also be auto-detected with `i_begin_comment = zif_text2tab=>c_auto_detect_by_space`. It tries to find spaces in the first line of the text and, if found, ignores this first line. The idea is that the descriptions will probably have spaces, while abap field names will not:

```text
Name    Date of birth <<< containes spaces
NAME    BIRTHDAY
JOHN    01.01.1990
```

## Error message redefinition

The exception class - `zcx_text2tab_error` - exposes `structure`, `field`, `line` and `msg` attributes (and some others). They can be used to reformat the message text if needed. For example:

```abap
  ...
  catch zcx_text2tab_error into lx. " Reformat to -> Import error at line LINE, field 'FIELD': MSG
    
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

This is supported in parser only at the moment. Serializer does not produce many error on line level.

## Checking version

`zif_text2tab` has the `version` attribute. And there is a helper method `check_version_fits` to check if the text2tab package has the minimal required version. To extract the current version prefer `zcl_text2tab_parser=>version( )` method.

```abap
* Assuming version is 2.1.0

" Returns false, 2.1.2 is required
zcl_text2tab_parser=>check_version_fits( 'v2.1.2' ). 

" Returns true, 2.1.0 > 2.0.0
zcl_text2tab_parser=>check_version_fits( 'v2.0.0' ). 
```
