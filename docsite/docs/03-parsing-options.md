---
sidebar_position: 3
---

# Parsing options and features

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

Renames can be also passed as a string in this format: `'field_to_rename:new_name, another_field:another_new_name'`. `;` and `,` are supported as separators. Coding convenience is important ;)

```abap
zcl_text2tab_parser=>create( lt_container )->parse(
  exporting
    i_rename_fields = 'Full_name:name'
    i_data          = my_get_some_raw_text_data( )
  importing
    e_container = lt_container ).

```

## Ignore non-flat fields

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
zcl_text2tab_parser=>create(
  i_pattern = ls_birthday
  i_begin_comment = '*' ).
```

The char '*' must have the first position in the text line.  Otherwise it isn't interpreted as a comment.

The comment (or rather the description) line can also be auto-detected with `i_begin_comment = zif_text2tab=>c_auto_detect_by_space`. It tries to find spaces in the first line of the text and, if found, ignores this first line. The idea is that the descriptions will probably have spaces, while abap field names will not:

```text
Name    Date of birth <<< containes spaces
NAME    BIRTHDAY
JOHN    01.01.1990
```

### Within-data comments

Also the data can contain commented lines. They can be skipped if started with a char passed with `i_skip_lines_starting_with` param. This can be useful for human-friendly data grouping.

```abap
zcl_text2tab_parser=>create(
  i_pattern = ls_birthday
  i_skip_lines_starting_with = '#' ).
```

```text
NAME    BIRTHDAY
# Managers <<< will be ignored
John    01.01.1990
# Employees <<< will be ignored
Anna    01.01.1990
```

