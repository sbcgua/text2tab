---
sidebar_position: 10
---

# Serialization

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

## Serialize selected fields

The `serialize` method also accepts `i_fields_only` param - a explicit field list to serialize. You can also pass `i_keep_order = abap_true` to keep serilization order according to the fields only list (by default the order is takes from the data structure).

## Getting header row only

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

## Serialize as HTML

The feature allows serialization to html table `<table><tr>...`. In particular this form is natively supported when pasted to Excel sheet. And in particular it is possible to mark fields as text, so that no auto conversion happen to the values. E.g. "1.1" is auto converted to a date which is not intended.

```abap
  lv_string = lo_serializer->as_html( i_text_fields = value#( ( 'CODE' ) ) )->serialize( lt_some_table ).
```

*To see the impact of modes, check `ZTEXT2TAB_BACKUP_EXAMPLE` - the selection screen allows control over the html/text modes. As well it allows copying the result to the clipboard for pasting to Excel after.*
