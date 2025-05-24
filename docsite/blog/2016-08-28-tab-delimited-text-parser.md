---
slug: tab-delimited-text-parser
title: Abap tab-delimited text parser
authors: sbcgua
tags: [old]
---

I'd like to share a piece of code which might be useful for someone. It is called text2tab (at the time of initial publishing - Abap data parser). Its purpose is parsing of TAB-delimited text into an arbitrary flat structure or internal table. Why TAB-delimited? This is the format which is used automatically if you copy (clipboard) something from Excel - this creates some opportunities for good program usability.

<!-- truncate -->

## Example

So what does it do. Let's say we have this data in a form of string (`CRLF` as a line delimiter, `TAB` as a field delimiter):

```text
NAME     BIRTHDATE
ALEX     01.01.1990
JOHN     02.02.1995
LARA     03.03.2000
```

... and a corresponding data type and internal table.

```abap
types: 
  begin of my_table_type,
    name      type char10,
    birthdate type datum,
  end of my_table_type.

data lt_container type my_table_type.
```

To parse the string into the container table just add the following code:

```abap
lcl_data_parser=>create( lt_container )->parse(
  exporting i_data      = lv_some_string_with_data
  importing e_container = lt_container ).
```

## Unstrict mode

The class supports some additional features, in particular, **unstrict mode** which allow to skip field of the target structure in text - useful when you need to load just several certain fields of a huge data structure (like standard tables in SAP). Let's consider our data type has additional field, unnecessary in the current context:

```abap
types:
  begin of my_table_type,
    name      type char10,
    // highlight-next-line
    city      type char40,   " << New field, but still just 2 in the text
    birthdate type datum,
  end of my_table_type.
...
lcl_data_parser=>create(
    i_pattern       = lt_container       
    i_amount_format = ' .'         " specify thousand and decimal delimiters
  )->parse(
    exporting
      i_data      = lv_some_string_with_data
      i_strict    = abap_false     " missing city field will not throw an error
      i_has_head  = abap_true      " headers in the first line of the text
    importing
      e_container = lt_container ).
```

Another feature: `i_has_head` parameter above means that the first line contains tech names of the fields - then the parser uses it to identify existing fields and their order (which may be flexible then).

## Use-cases

- We (our company) use the code for some of our company's products - like [FI Mass posting tool for SAP](https://vimeo.com/180405740)
- We use it in the [mockup loader](https://github.com/sbcgua/mockup_loader) - another our openly published tool for unit testing (actually the text2tab was a part of mockup loader initially)
- As a tool for mass uploads for custom tables of some other our products (selective backups)

The code is free to use under MIT licence and ca be found in this [github repository](https://github.com/sbcgua/text2tab).

Installation can be done with [abapGit](https://github.com/abapGit/abapGit) tool.

I hope you find this useful!

P.S. *Originally posted at [SAP Community platform](https://community.sap.com/t5/application-development-and-automation-blog-posts/abap-data-parser-open-source-tab-delimited-text-parser/ba-p/13210322) on 2016-Aug-28.*
