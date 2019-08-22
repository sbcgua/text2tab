interface ZIF_TEXT2TAB_DEEP_PROVIDER
  public .

  methods select
    importing
      i_address type string " e.g. filename[key_field_name=123]
      i_cursor  type any    " reference to currently processed data line to fetch field values
    exporting
      e_container type any
    raising
      zcx_text2tab_error.

endinterface.
