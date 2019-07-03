interface ZIF_TEXT2TAB_DEEP_PROVIDER
  public .

    methods select
      importing
        i_address type string
        i_cursor  type any
      exporting
        e_container type any.

endinterface.
