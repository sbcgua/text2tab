report ztext2tab_backup_example.

**********************************************************************
* LIBRARY
* this section is a part of abap_w3mi_poller
* url: https://github.com/sbcgua/abap_w3mi_poller
* it is used as service utils
* the actual TEXT2TAB usage example goes after the end of the section
* where class lcl_app definition starts
**********************************************************************

**********************************************************************
* ZCX_W3MIME_ERROR
**********************************************************************

class zcx_w3mime_error definition
  inheriting from cx_static_check
  final
  create public .

  public section.

    interfaces if_t100_message .

    constants:
      begin of zcx_w3mime_error,
        msgid type symsgid value 'SY',
        msgno type symsgno value '499',
        attr1 type scx_attrname value 'MSG',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of zcx_w3mime_error .
    data msg type string read-only .

    methods constructor
      importing
        !textid like if_t100_message=>t100key optional
        !previous like previous optional
        !msg type string optional .
    class-methods raise
      importing
        !msg type string
      raising
        zcx_w3mime_error .
endclass.

class ZCX_W3MIME_ERROR implementation.

  method constructor.
    super->constructor( previous = previous ).
    me->msg = msg .
    clear me->textid.
    if textid is initial.
      if_t100_message~t100key = zcx_w3mime_error .
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.


  method raise.
    raise exception type zcx_w3mime_error
      exporting
        textid = zcx_w3mime_error
        msg    = msg.
  endmethod.
endclass.

**********************************************************************
* ZCL_W3MIME_FS
**********************************************************************

class zcl_w3mime_fs definition
  final
  create public .

  public section.

    types:
      tt_files type standard table of file_info with key filename .

    class-data c_sep type char1 read-only .

    class-methods write_file
      importing
        !iv_filename type string
        !iv_size type i
      changing
        !ct_data type lvc_t_mime
      raising
        zcx_w3mime_error .
    class-methods write_file_x
      importing
        !iv_filename type string
        !iv_data type xstring
      raising
        zcx_w3mime_error .
    class-methods class_constructor .
endclass.

class ZCL_W3MIME_FS implementation.

  method class_constructor.
    cl_gui_frontend_services=>get_file_separator( changing file_separator = c_sep exceptions others = 4 ).
    if sy-subrc is not initial.
      c_sep = '\'. " Assume windows (eclipse ???)
    endif.
  endmethod.

  method write_file.

    cl_gui_frontend_services=>gui_download(
      exporting
        filename     = iv_filename
        filetype     = 'BIN'
        bin_filesize = iv_size
      changing
        data_tab   = ct_data
      exceptions
        others     = 1 ).

    if sy-subrc > 0.
      zcx_w3mime_error=>raise( 'Cannot write file' ). "#EC NOTEXT
    endif.

  endmethod.  " write_file.

  method write_file_x.

    data:
          lt_data type lvc_t_mime,
          lv_size type i.

    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer        = iv_data
      importing
        output_length = lv_size
      tables
        binary_tab    = lt_data.

    write_file(
      exporting
        iv_filename = iv_filename
        iv_size = lv_size
      changing
        ct_data = lt_data ).

  endmethod.  " write_file_x.
endclass.

**********************************************************************
* ZCL_W3MIME_ZIP_WRITER
**********************************************************************

class zcl_w3mime_zip_writer definition
  final
  create public .

  public section.

    type-pools abap .
    methods constructor
      importing
        !io_zip type ref to cl_abap_zip optional
        !iv_encoding type abap_encoding optional .
    methods add
      importing
        !iv_filename type string
        !iv_data type string .
    methods addx
      importing
        !iv_filename type string
        !iv_xdata type xstring .
    methods get_blob
      returning
        value(rv_blob) type xstring .
    methods read
      importing
        !iv_filename type string
      returning
        value(rv_data) type string
      raising
        zcx_w3mime_error .
    methods readx
      importing
        !iv_filename type string
      returning
        value(rv_xdata) type xstring
      raising
        zcx_w3mime_error .
    methods has
      importing
        !iv_filename type string
      returning
        value(r_yes) type abap_bool .
    methods is_dirty
      returning
        value(r_yes) type abap_bool .
    methods delete
      importing
        !iv_filename type string
      raising
        zcx_w3mime_error .
  private section.

    data mv_is_dirty type abap_bool .
    data mo_zip type ref to cl_abap_zip .
    data mo_conv_out type ref to cl_abap_conv_out_ce .
    data mo_conv_in type ref to cl_abap_conv_in_ce .
    type-pools abap .
    data mv_encoding type abap_encoding .
endclass.



class zcl_w3mime_zip_writer implementation.

  method add.
    data lv_xdata type xstring.
    mo_conv_out->convert(
      exporting data = iv_data
      importing buffer = lv_xdata ).

    addx(
      iv_filename = iv_filename
      iv_xdata    = lv_xdata ).
  endmethod.  " add.

  method addx.
    mo_zip->delete(
      exporting
        name = iv_filename
      exceptions others = 1 ). " ignore exceptions

    mo_zip->add( name = iv_filename content = iv_xdata ).
    mv_is_dirty = abap_true.
  endmethod.  " addx.

  method constructor.
    if io_zip is bound.
      mo_zip = io_zip.
    else.
      create object mo_zip.
    endif.

    if iv_encoding is not initial.
      mv_encoding = iv_encoding.
    else.
      mv_encoding = '4110'. " UTF8
    endif.

    mo_conv_out = cl_abap_conv_out_ce=>create( encoding = mv_encoding ).
    mo_conv_in  = cl_abap_conv_in_ce=>create( encoding = mv_encoding ).
  endmethod.  " constructor.

  method delete.
    mo_zip->delete( exporting name = iv_filename exceptions others = 4 ).
    if sy-subrc is not initial.
      zcx_w3mime_error=>raise( 'delete failed' ). "#EC NOTEXT
    endif.
    mv_is_dirty = abap_true.
  endmethod.

  method get_blob.
    rv_blob = mo_zip->save( ).
    mv_is_dirty = abap_false.
  endmethod.  " get_blob

  method has.
    read table mo_zip->files with key name = iv_filename transporting no fields.
    r_yes = boolc( sy-subrc is initial ).
  endmethod.

  method is_dirty.
    r_yes = mv_is_dirty.
  endmethod.

  method read.
    data:
          lv_xdata type xstring,
          lx       type ref to cx_root.

    lv_xdata = readx( iv_filename ).

    try.
      mo_conv_in->convert( exporting input = lv_xdata importing data = rv_data ).
    catch cx_root into lx.
      zcx_w3mime_error=>raise( msg = 'Codepage conversion error' ). "#EC NOTEXT
    endtry.

  endmethod.

  method readx.

    mo_zip->get(
      exporting
        name    = iv_filename
      importing
        content = rv_xdata
      exceptions zip_index_error = 1 ).

    if sy-subrc is not initial.
      zcx_w3mime_error=>raise( msg = |Cannot read { iv_filename }| ). "#EC NOTEXT
    endif.

    " Remove unicode signatures
    case mv_encoding.
      when '4110'. " UTF-8
        shift rv_xdata left deleting leading cl_abap_char_utilities=>byte_order_mark_utf8 in byte mode.
      when '4103'. " UTF-16LE
        shift rv_xdata left deleting leading cl_abap_char_utilities=>byte_order_mark_little in byte mode.
    endcase.

  endmethod.
endclass.


**********************************************************************
* END LIBRARY
**********************************************************************

class lcl_app definition final.
  public section.
    methods constructor raising cx_static_check.
    methods run
      importing
        iv_to_clip type abap_bool
        iv_as_html type abap_bool
    raising
      cx_static_check.

  private section.
    types:
      tty_t000 type standard table of t000 with key mandt.

    data mo_ser type ref to zcl_text2tab_serializer.
    data mo_zipw type ref to zcl_w3mime_zip_writer.

    methods get_t000
      returning
        value(rt_data) type tty_t000
      raising
        cx_static_check.
    methods save_to_file
      importing
        iv_data type string
      raising
        cx_static_check.
    methods save_to_clip
      importing
        iv_data type string
      raising
        cx_static_check.

endclass.

class lcl_app implementation.

  method constructor.
    create object mo_zipw.
    mo_ser = zcl_text2tab_serializer=>create( ).
  endmethod.

  method get_t000.
    write: / 'Backing up t000 ...'. "#EC NOTEXT
    select * from t000 into table rt_data.
  endmethod.

  method save_to_clip.

    types ty_line type c length 256.
    data lt_tab type table of ty_line.
    data lv_rc type i.

    split iv_data at cl_abap_char_utilities=>cr_lf into table lt_tab.

    cl_gui_frontend_services=>clipboard_export(
      importing
        data                 = lt_tab
      changing
        rc                   = lv_rc
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        no_authority         = 4
        others               = 5 ).

    write: / 'Done. Data saved to:' color 5, 'clipboard'. "#EC NOTEXT

  endmethod.

  method save_to_file.

    write: / 'Archiving ...'. "#EC NOTEXT

    mo_zipw->add(
      iv_filename = 't000.txt'
      iv_data = iv_data ).

    data lv_xdata type xstring.
    lv_xdata = mo_zipw->get_blob( ).

    data lv_filename type string.
    lv_filename = |ztext2tab-t000-example-{ sy-datum }_{ sy-uzeit }.zip|.
    zcl_w3mime_fs=>write_file_x(
      iv_filename = lv_filename
      iv_data     = lv_xdata ).

    data lv_workdir_name type string.
    cl_gui_frontend_services=>directory_get_current( changing current_directory = lv_workdir_name ).
    cl_gui_cfw=>flush( ).

    write: / 'Current dir:', lv_workdir_name. "#EC NOTEXT
    write: / 'Done. File saved to:' color 5, lv_filename. "#EC NOTEXT

  endmethod.

  method run.

    data lt_data type tty_t000.
    lt_data = get_t000( ).

    data lv_data type string.
    if iv_as_html = abap_true.
      data lt_text_fields type string_table.
      append 'cccoractiv' to lt_text_fields.
      mo_ser->as_html(
        i_bold_header = abap_true
        i_text_fields = lt_text_fields ).
    endif.
    lv_data = mo_ser->serialize( lt_data ).

    if iv_to_clip = abap_true.
      save_to_clip( lv_data ).
    else.
      save_to_file( lv_data ).
    endif.

  endmethod.

endclass.

**********************************************************************
* FORMS
**********************************************************************

form main
  using
    p_to_clip type abap_bool
    p_as_html type abap_bool.

  data lx type ref to cx_root.
  data lo_app type ref to lcl_app.

  " Ensure right version of text2tab
  data lv_required_text2tab_ver type string value 'v2.1.1'.
  if zcl_text2tab_parser=>check_version_fits( lv_required_text2tab_ver ) = abap_false.
    write: / 'Error: text2tab version is lower than required' color 6. "#EC NOTEXT
    write: / 'Required is:', lv_required_text2tab_ver. "#EC NOTEXT
    write: / 'Installed is:', zif_text2tab=>version. "#EC NOTEXT
    write: / 'Please upgrade text2tab'. "#EC NOTEXT
    return.
  endif.

  try .
    create object lo_app.
    lo_app->run(
      iv_to_clip = p_to_clip
      iv_as_html = p_as_html ).
  catch cx_root into lx.
    data msg type string.
    msg = lx->get_text( ).
    write: / 'Error:' color 6, msg. "#EC NOTEXT
  endtry.

endform.

selection-screen begin of block b1 with frame title txt_b1.

selection-screen begin of line.
parameters p_p0 radiobutton group g1 default 'X'.
selection-screen comment (60) txt_p0.
selection-screen end of line.

selection-screen begin of line.
parameters p_p1 radiobutton group g1.
selection-screen comment (60) txt_p1.
selection-screen end of line.

selection-screen begin of line.
parameters p_p2 type abap_bool as checkbox.
selection-screen comment (60) txt_p2.
selection-screen end of line.

selection-screen end of block b1.

initialization.

  txt_b1   = 'This program saves table T000...'. "#EC NOTEXT
  txt_p0   = '... to sap workdir'. "#EC NOTEXT
  txt_p1   = '... to clipboard'. "#EC NOTEXT
  txt_p2   = ' as HTML'. "#EC NOTEXT

start-of-selection.

  perform main using p_p1 p_p2.
