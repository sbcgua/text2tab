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

class ZCX_W3MIME_ERROR definition
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_W3MIME_ERROR,
      msgid type symsgid value 'SY',
      msgno type symsgno value '499',
      attr1 type scx_attrname value 'MSG',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_W3MIME_ERROR .
  data MSG type STRING read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSG type STRING optional .
  class-methods RAISE
    importing
      !MSG type STRING
    raising
      ZCX_W3MIME_ERROR .
ENDCLASS.

CLASS ZCX_W3MIME_ERROR IMPLEMENTATION.


method CONSTRUCTOR.
  CALL METHOD SUPER->CONSTRUCTOR
  EXPORTING
  PREVIOUS = PREVIOUS .
  me->MSG = MSG .
  clear me->textid.
  if textid is initial.
    IF_T100_MESSAGE~T100KEY = ZCX_W3MIME_ERROR .
  else.
    IF_T100_MESSAGE~T100KEY = TEXTID.
  endif.
endmethod.


method raise.
  raise exception type zcx_w3mime_error
    exporting
      textid = zcx_w3mime_error
      msg    = msg.
endmethod.
ENDCLASS.

**********************************************************************
* ZCL_W3MIME_FS
**********************************************************************

class ZCL_W3MIME_FS definition
  final
  create public .

public section.

  types:
    tt_files type standard table of file_info with key filename .

  class-data C_SEP type CHAR1 read-only .

  class-methods WRITE_FILE
    importing
      !IV_FILENAME type STRING
      !IV_SIZE type I
    changing
      !CT_DATA type LVC_T_MIME
    raising
      ZCX_W3MIME_ERROR .
  class-methods WRITE_FILE_X
    importing
      !IV_FILENAME type STRING
      !IV_DATA type XSTRING
    raising
      ZCX_W3MIME_ERROR .
  class-methods CLASS_CONSTRUCTOR .
ENDCLASS.

CLASS ZCL_W3MIME_FS IMPLEMENTATION.

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
ENDCLASS.

**********************************************************************
* ZCL_W3MIME_ZIP_WRITER
**********************************************************************

class ZCL_W3MIME_ZIP_WRITER definition
  final
  create public .

public section.

  type-pools ABAP .
  methods CONSTRUCTOR
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP optional
      !IV_ENCODING type ABAP_ENCODING optional .
  methods ADD
    importing
      !IV_FILENAME type STRING
      !IV_DATA type STRING .
  methods ADDX
    importing
      !IV_FILENAME type STRING
      !IV_XDATA type XSTRING .
  methods GET_BLOB
    returning
      value(RV_BLOB) type XSTRING .
  methods READ
    importing
      !IV_FILENAME type STRING
    returning
      value(RV_DATA) type STRING
    raising
      ZCX_W3MIME_ERROR .
  methods READX
    importing
      !IV_FILENAME type STRING
    returning
      value(RV_XDATA) type XSTRING
    raising
      ZCX_W3MIME_ERROR .
  methods HAS
    importing
      !IV_FILENAME type STRING
    returning
      value(R_YES) type ABAP_BOOL .
  methods IS_DIRTY
    returning
      value(R_YES) type ABAP_BOOL .
  methods DELETE
    importing
      !IV_FILENAME type STRING
    raising
      ZCX_W3MIME_ERROR .
private section.

  data MV_IS_DIRTY type ABAP_BOOL .
  data MO_ZIP type ref to CL_ABAP_ZIP .
  data MO_CONV_OUT type ref to CL_ABAP_CONV_OUT_CE .
  data MO_CONV_IN type ref to CL_ABAP_CONV_IN_CE .
  type-pools ABAP .
  data MV_ENCODING type ABAP_ENCODING .
ENDCLASS.



CLASS ZCL_W3MIME_ZIP_WRITER IMPLEMENTATION.

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

method HAS.
  read table mo_zip->files with key name = iv_filename transporting no fields.
  r_yes = boolc( sy-subrc is initial ).
endmethod.

method is_dirty.
  r_yes = mv_is_dirty.
endmethod.

method READ.
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

method READX.

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
      shift rv_xdata left deleting leading  cl_abap_char_utilities=>byte_order_mark_utf8 in byte mode.
    when '4103'. " UTF-16LE
      shift rv_xdata left deleting leading  cl_abap_char_utilities=>byte_order_mark_little in byte mode.
  endcase.

endmethod.
ENDCLASS.


**********************************************************************
* END LIBRARY
**********************************************************************

class lcl_app definition final.
  public section.
    methods constructor raising cx_static_check.
    methods run raising cx_static_check.

  private section.
    data mo_ser type ref to zcl_text2tab_serializer.
    data mo_zipw type ref to zcl_w3mime_zip_writer.
    methods pile_data
      importing
        it_tab  type standard table
        iv_name type string
      raising cx_static_check.
    methods save_t000 raising cx_static_check.

endclass.

class lcl_app implementation.

  method constructor.
    create object mo_zipw.
    mo_ser = zcl_text2tab_serializer=>create( ).
  endmethod.

  method pile_data.
    data lv_data type string.
    lv_data = mo_ser->serialize( it_tab ).
    mo_zipw->add( iv_filename = iv_name iv_data = lv_data ).
  endmethod.

  method save_t000.
    data lt_data type standard table of t000.
    write: / 'Backing up t000 ...'. "#EC NOTEXT
    select * from t000 into table lt_data.
    pile_data(
      iv_name = 't000.txt'
      it_tab  = lt_data ).
  endmethod.

  method run.

    save_t000( ).
    write: / 'Archiving ...'. "#EC NOTEXT

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

endclass.

**********************************************************************
* FORMS
**********************************************************************

form main.
  data lx type ref to cx_root.
  data lo_app type ref to lcl_app.

  " Ensure right version of text2tab
  data lv_required_text2tab_ver type string value 'v2.1.1'.
  if zcl_text2tab_parser=>check_version_fits( lv_required_text2tab_ver ) = abap_false.
    write: / 'Error: text2tab version is lower than required' color 6. "#EC NOTEXT
    write: / 'Required is:', lv_required_text2tab_ver. "#EC NOTEXT
    write: / 'Installed is:', zif_text2tab_constants=>version. "#EC NOTEXT
    write: / 'Please upgrade text2tab'. "#EC NOTEXT
    return.
  endif.

  try .
    create object lo_app.
    lo_app->run( ).
  catch cx_root into lx.
    data msg type string.
    msg = lx->get_text( ).
    write: / 'Error:' color 6, msg. "#EC NOTEXT
  endtry.

endform.

selection-screen begin of block b1 with frame title txt_b1.

selection-screen begin of line.
parameters p_p0 type char1.
selection-screen comment (60) txt_p0.
selection-screen end of line.

selection-screen end of block b1.

initialization.

  txt_b1   = 'Info'. "#EC NOTEXT
  txt_p0   = 'This program saves table T000 to sap workdir'. "#EC NOTEXT

start-of-selection.

  perform main.
