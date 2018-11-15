class ZCX_TEXT2TAB_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_TEXT2TAB_ERROR,
      msgid type symsgid value 'SY',
      msgno type symsgno value '499',
      attr1 type scx_attrname value 'METHNAME',
      attr2 type scx_attrname value 'MSG',
      attr3 type scx_attrname value 'LOCATION',
      attr4 type scx_attrname value '',
    end of ZCX_TEXT2TAB_ERROR .
  data METHNAME type STRING read-only .
  data MSG type STRING read-only .
  data CODE type CHAR2 read-only .
  data LOCATION type STRING read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !METHNAME type STRING optional
      !MSG type STRING optional
      !CODE type CHAR2 optional
      !LOCATION type STRING optional .
  class-methods RAISE
    importing
      !MSG type STRING
      !CODE type CHAR2 optional
      !LOCATION type STRING optional
      !METHNAME type CLIKE optional
    raising
      ZCX_TEXT2TAB_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCX_TEXT2TAB_ERROR IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->METHNAME = METHNAME .
me->MSG = MSG .
me->CODE = CODE .
me->LOCATION = LOCATION .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_TEXT2TAB_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.


METHOD raise.
  raise exception type zcx_text2tab_error
    exporting
      msg  = msg
      code = code
      methname = methname
      location = location.
ENDMETHOD.
ENDCLASS.
