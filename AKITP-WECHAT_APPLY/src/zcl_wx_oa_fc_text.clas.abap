CLASS zcl_wx_oa_fc_text DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wx_oa_fc .

    DATA control TYPE string VALUE `Text` ##NO_TEXT.
    DATA id TYPE string .
    DATA:
      BEGIN OF value,
        text TYPE string,
      END OF value .

    METHODS constructor
      IMPORTING
        !id TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WX_OA_FC_TEXT IMPLEMENTATION.


  METHOD constructor.

    me->id = id.
  ENDMETHOD.
ENDCLASS.
