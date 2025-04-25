CLASS zcl_wx_oa_fc_number DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wx_oa_fc .

    DATA control TYPE string VALUE `Number` ##NO_TEXT.
    DATA id TYPE string .
    DATA:
      BEGIN OF value,
        new_number TYPE string,
      END OF value .

    METHODS constructor
      IMPORTING
        !id TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WX_OA_FC_NUMBER IMPLEMENTATION.


  METHOD constructor.

    me->id = id.
  ENDMETHOD.
ENDCLASS.
