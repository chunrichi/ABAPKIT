CLASS zcl_wx_oa_fc_date DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wx_oa_fc .

    TYPES: BEGIN OF ENUM t_type,
             day, hour,
           END OF ENUM t_type.

    DATA control TYPE string VALUE `Date` ##NO_TEXT.
    DATA id TYPE string .
    DATA:
      BEGIN OF value,
        BEGIN OF date,
          type        TYPE string,
          s_timestamp TYPE string,
        END OF date,
      END OF value .

    METHODS constructor
      IMPORTING
        !id   TYPE string
        !type TYPE t_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WX_OA_FC_DATE IMPLEMENTATION.


  METHOD constructor.

    me->id = id.

    CASE type.
      WHEN day.
        me->value-date-type = 'day'.
      WHEN hour.
        me->value-date-type = 'hour'.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
