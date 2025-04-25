CLASS zcl_wx_oa_fc_selector DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wx_oa_fc .

    TYPES:
      BEGIN OF ENUM t_type,
        single, multi,
      END OF ENUM t_type,
      BEGIN OF ty_option,
        key TYPE string,
      END OF ty_option,
      tt_option TYPE STANDARD TABLE OF ty_option WITH DEFAULT KEY.

    DATA control TYPE string VALUE `Selector` ##NO_TEXT.
    DATA id TYPE string .
    DATA:
      BEGIN OF value,
        BEGIN OF selector,
          type    TYPE string,
          options TYPE tt_option,
        END OF selector,
      END OF value .

    METHODS constructor
      IMPORTING
        !id   TYPE string
        !type TYPE t_type .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WX_OA_FC_SELECTOR IMPLEMENTATION.


  METHOD constructor.

    me->id = id.

    " 老版本 ui2/cl_json 不支持 枚举

    CASE type.
      WHEN single.
        me->value-selector-type = 'single'.
      WHEN multi.
        me->value-selector-type = 'multi'.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
