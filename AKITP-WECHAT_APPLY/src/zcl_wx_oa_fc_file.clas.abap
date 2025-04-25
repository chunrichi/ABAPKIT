CLASS zcl_wx_oa_fc_file DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wx_oa_fc .

    TYPES: BEGIN OF ty_file,
             file_id   TYPE string,
             file_name TYPE string,
             file_size TYPE string,
             file_type TYPE string,
             file_url  TYPE string,
           END OF ty_file,
           tt_files TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.

    DATA control TYPE string VALUE `File` ##NO_TEXT.
    DATA id TYPE string .
    DATA:
      BEGIN OF value,
        files TYPE tt_files,
      END OF value .

    METHODS constructor
      IMPORTING
        !id TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WX_OA_FC_FILE IMPLEMENTATION.


  METHOD constructor.

    me->id = id.
  ENDMETHOD.
ENDCLASS.
