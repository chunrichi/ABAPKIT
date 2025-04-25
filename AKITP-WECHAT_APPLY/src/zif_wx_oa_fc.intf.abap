INTERFACE zif_wx_oa_fc
  PUBLIC .

  TYPES: BEGIN OF ty_text,
           text TYPE string,
           lang TYPE string,
         END OF ty_text,
         tt_text TYPE STANDARD TABLE OF ty_text WITH DEFAULT KEY.
  TYPES: BEGIN OF ty_kv,
           key   TYPE string,
           value TYPE string,
         END OF ty_kv,
         tt_kv TYPE STANDARD TABLE OF ty_kv WITH DEFAULT KEY.
ENDINTERFACE.
