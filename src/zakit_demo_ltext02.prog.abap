REPORT zakit_demo_ltext02.

" 长文本读取 -> 占用空间

TYPES: BEGIN OF ty_eban,
         banfn TYPE eban-banfn,
         bnfpo TYPE eban-bnfpo,
       END OF ty_eban.
DATA: lt_eban TYPE TABLE OF ty_eban.

DATA: lt_tline TYPE TABLE OF tline.

cl_abap_memory_utilities=>get_total_used_size(  IMPORTING size = DATA(lv_bs_size) ).

*SELECT
*  banfn,
*  bnfpo
*  FROM eban
*  INTO TABLE @lt_eban
*  UP TO 20000 ROWS.
SELECT
  tdname
  FROM stxl
  WHERE tdobject = 'EBAN'
    AND tdid = 'B01'
    AND tdspras = '1'
  INTO TABLE @lt_eban
  UP TO 10000 ROWS.

cl_abap_memory_utilities=>get_total_used_size(  IMPORTING size = DATA(lv_es_size) ).

DATA(lr_ltext) = NEW zcl_akit_ltext( tdobject = 'EBAN' tdid = 'B01' tdspras = '1' ).

cl_abap_memory_utilities=>get_total_used_size(  IMPORTING size = DATA(lv_init_size) ).

lr_ltext->load( VALUE #( FOR eb IN lt_eban ( |{ eb-banfn }{ eb-bnfpo }| ) ) ).

cl_abap_memory_utilities=>get_total_used_size(  IMPORTING size = DATA(lv_load_size) ).

cl_demo_output=>write( lv_bs_size ).
cl_demo_output=>write( lv_es_size ).
cl_demo_output=>write( lv_init_size ).
cl_demo_output=>write( lv_load_size ).

LOOP AT lt_eban INTO DATA(ls_eban1).
  IF sy-tabix = 3.
    EXIT.
  ENDIF.

  cl_abap_memory_utilities=>get_total_used_size(  IMPORTING size = DATA(lv_read_size) ).
  cl_demo_output=>write( lv_read_size ).

  DATA(lt_str) = lr_ltext->read( |{ ls_eban1-banfn }{ ls_eban1-bnfpo }| ).

  cl_abap_memory_utilities=>get_total_used_size(  IMPORTING size = DATA(lv_eread_size) ).
  cl_demo_output=>write( lv_eread_size ).

ENDLOOP.

cl_abap_memory_utilities=>get_total_used_size(  IMPORTING size = DATA(lv_ereads_size) ).
cl_demo_output=>write( lv_ereads_size ).


cl_demo_output=>display( ).
