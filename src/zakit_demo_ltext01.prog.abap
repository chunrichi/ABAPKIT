REPORT zakit_demo_ltext01.

" 长文本读取 -> 执行速度


TYPES: BEGIN OF ty_eban,
         banfn TYPE eban-banfn,
         bnfpo TYPE eban-bnfpo,
       END OF ty_eban.
DATA: lt_eban TYPE TABLE OF ty_eban.

DATA: lt_tline TYPE TABLE OF tline.

DATA: lv_btimestamp TYPE timestampl.
DATA: lv_etimestamp TYPE timestampl.

*SELECT
*  banfn,
*  bnfpo
*  FROM eban
*  INTO TABLE @lt_eban
*  UP TO 10000 ROWS.
SELECT
  tdname
  FROM stxl
  WHERE tdobject = 'EBAN'
    AND tdid = 'B01'
    AND tdspras = '1'
  INTO TABLE @lt_eban
  UP TO 10000 ROWS.

" --> 通过 READ TEXT 逐行读取

GET TIME STAMP FIELD lv_btimestamp.

LOOP AT lt_eban INTO DATA(ls_eban).
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'B01'
      language                = '1'
      name                    = CONV thead-tdname( |{ ls_eban-banfn }{ ls_eban-bnfpo }| )
      object                  = 'EBAN'
    TABLES
      lines                   = lt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF lt_tline IS NOT INITIAL.
    EXIT.
  ENDIF.

  CLEAR lt_tline.
ENDLOOP.

GET TIME STAMP FIELD lv_etimestamp.

DATA: lv_cast TYPE timestampl.

lv_cast = cl_abap_tstmp=>subtract( tstmp1 = lv_etimestamp tstmp2 = lv_btimestamp ).

cl_demo_output=>write( lv_cast ).

" <--

" >>> 通过 ZCL_AKIT_LTEXT 批量读取

GET TIME STAMP FIELD lv_btimestamp.

DATA(lr_ltext) = NEW zcl_akit_ltext( tdobject = 'EBAN' tdid = 'B01' tdspras = '1' ).

lr_ltext->load( VALUE #( FOR eb IN lt_eban ( |{ eb-banfn }{ eb-bnfpo }| ) ) ).

LOOP AT lt_eban INTO DATA(ls_eban1).
  DATA(lt_str) = lr_ltext->read( |{ ls_eban1-banfn }{ ls_eban1-bnfpo }| ).
ENDLOOP.

GET TIME STAMP FIELD lv_etimestamp.

lv_cast = cl_abap_tstmp=>subtract( tstmp1 = lv_etimestamp tstmp2 = lv_btimestamp ).

cl_demo_output=>write( lv_cast ).

" <<<

cl_demo_output=>display( ).
