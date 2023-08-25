CLASS zcl_akit_ltext DEFINITION
PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_tline TYPE TABLE OF tline WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ty_stxl,
        tdname TYPE stxl-tdname,
        srtf2  TYPE stxl-srtf2,
        clustr TYPE stxl-clustr,
        clustd TYPE stxl-clustd,
      END OF ty_stxl .
    TYPES:
      tt_stxl TYPE TABLE OF ty_stxl .
    TYPES:
      tt_tdname TYPE TABLE OF stxl-tdname .

    METHODS constructor
      IMPORTING
        !relid    TYPE stxl-relid DEFAULT 'TX'
        !tdobject TYPE stxl-tdobject
        !tdid     TYPE stxl-tdid
        !tdspras  TYPE stxl-tdspras .
    METHODS load
      IMPORTING
        !tdnames TYPE tt_tdname .
    METHODS read
      IMPORTING
        !tdname       TYPE stxl-tdname
      RETURNING
        VALUE(tlines) TYPE tt_tline .
    METHODS tline2str
      IMPORTING
        !tline     TYPE tt_tline
      RETURNING
        VALUE(str) TYPE string .
    METHODS exists
      IMPORTING
        !tdname      TYPE stxl-tdname
      RETURNING
        VALUE(subrc) TYPE sysubrc .
    CLASS-METHODS stline2str
      IMPORTING
        !tline     TYPE tt_tline
      RETURNING
        VALUE(str) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA relid TYPE stxl-relid .
    DATA tdobject TYPE stxl-tdobject .
    DATA tdid TYPE stxl-tdid .
    DATA tdspras TYPE stxl-tdspras .
    DATA stxls TYPE tt_stxl .
ENDCLASS.



CLASS ZCL_AKIT_LTEXT IMPLEMENTATION.


  METHOD constructor.

    me->relid    = relid.
    me->tdobject = tdobject.
    me->tdid     = tdid.
    me->tdspras  = tdspras.

  ENDMETHOD.


  METHOD exists.

    READ TABLE me->stxls TRANSPORTING NO FIELDS WITH KEY tdname = tdname BINARY SEARCH.

    subrc = sy-subrc.

  ENDMETHOD.


  METHOD load.

    IF tdnames IS INITIAL.
      RETURN.
    ENDIF.

    SELECT
      tdname,
      srtf2,
      clustr,
      clustd
      FROM ('stxl')
      FOR ALL ENTRIES IN @tdnames
      WHERE tdname = @tdnames-table_line
        AND relid  = @me->relid
        AND tdobject = @me->tdobject
        AND tdid   = @me->tdid
        AND tdspras = @me->tdspras
      INTO TABLE @me->stxls.

    SORT me->stxls BY tdname srtf2.

  ENDMETHOD.


  METHOD read.
    DATA: lv_tabix TYPE sytabix.

    DATA: lv_xstring TYPE xstring.

    " 读取数据
    READ TABLE me->stxls TRANSPORTING NO FIELDS WITH KEY tdname = tdname BINARY SEARCH.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.

      LOOP AT me->stxls INTO DATA(ls_stxl) FROM lv_tabix.
        IF ls_stxl-tdname <> tdname.
          EXIT.
        ENDIF.

        lv_xstring = lv_xstring && ls_stxl-clustd.
      ENDLOOP.
    ENDIF.

    IF lv_xstring IS NOT INITIAL.
      IMPORT tline = tlines FROM DATA BUFFER lv_xstring.
    ENDIF.

  ENDMETHOD.


  METHOD stline2str.

    " 多行拼接
    " 固定方法

    LOOP AT tline INTO DATA(ls_tline).
      str = str && ls_tline-tdline.

      AT LAST.
        EXIT.
      ENDAT.

      IF ls_tline-tdformat = '*'.
        str = str && cl_abap_char_utilities=>cr_lf.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD tline2str.

    " 多行拼接

    str = stline2str( tline ).

  ENDMETHOD.
ENDCLASS.