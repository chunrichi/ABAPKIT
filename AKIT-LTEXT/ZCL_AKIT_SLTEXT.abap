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
        lines  TYPE text_line_tab,
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

    DATA: lt_thead TYPE TABLE OF thead.
    DATA: lt_text_table TYPE text_lh.

    LOOP AT tdnames INTO DATA(lv_tdname).
      APPEND VALUE #(
        tdname   = lv_tdname
       " relid    = me->relid
        tdobject = me->tdobject
        tdid     = me->tdid
        tdspras  = me->tdspras
      ) TO lt_thead.
    ENDLOOP.

    CALL FUNCTION 'READ_TEXT_TABLE'
      IMPORTING
        text_table              = lt_text_table
      TABLES
        text_headers            = lt_thead
      EXCEPTIONS
        wrong_access_to_archive = 1
        OTHERS                  = 2.

    LOOP AT lt_text_table INTO DATA(ls_text).
      APPEND VALUE #(
        tdname = ls_text-header-tdname
        lines  = ls_text-lines
      ) TO me->stxls.
    ENDLOOP.

    SORT me->stxls BY tdname.

  ENDMETHOD.


  METHOD read.

    " 读取数据
    READ TABLE me->stxls INTO DATA(ls_stxls) WITH KEY tdname = tdname BINARY SEARCH.
    IF sy-subrc = 0.
      tlines = ls_stxls-lines.
    ENDIF.

  ENDMETHOD.


  METHOD stline2str.

    " 多行拼接
    " 固定方法

    LOOP AT tline INTO DATA(ls_tline).
      str =  str && ls_tline-tdline.

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