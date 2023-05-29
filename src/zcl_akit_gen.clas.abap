class ZCL_AKIT_GEN definition
  public
  final
  create public .

public section.

  types:
    tt_table_filds TYPE TABLE OF dd03p .
  types:
    BEGIN OF ty_result,
        " 返回消息
        type    TYPE bapi_mtype,
        message TYPE bapi_msg,
      END OF ty_result .

  class-methods TABLE
    importing
      !TABLENAME type DD02L-TABNAME
      !TABLEDESC type AS4TEXT
      !FIELDS type TT_TABLE_FILDS
    returning
      value(RS_RESULT) type TY_RESULT .
  class-methods LOCK .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS table_exists
      IMPORTING
        !tablename       TYPE dd02l-tabname
      RETURNING
        VALUE(rv_exists) TYPE abap_bool .
ENDCLASS.



CLASS ZCL_AKIT_GEN IMPLEMENTATION.


  METHOD lock.
  ENDMETHOD.


  METHOD table.

    " 表存在检查
    IF table_exists( tablename ) = abap_true.
      rs_result = VALUE #( type = 'E' message = 'table is exists' ).
      RETURN.
    ENDIF.

    " 创建表
    DATA: lv_rc       LIKE sy-subrc,
          lv_obj_name TYPE tadir-obj_name,
          ls_dd02v    TYPE dd02v,
          ls_dd09l    TYPE dd09l,
          lt_dd03p    TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_dd03p> LIKE LINE OF lt_dd03p.

    ls_dd02v-tabname    = tablename. " 表名
    ls_dd02v-ddlanguage = sy-langu.  " 原语言
    ls_dd02v-tabclass   = 'TRANSP'.
    ls_dd02v-ddtext     = tabledesc. " 描述
    ls_dd02v-contflag   = 'L'.       " 交付类
    ls_dd02v-exclass    = '1'.       " 扩展类（是否可增强 1 无法增强）

    ls_dd09l-tabname  = tablename.
    ls_dd09l-as4local = 'A'.         " 交付类
    ls_dd09l-tabkat   = '0'.         " 大小类别（最小为0）
    ls_dd09l-tabart   = 'APPL1'.     " 数据类（0主数据、1事务数据）
    ls_dd09l-bufallow = 'N'.         " 编辑类型（N不允许编辑）

    LOOP AT fields INTO DATA(ls_fields).
      APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
      MOVE-CORRESPONDING ls_fields TO <ls_dd03p>.

      <ls_dd03p>-tabname = tablename.

      " 常用字段
      " fieldname
      " position
      " keyflag
      " datatype
      " leng
    ENDLOOP.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = tablename
        dd02v_wa          = ls_dd02v
        dd09l_wa          = ls_dd09l
      TABLES
        dd03p_tab         = lt_dd03p
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
                 sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO rs_result-message.
      rs_result-type = 'E'.
      RETURN.
    ENDIF.

    lv_obj_name = tablename.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'TABL'
        wi_tadir_obj_name = lv_obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = '$TMP'
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
                 sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO rs_result-message.
      rs_result-type = 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name        = tablename
        auth_chk    = abap_false
      IMPORTING
        rc          = lv_rc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0 OR lv_rc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
                 sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO rs_result-message.
      IF rs_result-message IS INITIAL.
        rs_result-message = `migrate, error from DDIF_TABL_ACTIVATE`.
      ENDIF.
      rs_result-type = 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD table_exists.

    DATA: lv_tabname TYPE dd02l-tabname.

    SELECT SINGLE tabname FROM dd02l INTO lv_tabname
      WHERE tabname = tablename.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.
ENDCLASS.
