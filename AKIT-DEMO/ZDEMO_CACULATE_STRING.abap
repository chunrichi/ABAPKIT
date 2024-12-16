REPORT zdemo_caculate_string.

" 字符计算

" 已实现: + - * /
" 已实现: ()


CLASS lcl_calc DEFINITION.
  PUBLIC SECTION.
    DATA: calcform TYPE string. " 计算字符串 "

    DATA: operand_stack TYPE TABLE OF acdoca-hsl. " 数值堆栈
    DATA: operator_stack TYPE TABLE OF char1.     " 运算符堆栈

    METHODS constructor IMPORTING icalcform TYPE string.

    METHODS parse RETURNING VALUE(ohsl) TYPE acdoca-hsl.

    METHODS calculate.

    METHODS push_operand IMPORTING operand TYPE acdoca-hsl.
    METHODS push_operator IMPORTING operator TYPE char1.
    METHODS pop_operand RETURNING VALUE(operand) TYPE string.
    METHODS pop_operator RETURNING VALUE(operator) TYPE char1.

    METHODS is_operator_higher_priority IMPORTING iv_new_operator  TYPE char1
                                                  iv_top_operator  TYPE char1
                                        RETURNING VALUE(rv_result) TYPE abap_bool.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_operator_priority,
             operator TYPE string,
             priority TYPE i,
           END OF ty_operator_priority.
    DATA: lt_operator_priorities TYPE TABLE OF ty_operator_priority.
ENDCLASS.

START-OF-SELECTION.

  PERFORM frm_test_calc_string.

*&---------------------------------------------------------------------*
*& Form FRM_TEST_CALC_STRING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM frm_test_calc_string .

  DATA: l_calc     TYPE string,
        l_calc_hsl TYPE acdoca-hsl,
        l_hsl      TYPE acdoca-hsl.


  l_hsl = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 100.
  l_calc = '= 1 + 2 + 3 + 4 + 5 + 6 + 7 + 100'.

  CONDENSE l_calc NO-GAPS.
  DATA(lo_calc) = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  l_hsl = 1 + 2 - 3.
  l_calc = '= 1 + 2 - 3'.

  CONDENSE l_calc NO-GAPS.
  lo_calc = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  l_hsl = 10 * 5.
  l_calc = '= 10 * 5'.

  CONDENSE l_calc NO-GAPS.
  lo_calc = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  l_hsl = ( 10 + 20 ) * 3.
  l_calc = '=(10 + 20) * 3'.

  CONDENSE l_calc NO-GAPS.
  lo_calc = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  l_hsl = 100 / 10.
  l_calc = '= 100 / 10'.

  CONDENSE l_calc NO-GAPS.
  lo_calc = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  l_hsl = 100 - 50.
  l_calc = '= 100 - 50'.

  CONDENSE l_calc NO-GAPS.
  lo_calc = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  l_hsl = 2 ** 3.
  l_calc = '= 2 ** 3'.

  CONDENSE l_calc NO-GAPS.
  lo_calc = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  l_hsl = -1 + 2 - 3.
  l_calc = '= -1 + 2 - 3'.

  CONDENSE l_calc NO-GAPS.
  lo_calc = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  l_hsl = '10.5' + '20.5'.
  l_calc = '= 10.5 + 20.5'.

  CONDENSE l_calc NO-GAPS.
  lo_calc = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  l_hsl = 100 / 3.
  l_calc = '= 100 / 3'.

  CONDENSE l_calc NO-GAPS.
  lo_calc = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  l_hsl = 100 DIV 3.
  l_calc = '= 100 DIV 3'.

  CONDENSE l_calc NO-GAPS.
  lo_calc = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  l_hsl = 100 MOD 3.
  l_calc = '= 100 MOD 3'.

  CONDENSE l_calc NO-GAPS.
  lo_calc = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  l_hsl = ( 10 + 20 ) * 3 + 40.
  l_calc = '=(10 + 20 ) * 3 + 40'.

  CONDENSE l_calc NO-GAPS.
  lo_calc = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  l_hsl = 100 / ( 10 + 5 ) * 2.
  l_calc = '= 100 / (10 + 5) * 2'.

  CONDENSE l_calc NO-GAPS.
  lo_calc = NEW lcl_calc( l_calc ).
  cl_demo_output=>write( l_calc ).

  l_calc_hsl = lo_calc->parse( ).
  cl_demo_output=>write( boolc( l_calc_hsl = l_hsl ) ).

  IF l_calc_hsl <> l_hsl.
    cl_demo_output=>write( l_calc_hsl ).
    cl_demo_output=>write( l_hsl ).
  ENDIF.

  cl_demo_output=>display( ).

ENDFORM.


CLASS lcl_calc IMPLEMENTATION.

  METHOD constructor.
    me->calcform = icalcform.

    " 初始化运算符优先级映射表
    me->lt_operator_priorities = VALUE #(
        ( operator = '+' priority = 1 )
        ( operator = '-' priority = 1 )
        ( operator = '*' priority = 2 )
        ( operator = '/' priority = 2 )
        ( operator = '(' priority = 0 )
    ).
  ENDMETHOD.

  METHOD parse.

    DATA: l_cursor          TYPE c,
          l_cursor_index    TYPE i,
          l_cursor_operator TYPE c.
    DATA: l_seqno_s TYPE string,
          l_seqno   TYPE acdoca-hsl.

    REFRESH: me->operand_stack, me->operator_stack.

    " 遍历字符串
    WHILE l_cursor_index < strlen( me->calcform ).
      l_cursor = me->calcform+l_cursor_index(1).

      IF l_cursor CA '0123456789'.
        " 数字 -> 对应 seqno 索引
        l_seqno_s = l_cursor.
        ADD 1 TO l_cursor_index.

        WHILE l_cursor_index < strlen( me->calcform ) AND me->calcform+l_cursor_index(1) CA '0123456789'.
          l_seqno_s = l_seqno_s && me->calcform+l_cursor_index(1).
          ADD 1 TO l_cursor_index.
        ENDWHILE.
        l_seqno = l_seqno_s.
        push_operand( l_seqno ).
      ELSE.
        " 公式符号

        CASE l_cursor.
          WHEN '('.
            " 压入运算符堆栈
            push_operator( l_cursor ).
            ADD 1 TO l_cursor_index.
          WHEN ')'.
            " 弹出运算符进行计算
            "l_cursor_operator = pop_operator( ).
            WHILE l_cursor_operator NE '('.
              calculate( ).
              l_cursor_operator = pop_operator( ).
            ENDWHILE.
            ADD 1 TO l_cursor_index.
          WHEN '=' OR space.
            " pass
            ADD 1 TO l_cursor_index.
          WHEN OTHERS.
            l_cursor_operator = l_cursor.

            " 根据运算符优先级处理当前运算符
            WHILE NOT me->operator_stack IS INITIAL
              AND
               is_operator_higher_priority( iv_new_operator = l_cursor_operator
                                            iv_top_operator = me->operator_stack[ lines( me->operator_stack ) ] ) = abap_false.
              calculate( ).
              "l_cursor_operator = pop_operator( ).
            ENDWHILE.

            push_operator( l_cursor_operator ).
            ADD 1 TO l_cursor_index.
        ENDCASE.

      ENDIF.

    ENDWHILE.

    " 处理剩余在运算符栈中的运算符进行计算
    WHILE NOT me->operator_stack IS INITIAL.
      calculate( ).
    ENDWHILE.

    " 返回最终计算结果
    ohsl = pop_operand( ).

  ENDMETHOD.


  METHOD push_operand.
    APPEND operand TO me->operand_stack.
  ENDMETHOD.

  METHOD push_operator.
    IF operator IS NOT INITIAL.
      APPEND operator TO me->operator_stack.
    ENDIF.
  ENDMETHOD.

  METHOD pop_operand.
    CHECK NOT me->operand_stack IS INITIAL.
    operand = me->operand_stack[ lines( me->operand_stack ) ].
    DELETE me->operand_stack INDEX lines( me->operand_stack ).
  ENDMETHOD.

  METHOD pop_operator.
    CHECK NOT me->operator_stack IS INITIAL.
    operator = me->operator_stack[ lines( me->operator_stack ) ].
    DELETE me->operator_stack INDEX lines( me->operator_stack ).
  ENDMETHOD.

  METHOD is_operator_higher_priority.
    DATA: lv_new_priority TYPE i,
          lv_top_priority TYPE i.


    " 获取新运算符的优先级
    READ TABLE lt_operator_priorities WITH KEY operator = iv_new_operator INTO DATA(lv_new_priority_row).
    lv_new_priority = lv_new_priority_row-priority.

    " 获取栈顶运算符的优先级
    READ TABLE lt_operator_priorities WITH KEY operator = iv_top_operator INTO DATA(lv_top_priority_row).
    lv_top_priority = lv_top_priority_row-priority.

    rv_result = boolc( lv_new_priority > lv_top_priority ).
  ENDMETHOD.

  METHOD calculate.
    DATA: l_hsl_01 TYPE acdoca-hsl,
          l_hsl_02 TYPE acdoca-hsl.
    DATA: l_cursor_operator TYPE char1.

    l_hsl_02 = |{ pop_operand( ) }|.
    l_hsl_01 = |{ pop_operand( ) }|.
    l_cursor_operator = pop_operator( ).

    CASE l_cursor_operator.
      WHEN '+'.
        push_operand( l_hsl_01 + l_hsl_02 ).
      WHEN '-'.
        push_operand( l_hsl_01 - l_hsl_02 ).
      WHEN '*'.
        push_operand( l_hsl_01 * l_hsl_02 ).
      WHEN '/'.
        IF l_hsl_02 = 0.
          push_operand( 0 ).
        ELSE.
          push_operand( l_hsl_01 / l_hsl_02 ).
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.