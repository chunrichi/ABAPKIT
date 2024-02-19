CLASS zcl_akit_abap2xml DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA ixml TYPE REF TO if_ixml .
    DATA ixml_doc TYPE REF TO if_ixml_document .
    DATA oschema TYPE REF TO if_ixml_element.

    DATA pretty_name TYPE c.

    CLASS-METHODS serialize
      IMPORTING
        !data        TYPE data
        !pretty_name TYPE char1 DEFAULT ''
        !rootname    TYPE string DEFAULT 'root'
      RETURNING
        VALUE(xml)   TYPE string .
    METHODS _serialize
      IMPORTING
        !data        TYPE data
        !pretty_name TYPE char1 DEFAULT ''
        !rootname    TYPE string DEFAULT 'root'
      RETURNING
        VALUE(xml)   TYPE string .

    METHODS descrip_field
      IMPORTING
        !data      TYPE data
        !parent    TYPE REF TO if_ixml_element
        !fieldname TYPE string OPTIONAL.

    METHODS perety_name
      IMPORTING
                old        TYPE string
      RETURNING VALUE(new) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS readme .
ENDCLASS.



CLASS ZCL_AKIT_ABAP2XML IMPLEMENTATION.


  METHOD descrip_field.
    DATA: lr_struct TYPE REF TO cl_abap_structdescr,
          lr_type   TYPE REF TO cl_abap_typedescr.

    DATA: lr_element TYPE REF TO if_ixml_element.

    DATA: lv_fieldname TYPE string.

    FIELD-SYMBOLS: <l_data>  TYPE any,
                   <l_datas> TYPE STANDARD TABLE.

    DESCRIBE FIELD data TYPE DATA(l_type).

    CASE l_type.
      WHEN 'h'. " 表
        " 检查当前字段是否为空
        DATA(otable) = REF #( data ).
        ASSIGN otable->* TO <l_datas>.

        LOOP AT <l_datas> ASSIGNING FIELD-SYMBOL(<l_line>).
          DATA(lr_desc_type) = cl_abap_typedescr=>describe_by_data( <l_line> ).

          IF lr_desc_type->kind EQ cl_abap_typedescr=>kind_elem.
            " table_line（比较特殊正常不会如此创建）

            lr_element = me->ixml_doc->create_simple_element( name = `item` parent = parent ).
            descrip_field( data = <l_line> parent = lr_element ).
          ELSE.

            descrip_field( data = <l_line> parent = parent ).
          ENDIF.
        ENDLOOP.

      WHEN 'v' OR 'u'. " 深层结构 or 结构
        lr_struct ?= cl_abap_structdescr=>describe_by_data( data ).

        LOOP AT lr_struct->components INTO DATA(ls_comp).
          " 获取对应字段
          lv_fieldname = |DATA-{ ls_comp-name }|.

          ASSIGN (lv_fieldname) TO FIELD-SYMBOL(<lv_struct_field>).
          IF sy-subrc = 0 AND <lv_struct_field> IS ASSIGNED.

            lr_element = me->ixml_doc->create_simple_element( name = perety_name( |{ ls_comp-name }| ) parent = parent ).
            descrip_field( data = <lv_struct_field> parent = lr_element ).
          ENDIF.

        ENDLOOP.

      WHEN 'l'. " data refrence
        lv_fieldname = |DATA->*|.

        ASSIGN (lv_fieldname) TO FIELD-SYMBOL(<lv_ref_field>).
        IF sy-subrc = 0 AND <lv_ref_field> IS ASSIGNED.

          descrip_field( data = <lv_ref_field> parent = parent ).
        ENDIF.

      WHEN OTHERS.
        parent->set_value( |{ data }| ).
    ENDCASE.

  ENDMETHOD.


  METHOD perety_name.

    CASE pretty_name.
      WHEN 'L'.
        " LOW CASE 小写
        new = to_lower( old ).
      WHEN 'X'.
        " camel case
        new = |{ to_mixed( val = old case = 'A' ) }|.
      WHEN 'Y'.
        new = |{ to_mixed( val = old case = 'a' ) }|.
      WHEN OTHERS.
        " 转大写
        new = to_upper( old ).
    ENDCASE.
  ENDMETHOD.


  METHOD readme.

    " 由于 XML 的特性，此处只做简单的 ABAP2XML 的转换

  ENDMETHOD.


  METHOD serialize.

    " 将 data 转换为 xml
    DATA(lo_abap2xml) = NEW zcl_akit_abap2xml( ).

    xml = lo_abap2xml->_serialize( data = data
                            pretty_name = pretty_name
                            rootname    = rootname ).

  ENDMETHOD.


  METHOD _serialize.

    " 将 data 转换为 xml

    " 初始化
    me->ixml = cl_ixml=>create( ).
    me->ixml_doc = me->ixml->create_document( ).

    DATA(lr_encoding) = me->ixml->create_encoding(
      byte_order = if_ixml_encoding=>co_little_endian
      character_set = 'utf-8'
    ).
    me->ixml_doc->set_encoding( lr_encoding ).

    me->pretty_name = pretty_name.

    " 最外层节点
    oschema = me->ixml_doc->create_simple_element( name = rootname
                                                 parent = me->ixml_doc ).

    " 子节点解析
    descrip_field( data = data parent = oschema ).

    DATA(lr_ixml_osf) = me->ixml->create_stream_factory( ).
    DATA(lr_ostream) = lr_ixml_osf->create_ostream_cstring( xml ).

    CALL METHOD me->ixml_doc->render EXPORTING ostream = lr_ostream.
  ENDMETHOD.
ENDCLASS.