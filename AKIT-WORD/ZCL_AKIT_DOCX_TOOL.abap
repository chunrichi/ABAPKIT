CLASS zcl_akit_docx_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA zip TYPE REF TO cl_abap_zip .

    METHODS load_file_smw0
      IMPORTING
        !name TYPE string.
    METHODS load_file_local
      IMPORTING
        !path TYPE string .
    METHODS store
      IMPORTING
        !path TYPE string OPTIONAL .
    METHODS wd2o
      IMPORTING
        !xstr TYPE xstring
      EXPORTING
        !odoc TYPE REF TO if_ixml_document
        !oxml TYPE REF TO if_ixml .
    METHODS o2wd
      IMPORTING
        !idoc TYPE REF TO if_ixml_document
        !ixml TYPE REF TO if_ixml
      EXPORTING
        !xstr TYPE xstring .
    METHODS process_expo
      IMPORTING
        !data        TYPE REF TO data
        !no_dev_node TYPE abap_bool DEFAULT abap_true .
    METHODS process_node
      IMPORTING
        !ixml_node TYPE REF TO if_ixml_node
        !curr_data TYPE REF TO data OPTIONAL .
  PROTECTED SECTION.
    DATA: namespace_w TYPE string VALUE `http://schemas.openxmlformats.org/wordprocessingml/2006/main`.
  PRIVATE SECTION.
    DATA: remove_dev_node TYPE abap_bool.
    DATA: skip_nodes TYPE TABLE OF string,
          skip_lines TYPE i.

    METHODS:
      process_sdt
        IMPORTING ixml_node    TYPE REF TO if_ixml_node
                  curr_data    TYPE REF TO data
        RETURNING VALUE(subrc) TYPE i.

    METHODS:
      get_tag_node_if_exits
        IMPORTING ixml_node       TYPE REF TO if_ixml_node
        RETURNING VALUE(tag_node) TYPE REF TO if_ixml_node.


    METHODS:
      find_subnode_by_name_ns
        IMPORTING pnode        TYPE REF TO if_ixml_node
                  name         TYPE string
                  namespace    TYPE string
        RETURNING VALUE(fnode) TYPE REF TO if_ixml_node,
      find_attribute_by_name_ns
        IMPORTING pnode        TYPE REF TO if_ixml_node
                  name         TYPE string
                  namespace    TYPE string
        RETURNING VALUE(fnode) TYPE REF TO if_ixml_node.


    METHODS: get_iterator_by_name_ns
      IMPORTING pnode           TYPE REF TO if_ixml_node
                name            TYPE string
                namespace       TYPE string
      RETURNING VALUE(iterator) TYPE REF TO if_ixml_node_iterator.

    DATA: ixml TYPE REF TO if_ixml,
          idoc TYPE REF TO if_ixml_document.
ENDCLASS.



CLASS ZCL_AKIT_DOCX_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_AKIT_DOCX_TOOL->FIND_ATTRIBUTE_BY_NAME_NS
* +-------------------------------------------------------------------------------------------------+
* | [--->] PNODE                          TYPE REF TO IF_IXML_NODE
* | [--->] NAME                           TYPE        STRING
* | [--->] NAMESPACE                      TYPE        STRING
* | [<-()] FNODE                          TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD find_attribute_by_name_ns.
    DATA(l_attrs) = pnode->get_attributes( ).

    fnode = l_attrs->get_named_item_ns( name = 'val'
                                        uri  = namespace ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_AKIT_DOCX_TOOL->FIND_SUBNODE_BY_NAME_NS
* +-------------------------------------------------------------------------------------------------+
* | [--->] PNODE                          TYPE REF TO IF_IXML_NODE
* | [--->] NAME                           TYPE        STRING
* | [--->] NAMESPACE                      TYPE        STRING
* | [<-()] FNODE                          TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD find_subnode_by_name_ns.
    DATA(l_filter) = pnode->create_filter_name_ns( name = name namespace = namespace ).

    DATA(l_iterator) = pnode->create_iterator_filtered( filter = l_filter ).

    fnode = l_iterator->get_next( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_AKIT_DOCX_TOOL->GET_ITERATOR_BY_NAME_NS
* +-------------------------------------------------------------------------------------------------+
* | [--->] PNODE                          TYPE REF TO IF_IXML_NODE
* | [--->] NAME                           TYPE        STRING
* | [--->] NAMESPACE                      TYPE        STRING
* | [<-()] ITERATOR                       TYPE REF TO IF_IXML_NODE_ITERATOR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_iterator_by_name_ns.
    DATA(l_filter) = pnode->create_filter_name_ns( name = name namespace = namespace ).

    iterator = pnode->create_iterator_filtered( filter = l_filter ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_AKIT_DOCX_TOOL->GET_TAG_NODE_IF_EXITS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXML_NODE                      TYPE REF TO IF_IXML_NODE
* | [<-()] TAG_NODE                       TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_tag_node_if_exits.
    DATA(l_sdtpr) = find_subnode_by_name_ns( pnode     = ixml_node
                                             name      = `sdtPr`
                                             namespace = me->namespace_w ).

    CHECK l_sdtpr IS NOT INITIAL.

    tag_node = find_subnode_by_name_ns( pnode     = ixml_node
                                        name      = `tag`
                                        namespace = me->namespace_w ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_AKIT_DOCX_TOOL->LOAD_FILE_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] PATH                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_file_local.
    " 本地加载文件

    DATA: lv_file_name    TYPE string,
          lt_file_data    TYPE solix_tab,
          lv_file_xstring TYPE xstring,
          lv_length       TYPE i.

    lv_file_name = path.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = lv_file_name
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_length
      TABLES
        data_tab                = lt_file_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lv_file_xstring = cl_bcs_convert=>solix_to_xstring(
      it_solix = lt_file_data
      iv_size  = lv_length
    ).


    me->zip = NEW #( ).

    me->zip->load( lv_file_xstring ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_AKIT_DOCX_TOOL->LOAD_FILE_SMW0
* +-------------------------------------------------------------------------------------------------+
* | [--->] NAME                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_file_smw0.
    " smw0 加载文件

    DATA: lt_mime         TYPE TABLE OF w3mime,
          lv_file_xstring TYPE xstring.

    DATA(ls_key) = VALUE wwwdatatab( relid = 'MI'
                                     objid = name ).

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key    = ls_key
      TABLES
        mime   = lt_mime
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        lv_file_xstring = cl_bcs_convert=>xtab_to_xstring( lt_mime ).
      CATCH cx_bcs.
        RETURN.
    ENDTRY.

    me->zip = NEW #( ).

    me->zip->load( lv_file_xstring ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_AKIT_DOCX_TOOL->O2WD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IDOC                           TYPE REF TO IF_IXML_DOCUMENT
* | [--->] IXML                           TYPE REF TO IF_IXML
* | [<---] XSTR                           TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD o2wd.
    " 对象转文档

    DATA(lr_ixml_sf) = ixml->create_stream_factory( ).
    DATA(lr_ostream) = lr_ixml_sf->create_ostream_xstring( xstr ).

    idoc->render( EXPORTING ostream = lr_ostream ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_AKIT_DOCX_TOOL->PROCESS_EXPO
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE REF TO DATA
* | [--->] NO_DEV_NODE                    TYPE        ABAP_BOOL (default =ABAP_TRUE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD process_expo.

    CHECK me->zip IS BOUND.  " 已完成文件上载

    remove_dev_node = no_dev_node. " COND #( WHEN no_dev_node = 'X' THEN '' ELSE 'X' ).

    DATA(l_files) = me->zip->files.

    LOOP AT l_files INTO DATA(l_file).

      CASE l_file-name(9).
        WHEN `word/docu` OR `word/foot` OR `word/head`.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      " 只处理 正文、头、脚
      me->zip->get( EXPORTING name    = l_file-name
                    IMPORTING content = DATA(l_docu_xstr) ).

      IF l_docu_xstr IS NOT INITIAL.
        me->wd2o( EXPORTING xstr = l_docu_xstr
                  IMPORTING oxml = DATA(l_docu_xml)
                            odoc = DATA(l_docu_doc) ).

        me->ixml = l_docu_xml.
        me->idoc = l_docu_doc.

        me->process_node( ixml_node = l_docu_doc->get_root( )
                          curr_data = data ).

        CLEAR l_docu_xstr.
        me->o2wd( EXPORTING idoc = l_docu_doc
                            ixml = l_docu_xml
                  IMPORTING xstr = l_docu_xstr ).

        me->zip->delete( name = l_file-name ).
        me->zip->add( name = l_file-name content = l_docu_xstr ).
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_AKIT_DOCX_TOOL->PROCESS_NODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXML_NODE                      TYPE REF TO IF_IXML_NODE
* | [--->] CURR_DATA                      TYPE REF TO DATA(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD process_node.
    DATA: l_bc_skip_lines TYPE i,
          l_currur_child  TYPE i VALUE 0.

    DATA(l_childs) = ixml_node->get_children( ).

    DO.
      DATA(l_childs_len) = l_childs->get_length( ).
      ADD 1 TO l_currur_child.

      IF l_childs_len < l_currur_child.
        EXIT.
      ENDIF.

      DATA(l_child) = l_childs->get_item( l_currur_child - 1 ).

      CHECK l_child IS BOUND.

      DATA(l_name) = l_child->get_name( ).

      CASE l_name.
        WHEN `sdt`.
          " template node
          l_bc_skip_lines = me->skip_lines.
          CLEAR me->skip_lines.
          DATA(l_subrc) = process_sdt( ixml_node = l_child curr_data = curr_data ).

          l_currur_child = l_currur_child + me->skip_lines.

          me->skip_lines = l_bc_skip_lines.

          IF me->remove_dev_node = abap_true
            AND ( l_subrc = 3   " 深层结构
             " OR L_SUBRC = 2   " 不存在 ABAP 对应字段
            ).

            DATA(l_content) = find_subnode_by_name_ns( pnode     = l_child
                                                       name      = `sdtContent`
                                                       namespace = me->namespace_w ).

            IF l_content IS BOUND.
              DATA(l_content_childs) = l_content->get_children( ).

              DO l_content_childs->get_length( ) TIMES.
                DATA(l_cc) = l_content_childs->get_item( sy-index - 1 ).

                ixml_node->insert_child( new_child = l_cc->clone( )
                                         ref_child = l_child ).
              ENDDO.

              ixml_node->remove_child( l_child ).
            ENDIF.

          ENDIF.

        WHEN OTHERS.
          READ TABLE me->skip_nodes TRANSPORTING NO FIELDS WITH KEY table_line = l_name BINARY SEARCH.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.

          " 递归处理子节点
          IF l_child->get_type( ) = if_ixml_node=>co_node_element.
            process_node( ixml_node = l_child curr_data = curr_data ).
          ENDIF.

      ENDCASE.
    ENDDO.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_AKIT_DOCX_TOOL->PROCESS_SDT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IXML_NODE                      TYPE REF TO IF_IXML_NODE
* | [--->] CURR_DATA                      TYPE REF TO DATA
* | [<-()] SUBRC                          TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD process_sdt.

    DATA(l_name) = ixml_node->get_name( ).

    DATA(l_tag) = me->get_tag_node_if_exits( ixml_node ).

    " 无标签内容跳过
    subrc = 1. " -------> 无 TAG <------
    CHECK l_tag IS NOT INITIAL.

    " 获取标签内容
    DATA(l_val) = me->find_attribute_by_name_ns( pnode     = l_tag
                                                 name      = `val`
                                                 namespace = me->namespace_w ).

    DATA(l_fieldname) = l_val->get_value( ).

    IF l_fieldname = `item`
      OR l_fieldname = 'ITEM'.
      BREAK-POINT.
    ENDIF.

    FIELD-SYMBOLS: <l_data> TYPE any,
                   <l_tabl> TYPE STANDARD TABLE.

    " 检查字段在 ABAP 变量是否存在
    ASSIGN curr_data->* TO <l_data>.
    CHECK sy-subrc = 0.

    subrc = 2. " -------> 无 对应 ABAP 字段 <------
    ASSIGN COMPONENT l_fieldname OF STRUCTURE <l_data> TO FIELD-SYMBOL(<l_field>).
    CHECK sy-subrc = 0.

    " 查找 CONTENT
    DATA(l_content) = find_subnode_by_name_ns( pnode     = ixml_node
                                               name      = `sdtContent`
                                               namespace = me->namespace_w ).

    CHECK l_content IS BOUND.

    " 父节点
    DATA(l_parent) = ixml_node->get_parent( ).

    " ABAP 类型
    DESCRIBE FIELD <l_field> TYPE DATA(l_field_type).
    CASE l_field_type.
      WHEN 'u' OR 'v'.

        " 进一步处理 =》 检查子节点

        " 判断 content 内是否还有 std
        process_node( ixml_node = l_content curr_data = REF #( <l_field> ) ).
        subrc = 3. " -------> 无 值的结构 <------

      WHEN 'h'.

        " 内表 =》 处理文本
        ASSIGN <l_field> TO <l_tabl>.

        DATA(l_item_sdt) = find_subnode_by_name_ns( pnode     = l_content
                                                    name      = `sdt`
                                                    namespace = me->namespace_w ).
        DATA(lt_skip_backup) = me->skip_nodes.
        me->skip_nodes = VALUE #( ( `sdtPr` ) ).

        LOOP AT <l_tabl> ASSIGNING FIELD-SYMBOL(<l_line>).
          me->skip_lines = me->skip_lines + 1.

          DATA(l_bc_node) = l_item_sdt->clone( ).
          process_node( ixml_node = l_bc_node curr_data = REF #( <l_line> ) ).

          " DATA(l_tr) = find_subnode_by_name_ns( pnode     = l_bc_node
          "                                       name      = `tr`
          "                                       namespace = me->namespace_w ).

          " l_parent->append_child( l_tr ).
          l_parent->insert_child( new_child = l_bc_node
                                  ref_child = ixml_node ).
        ENDLOOP.

        me->skip_nodes = lt_skip_backup.

        l_parent->remove_child( ixml_node ).
        me->skip_lines = me->skip_lines - 1.

        subrc = 4.
      WHEN OTHERS.

        " 替换处理

        " 替换文本内容
        DATA: l_index TYPE i.
        DATA(l_iterator) = get_iterator_by_name_ns( pnode     = l_content
                                                    name      = `t`
                                                    namespace = me->namespace_w ).

        DO.
          l_index = sy-index.
          DATA(l_text) = l_iterator->get_next( ).

          IF l_text IS NOT BOUND.
            EXIT.
          ENDIF.

          IF l_index = 1.
            l_text->set_value( <l_field> ).
          ELSE.
            " 如果有多个，清空其他的
            l_text->set_value( '' ).
          ENDIF.
        ENDDO.

        " 替换原始节点

        DATA(l_content_childs) = l_content->get_children( ).

        DO l_content_childs->get_length( ) TIMES.
          DATA(l_cc) = l_content_childs->get_item( sy-index - 1 ).

          l_parent->insert_child( new_child = l_cc->clone( )
                                  ref_child = ixml_node ).
        ENDDO.

        l_parent->remove_child( ixml_node ).

        subrc = 0.
    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_AKIT_DOCX_TOOL->STORE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PATH                           TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD store.

    DATA(l_document) = me->zip->save( ).

    DATA: lt_file   TYPE solix_tab,
          lv_length TYPE i,
          lv_path   TYPE string.

    lt_file = cl_bcs_convert=>xstring_to_solix( iv_xstring = l_document ).
    lv_length = xstrlen( l_document ).

    IF lv_path IS INITIAL.
      cl_gui_frontend_services=>get_desktop_directory( CHANGING desktop_directory = lv_path ).
      cl_gui_cfw=>flush( ).
      CONCATENATE lv_path '\report\report_tmp.docx' INTO lv_path.
    ENDIF.

    cl_gui_frontend_services=>gui_download( EXPORTING  bin_filesize            = lv_length
                                                       filename                = lv_path
                                                       filetype                = 'BIN'
                                            CHANGING   data_tab                = lt_file
                                            EXCEPTIONS file_write_error        = 1
                                                       no_batch                = 2
                                                       gui_refuse_filetransfer = 3
                                                       invalid_type            = 4
                                                       no_authority            = 5
                                                       unknown_error           = 6
                                                       header_not_allowed      = 7
                                                       separator_not_allowed   = 8
                                                       filesize_not_allowed    = 9
                                                       header_too_long         = 10
                                                       dp_error_create         = 11
                                                       dp_error_send           = 12
                                                       dp_error_write          = 13
                                                       unknown_dp_error        = 14
                                                       access_denied           = 15
                                                       dp_out_of_memory        = 16
                                                       disk_full               = 17
                                                       dp_timeout              = 18
                                                       file_not_found          = 19
                                                       dataprovider_exception  = 20
                                                       control_flush_error     = 21
                                                       not_supported_by_gui    = 22
                                                       error_no_gui            = 23
                                                       OTHERS                  = 24 ).

    IF sy-subrc = 0.
      cl_gui_frontend_services=>execute( document = lv_path ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_AKIT_DOCX_TOOL->WD2O
* +-------------------------------------------------------------------------------------------------+
* | [--->] XSTR                           TYPE        XSTRING
* | [<---] ODOC                           TYPE REF TO IF_IXML_DOCUMENT
* | [<---] OXML                           TYPE REF TO IF_IXML
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD wd2o.
    " 文档转对象

    oxml = cl_ixml=>create( ).
    odoc = oxml->create_document( ).

    DATA(lr_xml_sf) = oxml->create_stream_factory( ).
    DATA(lr_stream) = lr_xml_sf->create_istream_xstring( xstr ).

    DATA(lr_parser) = oxml->create_parser(
      stream_factory = lr_xml_sf
      istream        = lr_stream
      document       = odoc
    ).

    IF lr_parser->parse( ) NE 0.
      CLEAR odoc.
      RETURN.
    ENDIF.

    lr_stream->close( ).

  ENDMETHOD.
ENDCLASS.