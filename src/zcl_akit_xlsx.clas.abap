class ZCL_AKIT_XLSX definition
  public
  final
  create public .

public section.

  class-methods UPLOAD
    importing
      !CONTENT type XSTRING
      !SHEETNAME type STRING optional
      !SKIP_LINE type I optional
      !DATA type DATA optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AKIT_XLSX IMPLEMENTATION.


  METHOD upload.

    " 通过二进制方式的文件上载
    " 按 note 2468709 不建议使用 cl_fdt_xl_spreadsheet，在非 BRF+ 中使用不受 SAP 支持

    " 相关文档
    " https://stackoverflow.com/questions/74722765/using-cl-fdt-xl-spreadsheet-class-for-a-csv-possible

    DATA: lo_excel TYPE REF TO cl_fdt_xl_spreadsheet.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    " 初始化 Excel 对象
    TRY.
        lo_excel = NEW #(
          document_name = 'upload'
          xdocument     = content ).
      CATCH cx_fdt_excel_core INTO DATA(lr_core).
        RETURN.
    ENDTRY.

    CHECK lo_excel IS BOUND.

    lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
     IMPORTING
       worksheet_names = DATA(lt_sheet) ).

    CHECK lt_sheet IS NOT INITIAL.

    IF sheetname IS NOT INITIAL.
      READ TABLE lt_sheet INTO DATA(lv_sheet) WITH KEY table_line = sheetname.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ELSE.
      READ TABLE lt_sheet INTO lv_sheet INDEX 1.
    ENDIF.

    " 获取文件的二进制数据
    TRY.
        DATA(lo_data) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_sheet ).

        CHECK lo_data IS BOUND.

        ASSIGN lo_data->* TO <lt_table>.

        CHECK <lt_table> IS ASSIGNED.

        LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table>).
          " 跳过的行
          CHECK sy-tabix > skip_line.

          " 当前行写入

        ENDLOOP.

      CATCH cx_sy_conversion_error.
        RETURN.
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
