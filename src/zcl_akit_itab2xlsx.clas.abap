CLASS zcl_akit_itab2xlsx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS itab_to_xstring
      IMPORTING
        !ir_data_ref      TYPE REF TO data
        !it_fcat          TYPE lvc_t_fcat
      RETURNING
        VALUE(rv_xstring) TYPE xstring .
    METHODS xstring_to_xlsx
      IMPORTING
        !iv_xstring TYPE xstring
        !iv_path TYPE string RETURNING VALUE(rv_subrc) TYPE sysubrc.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AKIT_ITAB2XLSX IMPLEMENTATION.


  METHOD itab_to_xstring.
    DATA: l_length     TYPE i,
          l_xml_stream TYPE xml_rawdata,
          lv_flavour   TYPE string,
          lv_version   TYPE string.
    DATA: lr_result_data TYPE REF TO cl_salv_ex_result_data_table.

    TRY.

        lr_result_data = cl_salv_ex_util=>factory_result_data_table(
          r_data                      = ir_data_ref
          t_fieldcatalog              = it_fcat
        ).

        CASE cl_salv_bs_a_xml_base=>get_version( ).
          WHEN if_salv_bs_xml=>version_25.
            lv_version = if_salv_bs_xml=>version_25.
          WHEN if_salv_bs_xml=>version_26.
            lv_version = if_salv_bs_xml=>version_26. " = 2.6
        ENDCASE.


        lv_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export. "Flavor for Complete ALV XML

        CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
          EXPORTING
            xml_type      = if_salv_bs_xml=>c_type_xlsx  "XLSX
            xml_version   = lv_version
            r_result_data = lr_result_data
            xml_flavour   = lv_flavour
            gui_type      = if_salv_bs_xml=>c_gui_type_gui  "Y6DK066330
          IMPORTING
            xml           = rv_xstring.

      CATCH cx_root.
        CLEAR rv_xstring.
    ENDTRY.
  ENDMETHOD.


  METHOD xstring_to_xlsx.
    DATA: lv_size TYPE i.
    DATA: lt_bintab TYPE solix_tab.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = iv_xstring
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_bintab.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = lv_size
        filename                  = iv_path
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_bintab
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
           ).

    rv_subrc = sy-subrc.
  ENDMETHOD.
ENDCLASS.
