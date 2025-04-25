CLASS zcl_wx_oa_fc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wx_oa_fc.

    TYPES: BEGIN OF ty_node,
             type    TYPE i,
             apv_rel TYPE i,
             userid  TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
           END OF ty_node,
           tt_node_list TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY,
           BEGIN OF ty_summary_list,
             summary_info TYPE zif_wx_oa_fc=>tt_text,
           END OF ty_summary_list,
           tt_summary_list TYPE STANDARD TABLE OF ty_summary_list WITH DEFAULT KEY.


    DATA creator_userid TYPE string.
    DATA template_id TYPE string.
    DATA use_template_approver TYPE i.
    DATA choose_department TYPE i.
    DATA: BEGIN OF process,
            node_list TYPE tt_node_list,
          END OF process.
    DATA: BEGIN OF apply_data,
            contents TYPE TABLE OF REF TO zif_wx_oa_fc,
          END OF apply_data.
    DATA summary_list TYPE tt_summary_list.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WX_OA_FC IMPLEMENTATION.
ENDCLASS.
