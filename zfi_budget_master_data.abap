*&---------------------------------------------------------------------*
*& Modulpool  ZFI_BUDG_MASTER_DATA
*& Created date    : 01.04.2021                                        *
*& Author's name   : Nirmal Kumar                                      *
*& Program title   : CAPEX Budgeting Master Home                       *                                
*& Purpose         : To Display CAPEX Budgeting Master Data            *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

PROGRAM zfi_budg_home_screen.


INCLUDE zfi_budg_home_screen_top.                " global Data

INCLUDE zfi_budg_home_screen_o01.                " PBO-Modules

INCLUDE zfi_budg_home_screen_i01.             " PAI-Modules

INCLUDE zfi_budg_home_screen_f01.               " FORM-Routines

*&---------------------------------------------------------------------*
*&  Include           ZFI_BUDG_MASTER_DATA_TOP
*&---------------------------------------------------------------------*

DATA : lv_flag1 TYPE char1,
       lv_flag2 TYPE char2,
       lv_flag3 TYPE char3.

CLASS lcl_event_handler DEFINITION DEFERRED.
DATA : g_tc_lines LIKE sy-loopc,                            "#EC NEEDED
       ok_code    LIKE sy-ucomm , ",  user command
       docking    TYPE REF TO cl_gui_docking_container,
       picture    TYPE REF TO cl_gui_picture,
       url        TYPE cndp_url,
       gt_dyn_fld TYPE TABLE OF dynpread ##NEEDED,
       gs_dyn_fld TYPE dynpread ##NEEDED,
       gv_hotspot TYPE char1.
CLASS lcl_picture DEFINITION DEFERRED.
TYPES :

  BEGIN OF ts_ref,
    cont     TYPE char50,
    pic      TYPE w3objid,
    tooltip  TYPE string,
    tcode    TYPE tcode,
    cont_obj TYPE REF TO cl_gui_custom_container,
    pic_obj  TYPE REF TO lcl_picture,
  END OF ts_ref.

DATA : gt_ref TYPE TABLE OF ts_ref,
       gs_ref TYPE ts_ref.

DATA : go_event_handler TYPE REF TO lcl_event_handler.
DATA : go_sel_pic TYPE REF TO lcl_picture.

CLASS lcl_picture DEFINITION INHERITING FROM cl_gui_picture.
  PUBLIC SECTION.
    METHODS dispatch REDEFINITION.
ENDCLASS.                    "lcl_picture DEFINITION

CLASS lcl_picture IMPLEMENTATION.
  METHOD dispatch.
    go_sel_pic = me.

    super->dispatch(
        EXPORTING
        cargo = cargo
        eventid = eventid
        is_shellevent = is_shellevent
        is_systemdispatch = is_systemdispatch ).
  ENDMETHOD.                    "dispatch
ENDCLASS.                    "lcl_picture DEFINITION

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS event_click
                  FOR EVENT control_click OF cl_gui_picture
      IMPORTING mouse_pos_x mouse_pos_y.
ENDCLASS.                    "lcl_event_handler DEFINITION

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD event_click.
    READ TABLE gt_ref INTO gs_ref WITH KEY pic_obj = go_sel_pic.
    IF sy-subrc EQ 0 AND gs_ref-cont = 'CONT2'.
      CALL SCREEN 9001.
    ELSEIF sy-subrc EQ 0 AND gs_ref-cont = 'CONT3'.
      CALL SCREEN 9002.
    ELSEIF sy-subrc EQ 0 AND gs_ref-cont = 'CONT4'.
      CALL SCREEN 9003.
    ENDIF.

    IF ( ( sy-subrc EQ 0 )  AND ( gs_ref-cont = 'CONT6' OR gs_ref-cont = 'CONT7' OR gs_ref-cont = 'CONT8'  ) ).
      IF lv_flag1 = 'X'.
        CLEAR : gs_ref-tcode.
      ENDIF.
      IF gs_ref-tcode IS NOT INITIAL.
        CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
          EXPORTING
            tcode  = gs_ref-tcode
          EXCEPTIONS
            ok     = 1
            not_ok = 2
            OTHERS = 3.
        IF sy-subrc <> 0.
          CASE sy-subrc.
            WHEN 1.
              CALL TRANSACTION gs_ref-tcode.
            WHEN OTHERS.
              MESSAGE 'You are not authorized' TYPE 'S' DISPLAY LIKE 'E'.
          ENDCASE.
        ENDIF.
        RETURN.
      ENDIF.
    ENDIF.

    IF ( ( sy-subrc EQ 0 )  AND ( gs_ref-cont = 'CONT10' OR gs_ref-cont = 'CONT11'
                                  OR gs_ref-cont = 'CONT12'  OR gs_ref-cont = 'CONT13'
                                  OR gs_ref-cont = 'CONT14'  OR gs_ref-cont = 'CONT15' OR gs_ref-cont = 'CONT16' ) ).
      IF lv_flag2 = 'X'.
        CLEAR : gs_ref-tcode.
      ENDIF.
      IF gs_ref-tcode IS NOT INITIAL.
        CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
          EXPORTING
            tcode  = gs_ref-tcode
          EXCEPTIONS
            ok     = 1
            not_ok = 2
            OTHERS = 3.
        IF sy-subrc <> 0.
          CASE sy-subrc.
            WHEN 1.
              CALL TRANSACTION gs_ref-tcode.
            WHEN OTHERS.
              MESSAGE 'You are not authorized' TYPE 'S' DISPLAY LIKE 'E'.
          ENDCASE.
        ENDIF.
        RETURN.
      ENDIF.
    ENDIF.

    IF ( ( sy-subrc EQ 0 )  AND ( gs_ref-cont = 'CONT18' OR gs_ref-cont = 'CONT19'
                                  OR gs_ref-cont = 'CONT20' OR gs_ref-cont = 'CONT21'
                                  OR gs_ref-cont = 'CONT22' OR gs_ref-cont = 'CONT23' ) ).
      IF lv_flag3 = 'X'.
        CLEAR : gs_ref-tcode.
      ENDIF.
      IF gs_ref-tcode IS NOT INITIAL.
        CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
          EXPORTING
            tcode  = gs_ref-tcode
          EXCEPTIONS
            ok     = 1
            not_ok = 2
            OTHERS = 3.
        IF sy-subrc <> 0.
          CASE sy-subrc.
            WHEN 1.
              CALL TRANSACTION gs_ref-tcode.
            WHEN OTHERS.
              MESSAGE 'You are not authorized' TYPE 'S' DISPLAY LIKE 'E'.
          ENDCASE.
        ENDIF.
        RETURN.
      ENDIF.
    ENDIF.


  ENDMETHOD.                    "event_click_specific
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Include           ZFI_BUDG_MASTER_DATA_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
*  SET PF-STATUS 'ZFI_BUDG_MASTER'.
*  SET TITLEBAR 'TITLE'.
  IF sy-ucomm EQ 'BACK'.
    IF docking IS NOT INITIAL.
      CALL METHOD docking->free.
      FREE docking.
    ENDIF.
  ENDIF.

  IF docking IS INITIAL.
    CREATE OBJECT docking
      EXPORTING
        repid                       = sy-repid
        dynnr                       = sy-dynnr
        side                        = docking->dock_at_top
        extension                   = 300
        ratio                       = 63
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT picture
      EXPORTING
        parent = docking.
    CHECK sy-subrc EQ 0.
    CALL METHOD picture->set_3d_border
      EXPORTING
        border = 1.

    CALL FUNCTION 'DP_PUBLISH_WWW_URL'
      EXPORTING
        objid                 = 'ZFI_BUDG_MAIN_IMG'
        lifetime              = 'T'
      IMPORTING
        url                   = url
      EXCEPTIONS
        dp_invalid_parameters = 1
        no_object             = 2
        dp_error_publish      = 3
        OTHERS                = 4.

    IF sy-subrc EQ 0.
      CALL METHOD picture->load_picture_from_url_async
        EXPORTING
          url = url.
    ENDIF.

*    P_CLNT = SY-MANDT.   "Commented By Srimani on 01-Sep-2014

  ENDIF.
  SET PF-STATUS 'ZFI_BUDG_MASTER'.
  SET TITLEBAR 'TITLE'.
  PERFORM create_containers.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'ZFI_BUDG_MASTER'.
  SET TITLEBAR 'TITLE1'.
  CLEAR : lv_flag1.
  PERFORM create_containers.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9002 OUTPUT.
  SET PF-STATUS 'ZFI_BUDG_MASTER'.
  SET TITLEBAR 'TITLE2'.
  CLEAR : lv_flag2.
  PERFORM create_containers.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9003 OUTPUT.
SET PF-STATUS 'ZFI_BUDG_MASTER'.
  SET TITLEBAR 'TITLE3'.
  CLEAR : lv_flag3.
  PERFORM create_containers.
ENDMODULE.

*&---------------------------------------------------------------------*
*&  Include           ZFI_BUDG_MASTER_DATA_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CLEAR :  sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE sy-ucomm.
      CLEAR : lv_flag1.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      lv_flag1 = 'X'.
      CLEAR :  sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.
  CASE sy-ucomm.
      CLEAR : lv_flag2.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      lv_flag2 = 'X'.
      CLEAR :  sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9003 INPUT.

  CASE sy-ucomm.
      CLEAR : lv_flag3.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      lv_flag3 = 'X'.
      CLEAR :  sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&  Include           ZFI_BUDG_MASTER_DATA_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_containers.

IF gt_ref IS INITIAL.
    CREATE OBJECT go_event_handler.
    REFRESH gt_ref.
    PERFORM create_cont USING:
*cont                 pic              tcode            TOOLTIP
*'CONT1     '   'ZFI_BUDG_MAIN_IMG'  'ABCD'         'Capital Budgeting',
'CONT2     '   'ZFI_BUDG_MASTER'    'ABCD'         'Budget Master',
'CONT3     '   'ZFI_BUDG_TRANS'     'ABCD'         'Budget Transactions',
'CONT4     '   'ZFI_BUDG_REPORTS'   'ABCD'         'Budget Reports',
'CONT5     '   'ZFI_BUDG_SIDE_IMG'  'ABCD'         'Capital Budgeting',
'CONT6     '   'ZFI_BUDG_ORG_ELEM'  'ZFI_MD01'     'Org.Element',
'CONT7     '   'ZFI_BUDG_PDK'       'ZFI_MD02'     'Program Domain',
'CONT8     '   'ZFI_BUDG_PAYMENT'   'ZFI_MD08'     'Payment Advice App. Level',
'CONT9     '   'ZFI_BUDG_SIDE_IMG'  'ABCD'         'Capital Budgeting',
'CONT10    '   'ZFI_BUDG_PLAN'      'ZFI_TR02'     'Budget Planning',
'CONT11    '   'ZFI_BUDG_CALEN'     'ZFI_TR05'     'Budget Calendarization',
'CONT12    '   'ZFI_BUDG_APP'       'ZFI_TR06'     'Budget Approval',
'CONT13    '   'ZFI_APP_BUDG  '     'ZFI_TR07'     'Approved Budget Calendarization',
'CONT14    '   'ZFI_PO_CF_FORECAST' 'ZFI_TR08'     'PO Cashflow Forecast',
'CONT15    '   'ZFI_PAY_ADC_CRT'    'ZFI_TR09'     'Payment Advice Create',
'CONT16    '   'ZFI_PAY_ADC_APP'    'ZFI_TR10'     'Payment Advice Approval',
'CONT17    '   'ZFI_BUDG_SIDE_IMG'  'ABCD'         'Capital Budgeting',
'CONT18    '   'ZFI_BUDG_PLAN_RPT'  'ZFI_RP01'     'CAPEX Budget Planning',
'CONT19    '   'ZFI_BUDG_CPX_ALY'   'ZFI_RP02'     'CAPEX Analysis Report Program',
'CONT20    '   'ZFI_BUDG_CPX_ORG'   'ZFI_RP03'     'CAPEX Analysis Report Org.Element',
'CONT21    '   'ZFI_CAPEX_TOOL'     'ZFI_RP04'     'CAPEX-Tool Spend Simulator',
'CONT22    '   'ZFI_CAPEX_SEPND_RP' 'ZFI_RP05'     'CAPEX-Spend analysis Report',
'CONT23    '   'ZFI_CAPEX_DET_RPT'  'ZFI_RP06'     'CAPEX-Detailed Report'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CREATE_CONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0015   text
*      -->P_0016   text
*      -->P_0017   text
*      -->P_0018   text
*----------------------------------------------------------------------*
FORM create_cont  USING    value(pv_cont)
                           value(pv_pic)
                           value(pv_tcode)
                           value(pv_tooltip).

   DATA : lv_event TYPE cntl_simple_event,
         lt_event TYPE cntl_simple_events.

  CLEAR : gs_ref.

  gs_ref-cont    = pv_cont.
  gs_ref-pic     = pv_pic.
  gs_ref-tcode   = pv_tcode.
  gs_ref-tooltip = pv_tooltip.

  CREATE OBJECT gs_ref-cont_obj
    EXPORTING
      container_name = gs_ref-cont.



  CREATE OBJECT gs_ref-pic_obj
    EXPORTING
      parent = gs_ref-cont_obj.

  CLEAR lt_event.
  lv_event-eventid = cl_gui_picture=>eventid_control_click.
  APPEND lv_event TO lt_event.
  CALL METHOD gs_ref-pic_obj->set_registered_events
    EXPORTING
      events = lt_event.

  CALL METHOD gs_ref-pic_obj->set_display_mode
    EXPORTING
      display_mode = cl_gui_picture=>display_mode_normal_center.


  DATA : lv_img TYPE w3objid,
         lv_url TYPE cndp_url.

  CLEAR : lv_img.
  lv_img = gs_ref-pic.

  CALL FUNCTION 'DP_PUBLISH_WWW_URL'
    EXPORTING
      objid    = lv_img
      lifetime = cndp_lifetime_transaction
    IMPORTING
      url      = lv_url
    EXCEPTIONS
      OTHERS   = 1.

  IF sy-subrc = 0.
    CALL METHOD gs_ref-pic_obj->load_picture_from_url_async
      EXPORTING
        url = lv_url.
  ENDIF.

**     Method To Set Display Mode Fit to Container
  CALL METHOD gs_ref-pic_obj->set_display_mode
    EXPORTING
      display_mode = cl_gui_picture=>display_mode_fit.


  gs_ref-pic_obj->set_tooltip(
    EXPORTING
      text  =  gs_ref-tooltip ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SET HANDLER:
    go_event_handler->event_click      FOR gs_ref-pic_obj.

  APPEND gs_ref TO gt_ref.
  CLEAR gs_ref.
ENDFORM.
