*&---------------------------------------------------------------------*
*& Report  ZHR_TRAVEL_HEAD_INTL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zhr_travel_head_intl.


INCLUDE zhr_travel_head_intl_sel.

INCLUDE zhr_travel_head_intl_sub.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_pernr-low.

  PERFORM pernr_f4 CHANGING s_pernr-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_pernr-high.

  PERFORM pernr_f4 CHANGING s_pernr-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_reinr-low.

  PERFORM reinr_f4 CHANGING s_reinr-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_reinr-high.

  PERFORM reinr_f4 CHANGING s_reinr-high.

FORM pernr_f4 CHANGING cv_pernr TYPE ztrip_head_intl-pernr.

  TYPES : BEGIN OF ty_pernr,
            pernr TYPE ztrip_head_intl-pernr,
          END OF ty_pernr.

  DATA: lt_values TYPE STANDARD TABLE OF ty_pernr,
        lt_return TYPE STANDARD TABLE OF ddshretval.

  SELECT DISTINCT pernr
     FROM ztrip_head_intl
     INTO TABLE lt_values.
  IF sy-subrc EQ 0.
    SORT lt_values BY pernr.
  ENDIF.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PERNR'
      value_org       = 'S'
    TABLES
      value_tab       = lt_values
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  cv_pernr = ls_return-fieldval.

ENDFORM.

FORM reinr_f4 CHANGING cv_reinr TYPE ztrip_head_intl-reinr.

  TYPES : BEGIN OF ty_reinr,
            reinr TYPE ztrip_head_intl-reinr,
          END OF ty_reinr.

  DATA: lt_values TYPE STANDARD TABLE OF ty_reinr,
        lt_return TYPE STANDARD TABLE OF ddshretval.

  DATA : lt_dynfields TYPE STANDARD TABLE OF dynpread,
         ls_dynfields TYPE dynpread.

  ls_dynfields-fieldname = 'S_PERNR-LOW'.
  APPEND ls_dynfields TO lt_dynfields.

  ls_dynfields-fieldname = 'S_PERNR-HIGH'.
  APPEND ls_dynfields TO lt_dynfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynfields.

  READ TABLE lt_dynfields INTO ls_dynfields WITH KEY fieldname = 'S_PERNR-LOW'.
  IF sy-subrc EQ 0.
    s_pernr-low = ls_dynfields-fieldvalue.
  ENDIF.

  READ TABLE lt_dynfields INTO ls_dynfields WITH KEY fieldname = 'S_PERNR-HIGH'.
  IF sy-subrc EQ 0.
    s_pernr-high = ls_dynfields-fieldvalue.
  ENDIF.


  IF s_pernr-high EQ '000000'.
    s_pernr-sign = 'I'.
    s_pernr-option = 'EQ'.
    APPEND s_pernr.
  ELSE.
    s_pernr-sign = 'I'.
    s_pernr-option = 'BT'.
    APPEND s_pernr.
  ENDIF.

  SELECT DISTINCT reinr
         FROM ztrip_head_intl
         INTO TABLE lt_values
         WHERE pernr IN s_pernr.

  CLEAR : ls_dynfields.
  REFRESH : lt_dynfields.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'REINR'
      value_org       = 'S'
    TABLES
      value_tab       = lt_values
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  cv_reinr = ls_return-fieldval.

ENDFORM.

*&---------------------------------------------------------------------*
*&
                  INCLUDE zhr_travel_head_intl_sel.
*&
*&---------------------------------------------------------------------*

DATA : lv_pernr TYPE ztrip_head_intl-pernr,
       lv_reinr TYPE ztrip_head_intl-reinr.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS : s_pernr FOR lv_pernr NO-EXTENSION OBLIGATORY,
                 s_reinr FOR lv_reinr NO-EXTENSION OBLIGATORY.

SELECTION-SCREEN : END OF BLOCK b1.

*&---------------------------------------------------------------------*
*&
                  INCLUDE zhr_travel_head_intl_sub.
*&
*&---------------------------------------------------------------------*

CLASS lcl_travel_head_intl DEFINITION .

  PUBLIC SECTION.

    TYPES : BEGIN OF ty_travel,
              mandt        TYPE ztrip_head_intl-mandt,
              pernr        TYPE ztrip_head_intl-pernr,
              reinr        TYPE ztrip_head_intl-reinr,
              ztr_type     TYPE ztrip_head_intl-ztr_type,
              status       TYPE ztrip_head_intl-status,
              l1_appr      TYPE ztrip_head_intl-l1_appr,
              l1_status    TYPE ztrip_head_intl-l1_status,
              l2_appr      TYPE ztrip_head_intl-l2_appr,
              l2_status    TYPE ztrip_head_intl-l2_status,
              passport_no  TYPE ztrip_head_intl-passport_no,
              pass_valdate TYPE ztrip_head_intl-pass_valdate,
              rem_altd     TYPE ztrip_head_intl-rem_altd,
              adv_amt      TYPE ztrip_head_intl-adv_amt,
              adv_cur      TYPE ztrip_head_intl-adv_cur,
              phone_no     TYPE ztrip_head_intl-phone_no,
              fname        TYPE ztrip_head_intl-fname,
              mname        TYPE ztrip_head_intl-mname,
              lname        TYPE ztrip_head_intl-lname,
              estimat_amt  TYPE ztrip_head_intl-estimat_amt,
              estimat_curr TYPE ztrip_head_intl-estimat_curr,
              fcountry     TYPE ztrip_head_intl-fcountry,
              tcountry     TYPE ztrip_head_intl-tcountry,
              id_type      TYPE ztrip_head_intl-id_type,
              id_proof     TYPE ztrip_head_intl-id_proof,
              net_exp_amt  TYPE ztrip_head_intl-net_exp_amt,
              adv_amt_usd  TYPE ztrip_head_intl-adv_amt_usd,
              due_amt_cmp  TYPE ztrip_head_intl-due_amt_cmp,
              due_amt_emp  TYPE ztrip_head_intl-due_amt_emp,
              ernam        TYPE ztrip_head_intl-ernam,
              erdat        TYPE ztrip_head_intl-erdat,
              ztime_creat  TYPE ztrip_head_intl-ztime_creat,
              aenam        TYPE ztrip_head_intl-aenam,
              aedat        TYPE ztrip_head_intl-aedat,
              ztime_chan   TYPE ztrip_head_intl-ztime_chan,
              dept_budget  TYPE ztrip_head_intl-dept_budget,
              act_budget   TYPE ztrip_head_intl-act_budget,
              esti_cost    TYPE ztrip_head_intl-esti_cost,
              air          TYPE ztrip_head_intl-air,
              train        TYPE ztrip_head_intl-train,
              car          TYPE ztrip_head_intl-car,
              bus          TYPE ztrip_head_intl-bus,
              aljeep       TYPE ztrip_head_intl-aljeep,
              prediem      TYPE ztrip_head_intl-prediem,
              hotel        TYPE ztrip_head_intl-hotel,
              perdiem_days TYPE ztrip_head_intl-perdiem_days,
              hotel_days   TYPE ztrip_head_intl-hotel_days,
              fb           TYPE ztrip_head_intl-fb,
              other        TYPE ztrip_head_intl-other,
              wbs          TYPE ztrip_head_intl-wbs,
              stay_flag    TYPE ztrip_head_intl-stay_flag,
              appr_by      TYPE ztrip_head_intl-appr_by,
              appr_date    TYPE ztrip_head_intl-appr_date,
              appr_time    TYPE ztrip_head_intl-appr_time,
            END OF ty_travel.


    TYPES : BEGIN OF ty_travel_new,
              check        TYPE sap_bool,
              mandt        TYPE ztrip_head_intl-mandt,
              pernr        TYPE ztrip_head_intl-pernr,
              reinr        TYPE ztrip_head_intl-reinr,
              ztr_type     TYPE ztrip_head_intl-ztr_type,
              status       TYPE ztrip_head_intl-status,
              l1_appr      TYPE ztrip_head_intl-l1_appr,
              l1_status    TYPE ztrip_head_intl-l1_status,
              l2_appr      TYPE ztrip_head_intl-l2_appr,
              l2_status    TYPE ztrip_head_intl-l2_status,
              passport_no  TYPE ztrip_head_intl-passport_no,
              pass_valdate TYPE ztrip_head_intl-pass_valdate,
              rem_altd     TYPE ztrip_head_intl-rem_altd,
              adv_amt      TYPE ztrip_head_intl-adv_amt,
              adv_cur      TYPE ztrip_head_intl-adv_cur,
              phone_no     TYPE ztrip_head_intl-phone_no,
              fname        TYPE ztrip_head_intl-fname,
              mname        TYPE ztrip_head_intl-mname,
              lname        TYPE ztrip_head_intl-lname,
              estimat_amt  TYPE ztrip_head_intl-estimat_amt,
              estimat_curr TYPE ztrip_head_intl-estimat_curr,
              fcountry     TYPE ztrip_head_intl-fcountry,
              tcountry     TYPE ztrip_head_intl-tcountry,
              id_type      TYPE ztrip_head_intl-id_type,
              id_proof     TYPE ztrip_head_intl-id_proof,
              net_exp_amt  TYPE ztrip_head_intl-net_exp_amt,
              adv_amt_usd  TYPE ztrip_head_intl-adv_amt_usd,
              due_amt_cmp  TYPE ztrip_head_intl-due_amt_cmp,
              due_amt_emp  TYPE ztrip_head_intl-due_amt_emp,
              ernam        TYPE ztrip_head_intl-ernam,
              erdat        TYPE ztrip_head_intl-erdat,
              ztime_creat  TYPE ztrip_head_intl-ztime_creat,
              aenam        TYPE ztrip_head_intl-aenam,
              aedat        TYPE ztrip_head_intl-aedat,
              ztime_chan   TYPE ztrip_head_intl-ztime_chan,
              dept_budget  TYPE ztrip_head_intl-dept_budget,
              act_budget   TYPE ztrip_head_intl-act_budget,
              esti_cost    TYPE ztrip_head_intl-esti_cost,
              air          TYPE ztrip_head_intl-air,
              train        TYPE ztrip_head_intl-train,
              car          TYPE ztrip_head_intl-car,
              bus          TYPE ztrip_head_intl-bus,
              aljeep       TYPE ztrip_head_intl-aljeep,
              prediem      TYPE ztrip_head_intl-prediem,
              hotel        TYPE ztrip_head_intl-hotel,
              perdiem_days TYPE ztrip_head_intl-perdiem_days,
              hotel_days   TYPE ztrip_head_intl-hotel_days,
              fb           TYPE ztrip_head_intl-fb,
              other        TYPE ztrip_head_intl-other,
              wbs          TYPE ztrip_head_intl-wbs,
              stay_flag    TYPE ztrip_head_intl-stay_flag,
              appr_by      TYPE ztrip_head_intl-appr_by,
              appr_date    TYPE ztrip_head_intl-appr_date,
              appr_time    TYPE ztrip_head_intl-appr_time,
            END OF ty_travel_new.

    DATA : lt_travel TYPE STANDARD TABLE OF ty_travel,
           ls_travel TYPE ty_travel,
           gt_travel TYPE STANDARD TABLE OF ty_travel_new,
           gs_travel TYPE ty_travel_new.


    DATA : o_alv TYPE REF TO cl_salv_table.

    METHODS : get_data,
      display_editable_alv,
      set_pf_status CHANGING co_alv TYPE REF TO cl_salv_table.

  PROTECTED SECTION.

  PRIVATE SECTION.


ENDCLASS.

CLASS cl_check DEFINITION.
  PUBLIC SECTION.

    DATA : cont1           TYPE REF TO cl_gui_custom_container,
           go_grid         TYPE REF TO cl_gui_alv_grid,
           gv_edit_flag(1) TYPE c VALUE 'X'.

    METHODS : on_check FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      handle_events FOR EVENT added_function OF cl_salv_events  IMPORTING e_salv_function.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_salv_editable DEFINITION
*----------------------------------------------------------------------*

CLASS lcl_salv_editable DEFINITION INHERITING FROM cl_salv_controller CREATE PRIVATE FINAL.

  PUBLIC SECTION.

    CLASS-METHODS: get_control_rtti IMPORTING i_salv         TYPE REF TO cl_salv_model_base
                                    EXPORTING e_adapter_type TYPE salv_de_adapter_type
                                              e_control_rtti TYPE REF TO cl_abap_typedescr,


      get_control IMPORTING i_salv           TYPE REF TO cl_salv_model_base
                  RETURNING VALUE(r_control) TYPE REF TO object,


      set_editable IMPORTING VALUE(i_fieldname) TYPE csequence OPTIONAL
                             i_salv_table       TYPE REF TO cl_salv_table
                             VALUE(i_editable)  TYPE abap_bool DEFAULT abap_true
                             VALUE(i_refresh)   TYPE abap_bool DEFAULT abap_true.

  PRIVATE SECTION.


    CLASS-METHODS: get_control_internal IMPORTING i_salv         TYPE REF TO cl_salv_model_base
                                        EXPORTING e_adapter_type TYPE salv_de_adapter_type
                                                  e_control      TYPE REF TO object.

ENDCLASS.


START-OF-SELECTION.

  DATA : travel_head TYPE REF TO lcl_travel_head_intl.

  CREATE OBJECT travel_head.

  travel_head->get_data( ).


CLASS cl_check IMPLEMENTATION.

  METHOD on_check.

    FIELD-SYMBOLS : <fs_data> LIKE LINE OF travel_head->gt_travel.
    READ TABLE travel_head->gt_travel ASSIGNING <fs_data> INDEX row.
    CHECK sy-subrc IS INITIAL.
    IF <fs_data>-check IS INITIAL.
      <fs_data>-check = 'X'.
    ELSE.
      CLEAR <fs_data>-check.
    ENDIF.
    travel_head->o_alv->refresh( ).

  ENDMETHOD.

  METHOD handle_events.

    CASE e_salv_function.

      WHEN 'SEL_ALL'.

        LOOP AT travel_head->gt_travel ASSIGNING FIELD-SYMBOL(<fs_sel>).
          CHECK sy-subrc IS INITIAL.
          IF <fs_sel>-check IS INITIAL.
            <fs_sel>-check = 'X'.
          ENDIF.
        ENDLOOP.

        travel_head->o_alv->refresh( ).

      WHEN 'DSEL_ALL'.

        LOOP AT travel_head->gt_travel ASSIGNING FIELD-SYMBOL(<fs_dsel>).
          CHECK sy-subrc IS INITIAL.
          IF <fs_dsel>-check IS NOT INITIAL.
            <fs_dsel>-check = ' '.
          ENDIF.
        ENDLOOP.

        travel_head->o_alv->refresh( ).

      WHEN '&SAVE'.

        DATA : gt_save TYPE STANDARD TABLE OF ztrip_head_intl,
               gs_save TYPE ztrip_head_intl.

        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
          IMPORTING
            e_grid = go_grid.
        IF NOT go_grid IS INITIAL.
          CALL METHOD go_grid->check_changed_data .
        ENDIF.

        LOOP AT travel_head->gt_travel ASSIGNING FIELD-SYMBOL(<fs_save>) WHERE check EQ 'X'.
          CHECK sy-subrc IS INITIAL.
          MOVE-CORRESPONDING <fs_save> TO gs_save.
          APPEND gs_save TO gt_save.
        ENDLOOP.

        MODIFY ztrip_head_intl FROM TABLE gt_save.
        IF sy-subrc EQ 0.
          MESSAGE i000(zhr) WITH text-003.
          lcl_salv_editable=>set_editable( i_salv_table = travel_head->o_alv
                                           i_editable   = abap_false ).
          travel_head->o_alv->refresh( ).
        ENDIF.


      WHEN '&CREATE'.

        DATA : counter   TYPE i VALUE 0,
               gt_create TYPE STANDARD TABLE OF travel_head->ty_travel_new,
               gs_create TYPE travel_head->ty_travel_new.

        LOOP AT travel_head->gt_travel ASSIGNING FIELD-SYMBOL(<fs_create>) WHERE check EQ 'X'.
          CHECK sy-subrc IS INITIAL.
          counter = counter + 1.
          IF counter GT 1.
            EXIT.
          ELSE.
            gs_create-check = 'X'.
            gs_create-pernr = <fs_create>-pernr.
            gs_create-reinr = <fs_create>-reinr.
            INSERT gs_create INTO travel_head->gt_travel.
          ENDIF.
        ENDLOOP.

        travel_head->o_alv->refresh( ).


      WHEN '&DELETE'.

        DATA : gt_delete TYPE STANDARD TABLE OF ztrip_head_intl,
               gs_delete TYPE ztrip_head_intl.

        LOOP AT travel_head->gt_travel ASSIGNING FIELD-SYMBOL(<fs_del>) WHERE check EQ 'X'.
          CHECK sy-subrc IS INITIAL.
          MOVE-CORRESPONDING <fs_del> TO gs_delete.
          APPEND gs_delete TO gt_delete.
        ENDLOOP.

        DELETE travel_head->gt_travel WHERE check = 'X'.
        DELETE ztrip_head_intl FROM TABLE gt_delete.
        IF sy-subrc EQ 0.
          MESSAGE i000(zhr) WITH text-004.
          travel_head->o_alv->refresh( ).
        ENDIF.



      WHEN '&EDIT'.

        IF gv_edit_flag IS NOT  INITIAL.
          lcl_salv_editable=>set_editable( i_salv_table = travel_head->o_alv ).
          CLEAR gv_edit_flag.
        ELSE.
          lcl_salv_editable=>set_editable( i_salv_table = travel_head->o_alv
                                           i_editable   = abap_false ).
          gv_edit_flag = 'X'.
        ENDIF.

       WHEN 'EXIT'.

         LEAVE TO SCREEN 0.
         EXIT.



    ENDCASE.


  ENDMETHOD.

ENDCLASS.


CLASS lcl_travel_head_intl IMPLEMENTATION.

  METHOD get_data.

    SELECT *
           FROM ztrip_head_intl
           INTO TABLE lt_travel
           WHERE pernr IN s_pernr
             AND reinr IN s_reinr.

    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING lt_travel TO gt_travel.
      SORT lt_travel BY pernr reinr.
      display_editable_alv( ).
    ELSE.
      MESSAGE e000(zhr) WITH text-002.
      LEAVE LIST-PROCESSING.
    ENDIF.


  ENDMETHOD.

  METHOD display_editable_alv.

    DATA : lc_msg     TYPE REF TO cx_salv_msg,
           lo_columns TYPE REF TO cl_salv_columns,
           lo_column  TYPE REF TO cl_salv_column_table,
           not_found  TYPE REF TO cx_salv_not_found,
           lo_display TYPE REF TO cl_salv_display_settings,
           lo_event   TYPE REF TO cl_salv_events_table,
           ls_key     TYPE salv_s_layout_key.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = o_alv
          CHANGING
            t_table      = gt_travel.


        lo_columns = o_alv->get_columns( ).
        lo_columns->set_optimize( 'X' ).
        TRY.
            lo_column ?= lo_columns->get_column( 'CHECK' ).
            lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
            lo_column->set_output_length( 7 ).

            lo_column ?= lo_columns->get_column( 'ZTR_TYPE' ).
            lo_column->set_output_length( 20 ).

            lo_column ?= lo_columns->get_column( 'L1_STATUS' ).
            lo_column->set_long_text( 'L1 Approved Flag' ).

            lo_column ?= lo_columns->get_column( 'L2_STATUS' ).
            lo_column->set_long_text( 'L2 Approved Flag' ).

            lo_column ?= lo_columns->get_column( 'REM_ALTD' ).
            lo_column->set_long_text( 'ALTD Remarks For Trip' ).

            lo_column ?= lo_columns->get_column( 'PHONE_NO' ).
            lo_column->set_long_text( 'Phone Number' ).

            lo_column ?= lo_columns->get_column( 'FNAME' ).
            lo_column->set_long_text( 'First Name in Passport' ).

            lo_column ?= lo_columns->get_column( 'MNAME' ).
            lo_column->set_long_text( 'Middle Name in Passport' ).

            lo_column ?= lo_columns->get_column( 'LNAME' ).
            lo_column->set_long_text( 'Last Name in Passport' ).

            lo_column ?= lo_columns->get_column( 'ID_TYPE' ).
            lo_column->set_long_text( 'ID Type' ).

            lo_column ?= lo_columns->get_column( 'ID_PROOF' ).
            lo_column->set_long_text( 'ID Proof' ).

            lo_column ?= lo_columns->get_column( 'ZTIME_CREAT' ).
            lo_column->set_long_text( 'Created Time' ).

            lo_column ?= lo_columns->get_column( 'ZTIME_CHAN' ).
            lo_column->set_long_text( 'Changed Time' ).

            lo_column ?= lo_columns->get_column( 'PERDIEM_DAYS' ).
            lo_column->set_long_text( 'Prediem days' ).

            lo_column ?= lo_columns->get_column( 'HOTEL_DAYS' ).
            lo_column->set_long_text( 'Hotel days' ).

            lo_column ?= lo_columns->get_column( 'STAY_FLAG' ).
            lo_column->set_long_text( 'Stay Flag' ).

        ENDTRY.
      CATCH cx_salv_msg INTO lc_msg.
    ENDTRY.

    lo_event = o_alv->get_event( ).

    DATA : lo_check TYPE REF TO cl_check.
    CREATE OBJECT lo_check.
    SET HANDLER lo_check->on_check FOR lo_event.
    SET HANDLER lo_check->handle_events FOR lo_event.

    CALL METHOD set_pf_status
      CHANGING
        co_alv = o_alv.

    IF gt_travel IS NOT INITIAL.

      o_alv->display( ).
    ELSE.
      MESSAGE i000(zhr) WITH text-005.
      LEAVE LIST-PROCESSING.
    ENDIF.


  ENDMETHOD.

  METHOD set_pf_status.

    co_alv->set_screen_status( pfstatus = 'STATUS'
                               report   = sy-repid
                               set_functions = co_alv->c_functions_all ).

  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS lcl_salv_editable IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_salv_editable IMPLEMENTATION.

  METHOD get_control_internal.

    DATA: lo_controller            TYPE REF TO cl_salv_controller_model,
          lo_adapter               TYPE REF TO cl_salv_adapter,
          lo_fullscreen_adapter    TYPE REF TO cl_salv_fullscreen_adapter,
          lo_grid_adapter          TYPE REF TO cl_salv_grid_adapter,
          lo_table_display_adapter TYPE REF TO if_salv_table_display_adapter,
          lo_tree_adapter_base     TYPE REF TO cl_salv_tree_adapter_base.

    CHECK e_adapter_type IS REQUESTED OR
          e_control      IS REQUESTED.

    IF  e_adapter_type IS REQUESTED.
      CLEAR e_adapter_type.
    ENDIF.

    IF  e_control IS REQUESTED.
      CLEAR e_control.
    ENDIF.

    lo_controller = i_salv->r_controller.
    CHECK lo_controller IS BOUND.

    lo_adapter = lo_controller->r_adapter.
    CHECK lo_adapter IS BOUND.

    IF e_adapter_type IS REQUESTED.
      e_adapter_type = lo_adapter->type.
    ENDIF.

    CHECK e_control IS REQUESTED.

    CASE lo_adapter->type.
      WHEN lo_adapter->if_salv_adapter~c_adapter_type_fullscreen.
        lo_fullscreen_adapter ?= lo_adapter.
        e_control = lo_fullscreen_adapter->get_grid( ).

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_grid.
        lo_grid_adapter ?= lo_adapter.
        e_control = lo_grid_adapter->get_grid( ).

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_hierseq.

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_list.
        lo_table_display_adapter ?= lo_adapter.
        e_control = lo_table_display_adapter->r_table.

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_tree.
        lo_tree_adapter_base ?= lo_adapter.
        e_control = lo_tree_adapter_base->r_tree.

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_append.

    ENDCASE.

  ENDMETHOD.                    "get_control_internal

  METHOD get_control_rtti.

    DATA: lv_adapter_type TYPE salv_de_adapter_type,
          lo_control      TYPE REF TO object.

    CHECK e_adapter_type IS REQUESTED OR
          e_control_rtti IS REQUESTED.

    IF  e_adapter_type IS REQUESTED.
      CLEAR e_adapter_type.
    ENDIF.

    IF  e_control_rtti IS REQUESTED.
      CLEAR e_control_rtti.
    ENDIF.

    get_control_internal( EXPORTING i_salv = i_salv IMPORTING e_adapter_type = lv_adapter_type e_control = lo_control ).

    IF e_adapter_type IS REQUESTED.
      e_adapter_type = lv_adapter_type.
    ENDIF.

    IF e_control_rtti IS REQUESTED.
      e_control_rtti = cl_abap_typedescr=>describe_by_object_ref( lo_control ).
    ENDIF.

  ENDMETHOD.                    "get_control_rtti

  METHOD get_control.

    CHECK r_control IS REQUESTED.

    get_control_internal( EXPORTING i_salv = i_salv IMPORTING e_control = r_control ).

  ENDMETHOD.                    "get_control

  METHOD set_editable.
    CONSTANTS: lc_stable TYPE lvc_s_stbl VALUE 'XX'.

    DATA: lo_grid     TYPE REF TO cl_gui_alv_grid,
          lt_fieldcat TYPE lvc_t_fcat,
          ls_layout   TYPE lvc_s_layo.

    FIELD-SYMBOLS: <fs_fieldcat> LIKE LINE OF lt_fieldcat.

    lo_grid ?= get_control( i_salv_table ).
    CHECK lo_grid IS BOUND.

    IF i_fieldname IS SUPPLIED AND
       i_fieldname IS NOT INITIAL.
      lo_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fieldcat ).
      READ TABLE lt_fieldcat ASSIGNING <fs_fieldcat> WITH KEY fieldname = i_fieldname.
      CHECK sy-subrc = 0.
      <fs_fieldcat>-edit = i_editable.
      lo_grid->set_frontend_fieldcatalog( lt_fieldcat ).
    ELSE.
      lo_grid->get_frontend_layout( IMPORTING es_layout = ls_layout ).
      ls_layout-edit = i_editable.
      lo_grid->set_frontend_layout( EXPORTING is_layout = ls_layout ).
    ENDIF.

    CHECK i_refresh = abap_true.
    i_salv_table->refresh( lc_stable ).

  ENDMETHOD.

ENDCLASS.
