*&--------------- Report  ZMM_SNP_SPA_PSF_WORKFLOW --------------------*
* Author's name   : Nirmal Kumar CG                                    *
* Program title   : SNP - SPA - PSF Report                             *
* Module          : MM                                                 *
*&---------------------------------------------------------------------*
* Description : Generating AlV report for SNP,SPA,PSF Workflows.       *
*&---------------------------------------------------------------------*

REPORT zmm_snp_spa_psf_workflow.


INCLUDE zmm_snp_spa_psf_workflow_ss.

&---------------------------------------------------------------------*
*&  Include           ZMM_SNP_SPA_PSF_WORKFLOW_SS
*&---------------------------------------------------------------------*

TABLES : zspa_pcf_release , zsss_release, t024.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS : p_rad1 RADIOBUTTON GROUP r1 DEFAULT 'X' USER-COMMAND u1,
             p_rad2 RADIOBUTTON GROUP r1.

SELECTION-SCREEN : END OF BLOCK b1.

SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

PARAMETERS : p_docty TYPE zspa_pcf_release-doc_type MODIF ID 100 AS LISTBOX VISIBLE LENGTH 10,
             p_ekgrp TYPE ekgrp MODIF ID 100,
             p_purgp TYPE ekgrp MODIF ID 101.

SELECT-OPTIONS : s_partc FOR zsss_release-partc MODIF ID 101 NO INTERVALS NO-EXTENSION.


SELECTION-SCREEN : END OF BLOCK b2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_docty.

  PERFORM list_parameter.

  IF p_rad1 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 101.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p_rad2 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 100.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_partc-low.

  PERFORM list_partc.
  
*&---------------------------------------------------------------------*
*&  Include           ZMM_SNP_SPA_PSF_WORKFLOW_SUB
*&---------------------------------------------------------------------*

INCLUDE ZMM_SNP_SPA_PSF_WORKFLOW_SUB.

FORM list_parameter.

  TYPES : BEGIN OF ty_doc,
            doc_type TYPE zspa_pcf_release-doc_type,
          END OF ty_doc.

  DATA : it_doc  TYPE STANDARD TABLE OF ty_doc,
         it_list TYPE vrm_values.

  CLEAR : it_list, it_doc.

  SELECT DISTINCT doc_type FROM zspa_pcf_release INTO TABLE it_doc WHERE doc_type NE ' '.
  IF sy-subrc EQ 0.
    SORT it_doc BY doc_type.
  ENDIF.

  it_list[] = VALUE #( FOR wa_doc IN it_doc
                     ( key = wa_doc-doc_type ) ).


  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_DOCTY'
      values          = it_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

ENDFORM.

FORM list_partc.

  SELECT DISTINCT partc FROM zsss_release INTO TABLE @DATA(it_partc).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PARTC'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'S_PARTC'
      value_org       = 'S'
    TABLES
      value_tab       = it_partc
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

CLASS lcl_spa_workflow DEFINITION.

  PUBLIC SECTION.

    TYPES : BEGIN OF ty_spa,
              docty  TYPE zspa_pcf_release-doc_type,
              purgp  TYPE zspa_pcf_release-ekgrp,
              purds  TYPE t024-eknam,
              rcode  TYPE zspa_pcf_release-rcode,
              zuser  TYPE zspa_pcf_release-zuser,
              zuname TYPE zspa_pcf_release-zuname,
            END OF ty_spa.

    DATA : o_alv  TYPE REF TO cl_salv_table,
           lt_spa TYPE STANDARD TABLE OF ty_spa.

    METHODS get_data.

    METHODS generate_output.

  PRIVATE SECTION.

    METHODS set_pf_status
      CHANGING co_alv TYPE REF TO cl_salv_table.

ENDCLASS.

CLASS lcl_spa_workflow IMPLEMENTATION.

  METHOD get_data.

    SELECT a~doc_type, a~ekgrp, b~eknam, a~rcode, a~zuser, a~zuname
         FROM zspa_pcf_release AS a
         INNER JOIN t024 AS b ON a~ekgrp = b~ekgrp
         INTO TABLE @lt_spa
         WHERE a~doc_type EQ @p_docty
         AND a~ekgrp EQ @p_ekgrp.

    IF sy-subrc EQ 0.
      SORT lt_spa BY docty
                        purgp.
    ELSE.
      MESSAGE 'No Data Found!!!' TYPE 'S' DISPLAY LIKE 'E' ##NO_TEXT.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDMETHOD.

  METHOD generate_output.


    DATA : lc_msg    TYPE REF TO cx_salv_msg,
           columns   TYPE REF TO cl_salv_columns_table,
           column    TYPE REF TO cl_salv_column,
           not_found TYPE REF TO cx_salv_not_found.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = o_alv
          CHANGING
            t_table      = lt_spa.

        columns = o_alv->get_columns( ).
        columns->set_optimize( 'X' ).
        TRY.
            column = columns->get_column( 'ZUNAME' ).
            column->set_long_text( 'User Name' ) ##NO_TEXT.
            column->set_medium_text( 'User Name' ) ##NO_TEXT.
            column->set_short_text( 'U.Name' ) ##NO_TEXT.
          CATCH cx_salv_not_found INTO not_found.
        ENDTRY.



      CATCH cx_salv_msg INTO lc_msg.
    ENDTRY.

    CALL METHOD set_pf_status
      CHANGING
        co_alv = o_alv.

    o_alv->display( ).


  ENDMETHOD.

  METHOD set_pf_status.

      co_alv->set_screen_status( pfstatus = 'ZSTANDARD'
                                 report   = sy-repid
                                 set_functions = co_alv->c_functions_all ).

  ENDMETHOD.


ENDCLASS.

CLASS lcl_snp_workflow DEFINITION.

  PUBLIC SECTION.

    TYPES : BEGIN OF ty_snp,
              partc  TYPE zsss_release-partc,
              ekgrp  TYPE zsss_release-ekgrp,
              eknam  TYPE t024-eknam,
              rcode  TYPE zsss_release-rcode,
              rlevel TYPE zsss_release-rlevel,
              zuser  TYPE zsss_release-zuser,
              zuname TYPE zsss_release-zuname,
            END OF ty_snp.

    DATA : o_alv  TYPE REF TO cl_salv_table,
           lt_snp TYPE STANDARD TABLE OF ty_snp.


    METHODS get_data_snp.

    METHODS generate_output_snp.

    PRIVATE SECTION.

    METHODS set_pf_status
      CHANGING co_alv TYPE REF TO cl_salv_table.

ENDCLASS.

CLASS lcl_snp_workflow IMPLEMENTATION.

  METHOD get_data_snp.

    SELECT a~partc ,a~ekgrp ,b~eknam ,a~rcode ,a~rlevel ,a~zuser ,a~zuname
      FROM zsss_release AS a
      INNER JOIN t024 AS b ON a~ekgrp = b~ekgrp
      INTO TABLE @lt_snp
      WHERE a~partc IN @s_partc
      AND a~ekgrp EQ @p_purgp.

    IF lt_snp IS NOT INITIAL.
      SORT lt_snp BY ekgrp.
    ELSE.
      MESSAGE 'No Data Found!!!' TYPE 'S' DISPLAY LIKE 'E' ##NO_TEXT.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDMETHOD.

  METHOD generate_output_snp.

    DATA : lc_msg    TYPE REF TO cx_salv_msg,
           columns   TYPE REF TO cl_salv_columns_table,
           column    TYPE REF TO cl_salv_column,
           not_found TYPE REF TO cx_salv_not_found,
           repid     TYPE sy-repid.


    IF lt_snp IS NOT INITIAL.

      TRY.
          CALL METHOD cl_salv_table=>factory(
            IMPORTING
              r_salv_table = o_alv
            CHANGING
              t_table      = lt_snp ).

          columns = o_alv->get_columns( ).
          columns->set_optimize( 'X' ).
          TRY.
              column = columns->get_column( 'PARTC' ).
              column->set_long_text( 'Part Criticality' ) ##NO_TEXT.
              column->set_medium_text( 'Part Criticality') ##NO_TEXT.
              column->set_short_text( 'Part C' ) ##NO_TEXT.

              column = columns->get_column( 'RLEVEL' ).
              column->set_long_text( 'Release Level' ) ##NO_TEXT.
              column->set_medium_text( 'Release Level' ) ##NO_TEXT.
              column->set_short_text( 'R Level' ) ##NO_TEXT.

              column = columns->get_column( 'ZUNAME' ).
              column->set_long_text( 'User Name' ) ##NO_TEXT.
              column->set_medium_text( 'User Name' ) ##NO_TEXT.
              column->set_short_text( 'User Name' ) ##NO_TEXT.

            CATCH cx_salv_not_found INTO not_found.
          ENDTRY.

        CATCH cx_salv_msg INTO lc_msg.
      ENDTRY.

      CALL METHOD set_pf_status
        CHANGING
          co_alv = o_alv.

      o_alv->display( ).

    ELSE.
      MESSAGE 'No Data Found!!! ' TYPE 'S' DISPLAY LIKE 'E' ##NO_TEXT.
    ENDIF.


  ENDMETHOD.

  METHOD set_pf_status.

    co_alv->set_screen_status( pfstatus = 'ZSTANDARD'
                                 report   = sy-repid
                                 set_functions = co_alv->c_functions_all ).

  ENDMETHOD.


ENDCLASS.

START-OF-SELECTION.

  DATA : spa_workflow TYPE REF TO lcl_spa_workflow.

  CREATE OBJECT spa_workflow.

  DATA : snp_workflow TYPE REF TO lcl_snp_workflow.

  CREATE OBJECT snp_workflow.

  IF p_rad1 = 'X'.
    IF p_docty IS NOT INITIAL AND p_ekgrp IS NOT INITIAL.

      PERFORM check_parameter_spa.
      spa_workflow->get_data( ).
      spa_workflow->generate_output( ).

    ELSE.
      MESSAGE 'Enter the Proc.Doc Type and Purchase GRP!' TYPE 'S' DISPLAY LIKE 'E' ##NO_TEXT.
    ENDIF.
  ENDIF.

  IF p_rad2 = 'X'.
    IF p_purgp IS NOT INITIAL .

      PERFORM check_parameter_snp.
      snp_workflow->get_data_snp( ).
      snp_workflow->generate_output_snp( ).

    ELSE.
      MESSAGE 'Please Enter the Purchasing Group!' TYPE 'S' DISPLAY LIKE 'E' ##NO_TEXT.
    ENDIF.
  ENDIF.

FORM check_parameter_spa.

  TYPES : BEGIN OF ty_ekgrp,
            ekgrp TYPE t024-ekgrp,
          END OF ty_ekgrp.

  DATA : it_ekgrp TYPE STANDARD TABLE OF ty_ekgrp.

  CLEAR : it_ekgrp.

  SELECT DISTINCT ekgrp
    FROM t024
    INTO TABLE it_ekgrp
    WHERE ekgrp EQ p_ekgrp.

  IF it_ekgrp IS INITIAL.
    MESSAGE 'Invalid Pruchasing Grp!!!' TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

FORM check_parameter_snp.

  TYPES : BEGIN OF ty_ekgrp,
            ekgrp TYPE t024-ekgrp,
          END OF ty_ekgrp.

  DATA : it_ekgrp TYPE STANDARD TABLE OF ty_ekgrp.

  CLEAR : it_ekgrp.

  SELECT DISTINCT ekgrp
    FROM t024
    INTO TABLE it_ekgrp
    WHERE ekgrp EQ p_purgp.

  IF it_ekgrp IS INITIAL.
    MESSAGE 'Invalid Pruchasing Grp!!!' TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

FORM set_pf_status USING rt_extab TYPE slis_t_extab ##CALLED ##NEEDED.
  SET PF-STATUS 'ZSTANDARD'.
ENDFORM.
