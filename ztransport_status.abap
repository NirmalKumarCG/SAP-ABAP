*&--------------------------------------------------------------------------*
*& Report  ZTRANSPORT_STATUS
*&--------------------------------------------------------------------------*
*& Program Id          :  ZTRANSPORT_STATUS                                 *
*& Description         :  Check Transport Request Status                    *
*& Created by          :  Nirmal Kumar CG                                   *
*& Created on          :  23/02/2020                                        *
*& Function module     :  GET_DOMAIN_VALUES                                 *
*&                        TR_OBJECT_TABLE                                   *
*&                        TR_READ_GLOBAL_INFO_OF_REQUEST                    *
*&                        REUSE_ALV_GRID_DISPLAY                            *
*&------------------------------------------------------------------------- *
*&------------------------------------------------------------------------- *
*&------------------------------------------------------------------------- *


REPORT  ztransport_status MESSAGE-ID wn.

TYPE-POOLS: ctslg, slis.
TABLES: e070.

TYPES: BEGIN OF ty_outtab,
         obj_name    TYPE e071-obj_name,    " Oject name
         task        TYPE e070-trkorr,      " Transport request
         subtask     TYPE e070-trkorr,      " Sub task
         objtyp      TYPE ddtext,           " Object type Desc
         pgmid       TYPE e071-pgmid ,      " Program ID in Requests and Tasks
         object      TYPE e071-object,      " Object Type
         trdesc      TYPE e07t-as4text,     " REquest Desc
         user        TYPE e070-as4user,     " User id
         name        TYPE char20,           " User name
         status      TYPE ddtext,           " Request status
         change_date TYPE e070-as4date,     " Release date
         change_time TYPE e070-as4time,     " release time
         quast(10)   TYPE c,                " QA Status
         prdst(10)   TYPE c,                " PRD Status
         quadt(10)   TYPE c,                " QA Date
         quatm(8)    TYPE c,                " QA Time
         prddt(10)   TYPE c,                " PRD Date
         prdtm(8)    TYPE c,                " PRD Time
         color(4)    TYPE c,                " Color
       END OF ty_outtab.
*
TYPES: BEGIN OF ty_objname,
         trkorr   TYPE e071-trkorr,      " Transport request
         as4pos   TYPE e071-as4pos,      " Dictionary: Line item
         obj_name TYPE e071-obj_name,    " Oject name
       END OF ty_objname.
*
DATA: gi_e070     TYPE STANDARD TABLE OF e070,
      gi_e071     TYPE STANDARD TABLE OF e071,
      gi_e070_tmp TYPE STANDARD TABLE OF e070,
      gi_e071_tmp TYPE STANDARD TABLE OF e071,
      gi_dd07v    TYPE STANDARD TABLE OF dd07v,
      gi_objtext  TYPE STANDARD TABLE OF ko100,
      gi_objname  TYPE STANDARD TABLE OF ty_objname,
      gi_obj      TYPE STANDARD TABLE OF ty_objname,
      gi_cab      TYPE tmscsyslst_typ,
      gi_systems  TYPE ctslg_systems,
      gi_fieldcat TYPE slis_t_fieldcat_alv,
      gi_final    TYPE STANDARD TABLE OF ty_outtab,
      gi_outtab   TYPE STANDARD TABLE OF ty_outtab.
*
DATA: gs_e070     TYPE e070,
      gs_e071     TYPE e071,
      gs_e070_tmp TYPE e070,
      gs_e071_tmp TYPE e071,
      gs_dd07v    TYPE dd07v,
      gs_objtext  TYPE ko100,
      gs_cab      TYPE tmscsyslst,
      gs_cofile   TYPE ctslg_cofile,
      gs_obj      TYPE ty_objname,
      gs_systems  LIKE LINE OF gi_systems,
      gi_clients  LIKE gs_systems-steps,
      gs_clients  LIKE LINE OF gi_clients,
      gi_actions  LIKE gs_clients-actions,
      gs_actions  LIKE LINE OF gi_actions,
      gs_fieldcat TYPE slis_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gs_final    TYPE ty_outtab,
      gs_outtab   TYPE ty_outtab.

FIELD-SYMBOLS <gf_outtab> TYPE ty_outtab.

DATA: gv_qua(3) TYPE c,
      gv_prd(3) TYPE c,
      gv_tabix  TYPE sy-tabix.

RANGES: r_prog FOR e071-obj_name.
*
*&---------------------------------------------------------------------*
*&                   SELECTION SCREEN                                  *
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: rb_prg RADIOBUTTON GROUP grp USER-COMMAND u1 DEFAULT 'X'.
PARAMETERS: p_prog TYPE e071-obj_name MODIF ID pp2.
SELECT-OPTIONS: s_trno FOR e070-trkorr NO INTERVALS MODIF ID pp2.
SELECTION-SCREEN SKIP.
PARAMETERS: rb_gen RADIOBUTTON GROUP grp.
SELECT-OPTIONS: s_date FOR e070-as4date MODIF ID pp1,
                s_name FOR e070-as4user NO INTERVALS MODIF ID pp1.

SELECTION-SCREEN END OF BLOCK b1.

*-Request Selection-*

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15)  text-003.
SELECTION-SCREEN COMMENT 40(25) text-004.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 01.
PARAMETERS: rb_mod  RADIOBUTTON GROUP g1 .            " Modifiable
SELECTION-SCREEN COMMENT 3(15)  text-030.
SELECTION-SCREEN POSITION 40.
PARAMETERS: rb_cus  RADIOBUTTON GROUP g2 .            " Customizing
SELECTION-SCREEN COMMENT 43(15)  text-031.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 01.
PARAMETERS: rb_rel  RADIOBUTTON GROUP g1.             " Released
SELECTION-SCREEN COMMENT 3(15)  text-032.
SELECTION-SCREEN POSITION 40.
PARAMETERS: rb_wrk  RADIOBUTTON GROUP g2.             " Workbench
SELECTION-SCREEN COMMENT 43(15)  text-033.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 01.
PARAMETERS: rb_both RADIOBUTTON GROUP g1 DEFAULT 'X'. " Both
SELECTION-SCREEN COMMENT 3(15)  text-034.
SELECTION-SCREEN POSITION 40.
PARAMETERS: rb_all RADIOBUTTON GROUP g2 DEFAULT 'X'.  " Both
SELECTION-SCREEN COMMENT 43(15)  text-034.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.

*-Server / Client Selection

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-040.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15)  text-035.
SELECTION-SCREEN COMMENT 40(25) text-036.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 01.
SELECTION-SCREEN COMMENT 3(15)  text-037.
PARAMETERS: p_dev TYPE char03 DEFAULT 'SE1'.         " Development
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 01.
SELECTION-SCREEN COMMENT 3(15)  text-038.
PARAMETERS: p_qua TYPE char03 DEFAULT 'QE1'.          " Quality
SELECTION-SCREEN POSITION 40.
PARAMETERS: p_cab TYPE char03 DEFAULT '400'.          " Quality Client
SELECTION-SCREEN END OF LINE.
*
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 01.
SELECTION-SCREEN COMMENT 3(15)  text-039.
PARAMETERS: p_prd TYPE char03 DEFAULT 'PE1'.          " Production
SELECTION-SCREEN POSITION 40.
PARAMETERS: p_ca1 TYPE char03 DEFAULT '400'.          " Production Client
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b3.

*&---------------------------------------------------------------------*
*&                   AT SELECTION SCREEN                               *
*&---------------------------------------------------------------------*
*
AT SELECTION-SCREEN OUTPUT.
*
  IF rb_gen = 'X'.
    CLEAR: p_prog,
           s_trno[].
*
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'PP2'.
          screen-input     = 0.
          screen-invisible = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
  ELSE.
    CLEAR : s_date[],
            s_name[].
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'PP1'.
          screen-input     = 0.
          screen-invisible = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
  ENDIF.
*
*&---------------------------------------------------------------------*
*&                   START OF SELECTION                                *
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM validate_data.
  PERFORM get_data.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM validate_data .
*
  IF rb_gen = 'X'.
    IF s_date IS INITIAL AND
       s_name IS INITIAL.
      MESSAGE e000 WITH text-011.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
    IF p_prog IS INITIAL AND
       s_trno IS INITIAL.
      MESSAGE e000 WITH text-013.
      LEAVE LIST-PROCESSING.
    ENDIF.
    IF p_prog IS NOT INITIAL AND
       s_trno IS NOT INITIAL.
      MESSAGE e000 WITH text-007.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
*
  IF p_dev IS INITIAL AND
     p_qua IS INITIAL AND
     p_prd IS INITIAL.
    MESSAGE e000 WITH text-012.
    LEAVE LIST-PROCESSING.
  ELSE.
    gv_qua = p_qua.
    gv_prd = p_prd.
  ENDIF.
*
ENDFORM.                    " VALIDATE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .
*
  " Get data from E070 table
  IF rb_gen = 'X'.
    SELECT *
      FROM e070
      INTO TABLE gi_e070
     WHERE ( trstatus = 'D' OR
             trstatus = 'R' )
       AND as4user IN s_name
       AND as4date IN s_date.
    IF sy-subrc = 0.
      SORT gi_e070 BY trkorr.
    ENDIF.
  ELSE.
    " If Program name is entered in selection
    " screen build ranges
    IF p_prog IS NOT INITIAL.
      CONCATENATE p_prog '*' INTO p_prog.
      r_prog-sign   = 'I'.
      r_prog-option = 'CP'.
      r_prog-low    = p_prog.
      APPEND r_prog.
*
      " Get the Transport request numbers from E071 for Program
      SELECT trkorr
             as4pos
             obj_name
        FROM e071
        INTO TABLE gi_objname
       WHERE obj_name IN r_prog.
      IF sy-subrc = 0.
        " Get all the detail from E070 table for E071 records
        SELECT *
          FROM e070
          INTO TABLE gi_e070
           FOR ALL ENTRIES IN gi_objname
         WHERE trkorr = gi_objname-trkorr.
        " Get distinct object names
        gi_obj[] = gi_objname.
        SORT gi_obj BY obj_name.
        DELETE ADJACENT DUPLICATES FROM gi_obj COMPARING obj_name.
      ENDIF.
    ELSE.
      " Get Transport request details from E070 table
      SELECT *
        FROM e070
        INTO TABLE gi_e070
       WHERE ( trstatus = 'D' OR
               trstatus = 'R' )
         AND strkorr IN s_trno.
      SELECT *
        FROM e070
        APPENDING TABLE gi_e070
       WHERE ( trstatus = 'D' OR
               trstatus = 'R' )
         AND trkorr IN s_trno.
      IF gi_e070 IS NOT INITIAL.
        SORT gi_e070 BY trkorr.
      ENDIF.
    ENDIF.
  ENDIF.
*
  gi_e070_tmp[] = gi_e070[].
  CLEAR: gi_e070[].
  IF gi_e070_tmp IS NOT INITIAL.
    IF rb_mod = 'X' AND
       rb_cus = 'X'.
      LOOP AT gi_e070_tmp INTO gs_e070_tmp WHERE   trstatus   = 'D'
                                             AND ( trfunction = 'W'
                                              OR   trfunction = 'T' ).
        MOVE gs_e070_tmp TO gs_e070.
        APPEND gs_e070 TO gi_e070.
        CLEAR: gs_e070, gs_e070_tmp.
      ENDLOOP.
    ELSEIF rb_mod = 'X' AND
           rb_wrk = 'X'.
      LOOP AT gi_e070_tmp INTO gs_e070_tmp WHERE   trstatus   = 'D'
                                             AND ( trfunction = 'K'
                                              OR   trfunction = 'T' ).
        MOVE gs_e070_tmp TO gs_e070.
        APPEND gs_e070 TO gi_e070.
        CLEAR: gs_e070, gs_e070_tmp.
      ENDLOOP.
    ELSEIF rb_mod = 'X' AND
           rb_all = 'X'.
      LOOP AT gi_e070_tmp INTO gs_e070_tmp WHERE   trstatus   = 'D'
                                             AND ( trfunction = 'W'
                                              OR  trfunction  = 'K'
                                              OR  trfunction  = 'T' ).
        MOVE gs_e070_tmp TO gs_e070.
        APPEND gs_e070 TO gi_e070.
        CLEAR: gs_e070, gs_e070_tmp.
      ENDLOOP.
    ELSEIF rb_rel = 'X' AND
           rb_cus = 'X'.
      LOOP AT gi_e070_tmp INTO gs_e070_tmp WHERE   trstatus   = 'R'
                                             AND ( trfunction = 'W'
                                              OR   trfunction = 'T' ).
        MOVE gs_e070_tmp TO gs_e070.
        APPEND gs_e070 TO gi_e070.
        CLEAR: gs_e070, gs_e070_tmp.
      ENDLOOP.
    ELSEIF rb_rel = 'X' AND
           rb_wrk = 'X'.
      LOOP AT gi_e070_tmp INTO gs_e070_tmp WHERE   trstatus   = 'R'
                                             AND ( trfunction = 'K'
                                              OR   trfunction = 'T' ).
        MOVE gs_e070_tmp TO gs_e070.
        APPEND gs_e070 TO gi_e070.
        CLEAR: gs_e070, gs_e070_tmp.
      ENDLOOP.
    ELSEIF rb_rel = 'X' AND
           rb_all = 'X'.
      LOOP AT gi_e070_tmp INTO gs_e070_tmp WHERE   trstatus   = 'R'
                                             AND ( trfunction = 'W'
                                              OR   trfunction = 'K'
                                              OR   trfunction = 'T' ).
        MOVE gs_e070_tmp TO gs_e070.
        APPEND gs_e070 TO gi_e070.
        CLEAR: gs_e070, gs_e070_tmp.
      ENDLOOP.
    ELSEIF rb_both = 'X' AND
           rb_cus  = 'X'.
      LOOP AT gi_e070_tmp INTO gs_e070_tmp WHERE ( trstatus   = 'D'
                                              OR   trstatus   = 'R' )
                                             AND ( trfunction = 'W'
                                              OR   trfunction = 'T' ).
        MOVE gs_e070_tmp TO gs_e070.
        APPEND gs_e070 TO gi_e070.
        CLEAR: gs_e070, gs_e070_tmp.
      ENDLOOP.
    ELSEIF rb_both = 'X' AND
           rb_wrk  = 'X'.
      LOOP AT gi_e070_tmp INTO gs_e070_tmp WHERE ( trstatus   = 'D'
                                              OR   trstatus   = 'R' )
                                             AND ( trfunction = 'K'
                                              OR   trfunction = 'T' ).
        MOVE gs_e070_tmp TO gs_e070.
        APPEND gs_e070 TO gi_e070.
        CLEAR: gs_e070, gs_e070_tmp.
      ENDLOOP.
    ELSEIF rb_both = 'X' AND
           rb_all  = 'X'.
      LOOP AT gi_e070_tmp INTO gs_e070_tmp WHERE ( trstatus   = 'D'
                                              OR   trstatus   = 'R' )
                                             AND ( trfunction = 'W'
                                              OR   trfunction = 'K'
                                              OR   trfunction = 'S'
                                              OR   trfunction = 'T' ).
        MOVE gs_e070_tmp TO gs_e070.
        APPEND gs_e070 TO gi_e070.
        CLEAR: gs_e070, gs_e070_tmp.
      ENDLOOP.

    ENDIF.
  ENDIF.
*
  IF gi_e070 IS NOT INITIAL.
    " Get tranport request detaila from E071 table
    SELECT *
      FROM e071
      INTO TABLE gi_e071
       FOR ALL ENTRIES IN gi_e070
     WHERE trkorr = gi_e070-trkorr.
    IF sy-subrc = 0.
      DELETE gi_e071 WHERE pgmid  = 'CORR'
                     AND   object = 'RELE'.
*
      IF rb_prg = 'X'.
        IF gi_objname IS NOT INITIAL.
          " Retain only the relevant TR's for the Object entered in
          " Seleciton screen
          LOOP AT gi_obj INTO gs_obj.
            LOOP AT gi_e071 INTO gs_e071 WHERE obj_name = gs_obj-obj_name.
              MOVE gs_e071 TO gs_e071_tmp.
              APPEND gs_e071_tmp TO gi_e071_tmp.
              CLEAR: gs_e071_tmp.
            ENDLOOP.
            CLEAR: gs_e071.
          ENDLOOP.
          CLEAR: gi_e071[].
          gi_e071[] = gi_e071_tmp[].
          CLEAR: gi_e071_tmp[].
        ENDIF.
      ENDIF.
    ENDIF.
*
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname        = 'TRSTATUS'
        text           = 'X'
        fill_dd07l_tab = ' '
      TABLES
        values_tab     = gi_dd07v.
*
    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = gi_objtext.
*
    " Move relevant data to GI_OUTTAB
    LOOP AT gi_e071 INTO gs_e071.
      READ TABLE gi_e070 INTO gs_e070 WITH KEY trkorr = gs_e071-trkorr.
      IF sy-subrc EQ 0.
        gs_outtab-task   = gs_e070-strkorr.      " Task
      ENDIF.
      gs_outtab-subtask     = gs_e071-trkorr.       " Sub task
      gs_outtab-obj_name    = gs_e071-obj_name.     " Object name
      gs_outtab-user        = gs_e070-as4user.      " User name
      gs_outtab-change_date = gs_e070-as4date.      " Latest Change date
      gs_outtab-change_time = gs_e070-as4time.      " Latest Change time

      READ TABLE gi_dd07v INTO gs_dd07v WITH KEY domvalue_l = gs_e070-trstatus.
      IF sy-subrc EQ 0.
        gs_outtab-status   = gs_dd07v-ddtext.    " Status
      ENDIF.

      " Request Description
      SELECT
      SINGLE as4text
        FROM e07t
        INTO gs_outtab-trdesc
       WHERE trkorr = gs_e070-trkorr
         AND langu = sy-langu.
      IF sy-subrc <>  0.
        gs_outtab-trdesc = text-010. " No text available for Request
      ENDIF.

      " Object type
      READ TABLE gi_objtext INTO gs_objtext WITH KEY pgmid =  gs_e071-pgmid
                                                     object = gs_e071-object.
      IF sy-subrc EQ 0.
        gs_outtab-objtyp =  gs_objtext-text.
        CLEAR: gs_objtext .
      ENDIF.
      APPEND gs_outtab  TO gi_outtab.
      CLEAR: gs_outtab.
    ENDLOOP.
  ENDIF.
*
  LOOP AT gi_outtab INTO gs_outtab.
    CLEAR: gv_tabix, gs_cofile, gs_systems, gs_actions, gs_clients, gs_cab,
           gi_systems[], gi_clients[],gi_actions[], gi_cab[].
    gv_tabix = sy-tabix.
    " Assign Production system
    gs_cab-sysnam = gv_prd.
    APPEND gs_cab TO gi_cab.
    " Check whether Transport is moved to PRD system
    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING
        iv_trkorr       = gs_outtab-subtask
        iv_dir_type     = 'T'
        it_comm_systems = gi_cab
      IMPORTING
        es_cofile       = gs_cofile.
*
    CLEAR: gi_systems.
    gi_systems = gs_cofile-systems.
* If TR is not move to PRD system
    IF gi_systems IS INITIAL.
      CLEAR: gs_cab, gi_cab[].
      " Assign Quality System
      gs_cab-sysnam = gv_qua.
      APPEND gs_cab TO gi_cab.
      " Check whether Transport is moved to QA system
      CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
        EXPORTING
          iv_trkorr       = gs_outtab-subtask
          iv_dir_type     = 'T'
          it_comm_systems = gi_cab
        IMPORTING
          es_cofile       = gs_cofile.
      CLEAR: gi_systems.
      gi_systems = gs_cofile-systems.
    ENDIF.
*
    " Check whether the TR is available in QA system
    READ TABLE gi_systems INTO gs_systems WITH KEY systemid = gv_qua.
    IF sy-subrc = 0.
      gi_clients = gs_systems-steps.
      " Check whether the TR is imported for the QA Client
      READ TABLE gi_clients INTO gs_clients WITH KEY stepid = 'I'.
      IF sy-subrc EQ 0.
        IF gs_systems-rc = '8'.      " Import failed
          gs_outtab-quast = text-009. " Failed
        ELSE.
          gs_outtab-quast = text-008. " Success
        ENDIF.
        gi_actions = gs_clients-actions.
        READ TABLE gi_actions INTO gs_actions INDEX 1.
        IF sy-subrc EQ 0.
          IF gs_actions-date IS NOT INITIAL.
            WRITE: gs_actions-date  TO gs_outtab-quadt.
          ENDIF.
          IF gs_actions-time IS NOT INITIAL..
            WRITE: gs_actions-time  TO gs_outtab-quatm.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*
    " Check whether the TR is available in PRD system
    READ TABLE gi_systems INTO gs_systems WITH KEY systemid = gv_prd.
    IF sy-subrc = 0.
      gi_clients = gs_systems-steps.
      " Check whether the TR is imported for the PRD Client
      READ TABLE gi_clients INTO gs_clients WITH KEY stepid = 'I'.
      IF sy-subrc EQ 0.
        IF gs_systems-rc = '8'.      " Import failed
          gs_outtab-prdst = text-009. " Failed
        ELSE.
          gs_outtab-prdst = text-008. " Success
        ENDIF.
        gi_actions = gs_clients-actions.
        READ TABLE gi_actions INTO gs_actions INDEX 1.
        IF sy-subrc EQ 0.
          IF gs_actions-date IS NOT INITIAL.
            WRITE: gs_actions-date  TO gs_outtab-prddt.
          ENDIF.
          IF gs_actions-time IS NOT INITIAL.
            WRITE: gs_actions-time  TO gs_outtab-prdtm.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY gi_outtab FROM gs_outtab INDEX gv_tabix.
    CLEAR: gs_systems,
           gs_clients,
           gs_actions.
    CLEAR: gs_outtab.
  ENDLOOP.
*
  " Move the records to GI_FINAL where QUAST has values
  LOOP AT gi_outtab INTO gs_outtab WHERE quast NE ''.
    MOVE gs_outtab TO gs_final.
    APPEND gs_final TO gi_final.
    CLEAR: gs_final, gs_outtab.
  ENDLOOP.

*
  " Move QA  - Status, Time & Date to GI_OUTTAB
  "      PRD - Status, Time & Date to GI_OUTTAB
  LOOP AT gi_outtab INTO gs_outtab WHERE quast = ''.
    CLEAR: gv_tabix.
    gv_tabix = sy-tabix.
    READ TABLE gi_final INTO gs_final WITH KEY subtask = gs_outtab-task.
    IF sy-subrc = 0.
      gs_outtab-quast = gs_final-quast.
      gs_outtab-quadt = gs_final-quadt.
      gs_outtab-quatm = gs_final-quatm.
      gs_outtab-prdst = gs_final-prdst.
      gs_outtab-prddt = gs_final-prddt.
      gs_outtab-prdtm = gs_final-prdtm.
      MODIFY gi_outtab FROM gs_outtab INDEX gv_tabix.
    ENDIF.
    CLEAR: gs_final, gs_outtab.
  ENDLOOP.
*
  CLEAR: gi_final[].
  SORT gi_outtab BY task.
  gi_final[] = gi_outtab[].
*
  " Delete the duplicate records in GI_OUTTAB, based on
  " Task in GI_FINAL
  LOOP AT gi_outtab INTO gs_outtab WHERE task = ''.
    CLEAR: gv_tabix.
    gv_tabix = sy-tabix.
    READ TABLE gi_final INTO gs_final WITH KEY task = gs_outtab-subtask.
    IF sy-subrc = 0.
      DELETE gi_outtab INDEX gv_tabix.
      CONTINUE.
    ENDIF.
    CLEAR: gs_outtab, gs_final.
  ENDLOOP.
*-To add user name-*
*
  IF gi_outtab IS NOT INITIAL.
    SELECT * FROM v_usr_name
             INTO TABLE @DATA(it_usr)
             FOR ALL ENTRIES IN @gi_outtab
             WHERE bname = @gi_outtab-user.
    IF sy-subrc IS INITIAL.
      SORT it_usr BY bname.
      DELETE ADJACENT DUPLICATES FROM it_usr COMPARING bname.
    ENDIF.

    LOOP AT gi_outtab ASSIGNING <gf_outtab>.
      READ TABLE it_usr INTO DATA(wa_usr)
      WITH KEY bname = <gf_outtab>-user BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        CONDENSE wa_usr-name_first.
        CONDENSE wa_usr-name_last.
        CONCATENATE wa_usr-name_first wa_usr-name_last INTO <gf_outtab>-name SEPARATED BY space.
        CONDENSE <gf_outtab>-name.
      ENDIF.
    ENDLOOP.
  ENDIF.

*-To highlight error records in RED-*
  LOOP AT gi_outtab INTO gs_outtab WHERE quast = text-009 OR prdst = text-009.
    gs_outtab-color = 'C610'.
    MODIFY gi_outtab FROM gs_outtab TRANSPORTING color.
  ENDLOOP.
*-*

  SORT gi_outtab BY task obj_name.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_alv .
*
  IF gi_outtab IS NOT INITIAL.
    PERFORM field_catalog USING:
                '1'  'OBJ_NAME'     'Object Name'(015),
                '2'  'TASK'         'Task Number'(016),
                '3'  'SUBTASK'      'Sub Task No'(017),
                '4'  'OBJTYP'       'Object Type'(018),
                '5'  'STATUS'       'Request Status'(019),
                '6'  'USER'         'User Id'(041),
                '7'  'NAME'         'User Name'(020),
                '8'  'CHANGE_DATE'  'Change Date'(021),
                '9'  'CHANGE_TIME'  'Change Time'(022),
                '10' 'TRDESC'       'Transport Description'(023),
                '11' 'QUAST'        'QA Status'(024),
                '12' 'QUADT'        'QA Date'(025),
                '13' 'QUATM'        'QA Time'(026),
                '14' 'PRDST'        'PRD Status'(027),
                '15' 'PRDDT'        'PRD Date'(028),
                '16' 'PRDTM'        'PRD Time'(029).

    gs_layout-colwidth_optimize = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-info_fieldname    = 'COLOR'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        it_fieldcat        = gi_fieldcat
        is_layout          = gs_layout
      TABLES
        t_outtab           = gi_outtab
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
*
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG
*&---------------------------------------------------------------------*
*      Building a field catalog
*----------------------------------------------------------------------*

FORM field_catalog USING:p_pos TYPE i
                         p_fname TYPE string
*                         p_outputlen TYPE string
                         p_stext TYPE string.
  gs_fieldcat-col_pos   = p_pos.
  gs_fieldcat-fieldname = p_fname.
*  gs_fieldcat-outputlen = p_outputlen.
  gs_fieldcat-seltext_l = p_stext.
  APPEND gs_fieldcat TO gi_fieldcat.
  CLEAR:gs_fieldcat.

ENDFORM.                    " FIELD_CATALOG
