*This is a dialog program / Module Pool Program with a screen display of table control wizard which enables the user to 
 enter the time management status of the employer . Logic written for addition and deletion of rows. Created a save button
 to update the data into a database table .

*&---------------------------------------------------------------------*
*& Report  ZHR_TIME_STAT
*&---------------------------------------------------------------------*

REPORT zhr_time_stat.

TABLES : zhr_time_stat.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_CTRL' ITSELF
CONTROLS: tc_ctrl TYPE TABLEVIEW USING SCREEN 9000.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_CTRL'
DATA:     g_tc_ctrl_lines  LIKE sy-loopc.


TYPES: BEGIN OF ty_stat,         "output type required.
         mandt      TYPE mandt,
         pernr      TYPE p_pernr,
         begda      TYPE begda,
         endda      TYPE endda,
         tmsta      TYPE pt_zterf,
         changed_by TYPE uname,
         changed_on TYPE sy-datum,
         flag,
       END OF ty_stat.



DATA: gt_stat TYPE TABLE OF ty_stat,
      gs_stat TYPE ty_stat.

DATA : gt_sta TYPE TABLE OF zhr_time_stat,
       gs_sta TYPE zhr_time_stat.

DATA:     ok_code LIKE sy-ucomm.

START-OF-SELECTION.
  CALL SCREEN 9000.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_CTRL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_ctrl_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_stat LINES tc_ctrl-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_CTRL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc_ctrl_get_lines OUTPUT.
  g_tc_ctrl_lines = sy-loopc.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'ZHR_TIMESTAT_PF'.   "sets PF status for the result page .
  SET TITLEBAR 'ZHR_TIMESTAT_TITLE'. "sets title for the result
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SELECT_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE select_data OUTPUT.
  PERFORM get_data.
  IF gt_stat IS INITIAL .
    DO 1 TIMES .
      APPEND INITIAL LINE TO gt_stat.
    ENDDO.
  ENDIF.
ENDMODULE .

*&SPWIZARD: INPUT MODULE FOR TC 'TC_VENDOR'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc_ctrl_modify INPUT.
  MODIFY gt_stat
    FROM gs_stat
    INDEX tc_ctrl-current_line.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC_CTRL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc_ctrl_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_CTRL'
                              'GT_STAT'
                              ' '
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  DATA flag1(1) TYPE c.

  CLEAR ok_code.
  ok_code = sy-ucomm.

  CASE ok_code.
    WHEN 'BACK'.  "back
      LEAVE PROGRAM.
    WHEN 'EXI'.   "exit
      LEAVE PROGRAM.
    WHEN 'ADD'.   "insert a editable row
      APPEND INITIAL LINE TO gt_stat.
    WHEN 'DEL'.   "delete a row if flag = 'X'.
      PERFORM delete.
    WHEN 'SAVE'.
      "for updated rows to be saved.

      FREE gt_sta.
      DELETE gt_stat WHERE pernr IS INITIAL.
      IF gt_stat IS NOT INITIAL.
*        GT_STA = VALUE #( LET LT_NEW = GT_STAT IN FOR WA IN LT_NEW ( PERNR = |{ WA-PERNR ALPHA = IN }|
*                                                                     BEGDA = WA-BEGDA
*                                                                     ENDDA = WA-ENDDA
*                                                                     TMSTA = WA-TMSTA
*                                                                     CHANGED_BY = WA-CHANGED_BY
*                                                                     CHANGED_ON = WA-CHANGED_ON ) ).
        gt_sta = VALUE #( FOR wa1 IN gt_stat ( pernr = |{ wa1-pernr ALPHA = IN }|
                                                                     begda = wa1-begda
                                                                     endda = wa1-endda
                                                                     tmsta = wa1-tmsta
                                                                     changed_by = wa1-changed_by
                                                                    changed_on = wa1-changed_on ) ).
        "Added by Nirmal on 30.01.2020 - TR SE1K9A1XVU.
        CLEAR flag1.
        LOOP AT gt_stat INTO DATA(gs_valid).
          IF gs_valid-begda GT gs_valid-endda.
            flag1 = 'X'.
            MESSAGE 'Start date cannot be greater than End date' TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ELSEIF gs_valid-tmsta NE 1 AND gs_valid-tmsta NE 9.
            flag1 = 'X'.
            MESSAGE 'Time Status should be 1 or 9' TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF flag1 IS INITIAL.               "Added by Nirmal on 30.01.2020 - TR SE1K9A1XVU.
          MOVE-CORRESPONDING gt_stat TO gt_sta.
          IF gt_sta IS NOT INITIAL.
          MODIFY zhr_time_stat FROM TABLE gt_sta.
          IF sy-subrc EQ 0.
          COMMIT WORK.
          MESSAGE 'Data Successfully Updated' TYPE 'I' DISPLAY LIKE 'S'.
        ENDIF.
      ENDIF.
     ENDIF.
    ENDIF.
  ENDCASE.
  CLEAR ok_code.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete.  "to delete the rows when flag = 'X'.
  FREE gt_sta.
  IF gt_stat IS NOT INITIAL.
    READ TABLE gt_stat INTO DATA(gs_s) WITH KEY flag = 'X'.
    IF sy-subrc = 0.
      DATA(gt_del) = gt_stat[].
      DELETE gt_del WHERE flag NE 'X' .
      IF gt_del IS NOT INITIAL.
        MOVE-CORRESPONDING gt_del TO gt_sta.
        IF gt_sta IS NOT INITIAL.
        DELETE zhr_time_stat FROM TABLE gt_sta.
          IF sy-subrc = 0.
          DELETE gt_stat WHERE flag EQ 'X'.
          COMMIT WORK.
          MESSAGE 'Data Succefully Deleted' TYPE 'I' DISPLAY LIKE 'S'.
          LEAVE LIST-PROCESSING .
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE 'Please Select Atleast One checkbox' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
 ENDIF.


ENDFORM.


*&---------------------------------------------------------------------*
*&     Form GET_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data.
  IF gt_stat IS INITIAL.
    SELECT * FROM zhr_time_stat
       INTO CORRESPONDING FIELDS OF TABLE gt_stat.
    IF sy-subrc EQ 0.
      SORT gt_stat BY pernr.
    ENDIF.
  ENDIF.
ENDFORM .


*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
