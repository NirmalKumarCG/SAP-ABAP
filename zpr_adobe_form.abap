*&---------------------------------------------------------------------*
*======================================================================*
*                     ZPR_ADOBE_FORM                                   *
*======================================================================*
* Program     : SAP Adobe Forms Driver Program                         *
* Description : Driver Program to Print Adobe form                     *
*======================================================================*
REPORT yram_adobe_form_program1.
 
TABLES : apb_lpd_otr_keys.

DATA: gv_fm_name         TYPE rs38l_fnam,      " FM Name
      gs_fp_docparams    TYPE sfpdocparams,
      gs_fp_outputparams TYPE sfpoutputparams,
      lx_fp_api          TYPE REF TO cx_fp_api,
      cntry(2)           TYPE c.
   
CONSTANTS : gv_form_name TYPE fpname VALUE 'ZPR_ADOBE_FORM'.
 
**&&~~ Selection Screen

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS  : p_carrid TYPE sflight-carrid ,
              p_connid TYPE sflight-connid.
              
SELECTION-SCREEN : END OF BLOCK b1.

**&&~~ Form Processing: Call Form - Open

gs_fp_outputparams-preview = 'X'.

"sets the output parameters and open up the spool job.
CALL FUNCTION 'FP_JOB_OPEN'
  CHANGING
    ie_outputparams = gs_fp_outputparams
  EXCEPTIONS
    cancel          = 1
    usage_error     = 2
    system_error    = 3
    internal_error  = 4
    OTHERS          = 5.
IF sy-subrc <> 0.
  " Suitable Error Handling
ENDIF.

**&&~~ Get the Function module name based on Form Name

TRY.
CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
  EXPORTING
    i_name     = gv_form_name
  IMPORTING
    e_funcname = gv_fm_name.
IF sy-subrc <> 0.
  " Suitable Error Handling
ENDIF.

CATCH cx_fp_api INTO lx_fp_api.
"If there is an error in fetching the FM , Displaying error message and exit"
MESSAGE 'There was an error in the FM' TYPE 'E' DISPLAY LIKE 'S'.
EXIT.
ENDTRY.

"setting language and country variables into the gs_fp_docparams structure.
gs_fp_docparams-langu = sy-langu.

SELECT lasio
       FROM t002 
       INTO cntry 
       WHERE spras EQ sy-langu.

gs_fp_docparams-country = cntry.


**&&~~ Take the FM name by executing the form - by using Pattern-
**&&~~ call that FM and replace the FM Name by gv_fm_name
**&&~~ Call the Generated FM

CALL FUNCTION gv_fm_name   "'/1BCDWB/SM00000176'
  EXPORTING
    /1bcdwb/docparams = gs_fp_docparams
    i_carrid          = p_carrid
    i_connid          = p_connid "(depends on the form importing parameters)
* IMPORTING
*   /1BCDWB/FORMOUTPUT       =
  EXCEPTIONS
    usage_error       = 1
    system_error      = 2
    internal_error    = 3
    OTHERS            = 4.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.


**&&~~ Form Processing: Call Form - Close

CALL FUNCTION 'FP_JOB_CLOSE'
* IMPORTING
*   E_RESULT             =
* EXCEPTIONS
*   USAGE_ERROR          = 1
*   SYSTEM_ERROR         = 2
*   INTERNAL_ERROR       = 3
*   OTHERS               = 4.
  
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

