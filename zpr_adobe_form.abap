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
      gs_fp_outputparams TYPE sfpoutputparams.
 
CONSTANTS : gv_form_name TYPE fpname VALUE 'ZPR_ADOBE_FORM'.
 
**&&~~ Selection Screen

PARAMETERS : p_text TYPE char30.

**&&~~ Form Processing: Call Form - Open

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

CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
  EXPORTING
    i_name     = gv_form_name
  IMPORTING
    e_funcname = gv_fm_name.
IF sy-subrc <> 0.
  " Suitable Error Handling
ENDIF.

**&&~~ Take the FM name by executing the form - by using Pattern-
**&&~~ call that FM and replace the FM Name by gv_fm_name
**&&~~ Call the Generated FM

CALL FUNCTION gv_fm_name   "'/1BCDWB/SM00000176'
  EXPORTING
    /1bcdwb/docparams = gs_fp_docparams
    iv_text           = p_text
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

