
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40
41
42
43
44
45
46
47
48
49
50
51
52
53
54
55
56
57
58
59
60
61
62
63
64
65
66
67
68
69
70
71
72
73
74
75
76
77
78
79
80
81
82
83
84
 
*&---------------------------------------------------------------------*
*======================================================================*
*                     YRAM_ADOBE_FORM_PROGRAM1                           *
*======================================================================*
* Project     : SAP Adobe Forms Tutorial                               *
* Author      : Ramanjula Naidu DARURU                                 *
* Description : Driver Program to Print Adobe form                     *
*======================================================================*
REPORT yram_adobe_form_program1.
 
TABLES : apb_lpd_otr_keys.
 
**&&~~ Data Objects
DATA: gv_fm_name         TYPE rs38l_fnam,      " FM Name
      gs_fp_docparams    TYPE sfpdocparams,
      gs_fp_outputparams TYPE sfpoutputparams.
 
CONSTANTS : gv_form_name TYPE fpname VALUE 'YRAM_ADOBE_FORM1'.
 
**&&~~ Selection Screen
*
PARAMETERS : p_text TYPE char30.
*&---------------------------------------------------------------------*
**&&~~ Form Processing: Call Form - Open
*
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
*&---------------------------------------------------------------------*
**&&~~ Get the Function module name based on Form Name
*
CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
  EXPORTING
    i_name     = gv_form_name
  IMPORTING
    e_funcname = gv_fm_name.
IF sy-subrc <> 0.
  " Suitable Error Handling
ENDIF.
*&---------------------------------------------------------------------*
**&&~~ Take the FM name by executing the form - by using Pattern-
**&&~~ call that FM and replace the FM Name by gv_fm_name
*
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
*&---------------------------------------------------------------------*
**&&~~ Form Processing: Call Form - Open
*
CALL FUNCTION 'FP_JOB_CLOSE'
* IMPORTING
*   E_RESULT             =
* EXCEPTIONS
*   USAGE_ERROR          = 1
*   SYSTEM_ERROR         = 2
*   INTERNAL_ERROR       = 3
*   OTHERS               = 4
  .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.
*&---------------------------------END----------------------------------*
