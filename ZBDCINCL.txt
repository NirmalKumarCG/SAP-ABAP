*&---------------------------------------------------------------------*
*&  Include           ZBDCINCL
*&---------------------------------------------------------------------*

***INCLUDE ZBDCINCL.
************************************************************************
* PROGRAM     : ZBDCINCL                                               *
* PROGRAM TYPE: Include program                                        *
* DESCRIPTION : Various routines which are required when creating      *
*               a batch input session.                                 *
*======================================================================*
* CHANGE HISTORY
* Date         By      Request     Description
* -----------  ------  ----------  ------------------------------------
* 02-SEP-1998  WOPTU1  D02k902489  Initial implementation
*
************************************************************************

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

*----------------------------------------------------------------------*
*    Form  BDC_OPEN_GROUP
*----------------------------------------------------------------------*
* Open a BDC session named v_group.
*----------------------------------------------------------------------*
FORM BDC_OPEN_GROUP USING V_GROUP.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
         GROUP = V_GROUP
         USER  = SY-UNAME
         KEEP  = 'X'.

  IF SY-SUBRC NE 0.
     WRITE: / 'BDC_OPEN_GROUP failed to open:', V_GROUP,
            / 'Return code:', SY-SUBRC.
     STOP.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*    Form  BDC_INSERT
*----------------------------------------------------------------------*
* Insert BDC transaction into the session.
*----------------------------------------------------------------------*
FORM BDC_INSERT USING V_TCODE.

  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE     = V_TCODE
       TABLES
            DYNPROTAB = BDCDATA.

  IF SY-SUBRC NE 0.
     WRITE: / 'BDC_INSERT failed :', V_TCODE,
            / 'Return code:', SY-SUBRC.
     STOP.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*    Form  BDC_CLOSE
*----------------------------------------------------------------------*
* Close the session which was previously opened.
*----------------------------------------------------------------------*
FORM BDC_CLOSE_GROUP USING V_GROUP.

  CALL FUNCTION 'BDC_CLOSE_GROUP'.

  IF SY-SUBRC NE 0.
     WRITE: / 'BDC_CLOSE_GROUP failed to close:', V_GROUP,
            / 'Return code:', SY-SUBRC.
     STOP.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*    Form  BDC_DYNPRO
*----------------------------------------------------------------------*
* Insert screen program name and no. into BDCDATA.
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM
                      DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.

*----------------------------------------------------------------------*
*    Form  BDC_FIELD
*----------------------------------------------------------------------*
* Insert screen field name and value into BDCDATA.
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM
                     FVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.

*-------------------------- End of program ----------------------------*
