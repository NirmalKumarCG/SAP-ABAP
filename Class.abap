&******************************************************************************************************************&
&******************************************************************************************************************&
&                                               CLASS lcl_car DEFINITION
&******************************************************************************************************************&
&******************************************************************************************************************&

CLASS lcl_car DEFINITION.

PUBLIC DEFINITION.

CLASS-DATA : numofcars TYPE i.

DATA : make      TYPE c LENGTH 20,
       model     TYPE c LENGTH 20,
       numseats     TYPE i,
       speed     TYPE i,
       maxspeed TYPE i,
       
METHODS setnumseats IMPORTING numseat.

METHODS gofaster IMPORTING  increment TYPE i
                 EXPORTING  value(result) TYPE i.

METHODS goslower IMPORTING increment TYPE i,
                 RETURNING value(result) TYPE i.

PRIVATE DEFINITION.

ENDCLASS.

&******************************************************************************************************************&
&******************************************************************************************************************&
&                                             CLASS lcl_car IMPLEMENTATION
&******************************************************************************************************************&
&******************************************************************************************************************&



CLASS lcl_car IMPLEMENTATION.

METHOD setnumseats.
numseats = newnumseats.
ENDMETHOD 

METHOD gofaster.
DATA tmpspeed TYPE i.
tmpspeed = tmpspeed + increment.
IF tmpspeed <= maxspeed.
speed = speed + increment.
ENDIF.
result = speed. 
ENDMETHOD   


METHOD goslower.
DATA tmpspeed TYPE i.
tmpspeed = tmpspeed - increment.
IF tmpspeed >= maxspeed.
speed = speed - increment.
ENDIF.
result = speed.
ENDMETHOD


ENDCLASS.
