*Creating Classes and Subclasses for better understanding the concepts of Inheritance.

*&---------------------------------------------------------------------------------------*
*&                            CLASS lcl_vehicle DEFINITON                                *
*&---------------------------------------------------------------------------------------*

CLASS lcl_vehicle DEFINITION.

PUBLIC SECTION.

  METHODS gofaster.

  METHODS writespeed.

PROTECTED SECTION.

  DATA : speed TYPE i.

PRIVATE SECTION.

ENDCLASS.

*&---------------------------------------------------------------------------------------*
*&                            CLASS lcl_vehicle IMPLEMENTATION                           *
*&---------------------------------------------------------------------------------------*

CLASS lcl_vehicle IMPLEMENTATION.

METHOD gofaster.

DATA : tmpspeed TYPE i.
tmpspeed = speed + 1.
speed = tmpspeed.
CLEAR tmpspeed.

ENDMETHOD.

METHOD writespeed.

WRITE : / 'The Vehicle Speed is' , speed.

ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------------------------*
*&                            CLASS lcl_car DEFINITON                                    *
*&---------------------------------------------------------------------------------------*

CLASS lcl_car DEFINITION INHERITING FROM lcl_vehicle.

PUBLIC SECTION.

METHODS refuel.

PROTECTED SECTION.

DATA fuellevel TYPE i.

ENDCLASS.

*&---------------------------------------------------------------------------------------*
*&                            CLASS lcl_car IMPLEMENTATION                               *
*&---------------------------------------------------------------------------------------*

CLASS lcl_car IMPLEMENTATION.

METHOD refuel.

fuellevel = 60.
Write : / 'You have just filled up your fuel tank!'. 

ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------------------------*
*&                            CLASS lcl_boat DEFINITON                                   *
*&---------------------------------------------------------------------------------------*

CLASS lcl_boat DEFINITION INHERITING FROM lcl_vehicle.

ENDCLASS.

START-OF-SELECTION.

DATA : car TYPE REF TO lcl_car.
CREATE OBJECT car.

DATA : boat TYPE REF TO lcl_boat.
CREATE OBJECT boat.

car->gofaster( ).
car->writespeed( ).
car->refuel( ).

boat->gofaster( ).
boat->writespeed( ).





