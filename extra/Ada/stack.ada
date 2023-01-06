
-------- SIMTEL20 Ada Software Repository Prologue ------------
--                                                           -*
-- Unit name    : stack_package
-- Version      : 1.0
-- Author       : Tom Duke
--              : TI Ada Technology Branch
--              : PO Box 801, MS 8007
--              : McKinney, TX  75069
-- DDN Address  : DUKE%TI-EG at CSNET-RELAY
-- Copyright    : (c) N/A 
-- Date created : 16 Apr 85 
-- Release date : 16 Apr 85 
-- Last update  : 16 Apr 85 
-- Machine/System Compiled/Run on :DG MV 10000, ROLM ADE
--                                                           -*
---------------------------------------------------------------
--                                                           -*
-- Keywords     : stack, generic stack 
----------------:
--
-- Abstract     : This is a generic package that provides the types,
----------------: procedures, and exceptions to define an abstract stack
----------------: and its corresponding operations.  Using an
----------------: instantiation of this generic package, one can declare
----------------: multiple versions of a stack of type GENERIC_STACK.
----------------: The stack operations provided include:
----------------: 1. clear the stack,
----------------: 2. pop the stack,
----------------: 3. push an element onto the stack, and
----------------: 4. access the top element on the stack.
----------------:  
--                                                           -*
------------------ Revision history ---------------------------
--                                                           -*
-- DATE         VERSION	AUTHOR                  HISTORY
-- 4/16/85	1.0	Tom Duke		Initial Release
--                                                           -*
------------------ Distribution and Copyright -----------------
--                                                           -*
-- This prologue must be included in all copies of this software.
--
-- This software is released to the Ada community.
-- This software is released to the Public Domain (note:
--   software released to the Public Domain is not subject
--   to copyright protection).
-- Restrictions on use or distribution:  NONE
--                                                           -*
------------------ Disclaimer ---------------------------------
--                                                           -*
-- This software and its documentation are provided "AS IS" and
-- without any expressed or implied warranties whatsoever.
-- No warranties as to performance, merchantability, or fitness
-- for a particular purpose exist.
--
-- Because of the diversity of conditions and hardware under
-- which this software may be used, no warranty of fitness for
-- a particular purpose is offered.  The user is advised to
-- test the software thoroughly before relying on it.  The user
-- must assume the entire risk and liability of using this
-- software.
--
-- In no event shall any person or organization of people be
-- held responsible for any direct, indirect, consequential
-- or inconsequential damages or lost profits.
--                                                           -*
-------------------END-PROLOGUE--------------------------------

generic

  type ELEMENTS is private;
  SIZE : POSITIVE;

package STACK_PACKAGE is

  type GENERIC_STACK is  private;


  function TOP_ELEMENT( STACK  : in  GENERIC_STACK )
    return ELEMENTS;

  function STACK_IS_EMPTY( STACK : in GENERIC_STACK )
    return BOOLEAN;

  procedure CLEAR_STACK( STACK : in out GENERIC_STACK );


  procedure PUSH       ( FRAME : in ELEMENTS;
                         STACK : in out GENERIC_STACK );

  procedure POP        ( FRAME : out ELEMENTS;
                         STACK : in out GENERIC_STACK );

  NULL_STACK      : exception;
  STACK_OVERFLOW  : exception;
  STACK_UNDERFLOW : exception;


private

  type STACK_LIST is array ( 1 .. SIZE ) of ELEMENTS;

  type GENERIC_STACK  is
     record
      CONTENTS       :  STACK_LIST;
      TOP            :  NATURAL range NATURAL'FIRST .. SIZE := NATURAL'FIRST;
     end record;

end STACK_PACKAGE;


-------------------------------------------------------------------------


package body STACK_PACKAGE is

---------------
--  function TOP_ELEMENT  --  This function returns the value of the top
--                            element on the stack.  It does not return a
--  pointer to the top element.  If the stack is empty, a constraint error
--  occurs.  The exception handler will then raise the NULL_STACK
--  exception and pass it to the calling procedure.
---------------
  function TOP_ELEMENT( STACK : in  GENERIC_STACK ) return ELEMENTS is
  begin
   return STACK.CONTENTS(STACK.TOP);
   exception
      when CONSTRAINT_ERROR =>
         raise NULL_STACK;
      when others =>
         raise;
  end TOP_ELEMENT;

  ----------
  --  Is stack empty?
  ----------
  function STACK_IS_EMPTY( STACK : in GENERIC_STACK )
    return BOOLEAN is
  begin
    return (STACK.TOP = NATURAL'FIRST);
  exception
    when OTHERS =>
         raise;
  end STACK_IS_EMPTY;


---------------
--  procedure CLEAR_STACK  --  This procedure resets the stack pointer, TOP,
--                             to a value representing an empty stack.
---------------
  procedure CLEAR_STACK( STACK : in out GENERIC_STACK ) is
  begin
   STACK.TOP := NATURAL'FIRST;
  end CLEAR_STACK;


---------------
--  procedure PUSH  --  This procedure attempts to push another element onto
--                      the stack.  If the stack is full, a constraint error
--  occurs.  The exception handler will then raise the STACK_OVERFLOW
--  exception and pass it to the calling procedure.
---------------
  procedure PUSH       ( FRAME : in ELEMENTS;
                         STACK : in out GENERIC_STACK ) is
  begin
   STACK.TOP := STACK.TOP + 1;
   STACK.CONTENTS(STACK.TOP) := FRAME;
   exception
      when CONSTRAINT_ERROR =>
         raise STACK_OVERFLOW;
      when others =>
         raise;
  end PUSH;


---------------
--  procedure POP  --  This procedure attempts to pop an element from
--                     the stack.  If the stack is empty, a constraint error
--  occurs.  The exception handler will then raise the STACK_UNDERFLOW
--  exception and pass it to the calling procedure.
---------------
  procedure POP        ( FRAME : out ELEMENTS;
                         STACK : in out GENERIC_STACK ) is
  begin
   FRAME := STACK.CONTENTS(STACK.TOP);
   STACK.TOP := STACK.TOP - 1;
   exception
      when CONSTRAINT_ERROR =>
         raise STACK_UNDERFLOW;
      when others =>
         raise;
  end POP;

end STACK_PACKAGE;
