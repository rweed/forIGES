! This file is part of forIGES
! https://github.com/rweed/forIGES

! Copyright (C) 2024 Richard Weed.
! All rights reserved.

! Redistribution and use in source and binary forms, with or without 
! modification, are permitted provided that the following conditions are met:

! 1. Redistributions of source code, in whole or in part, must retain the  
! above copyright notice, this list of conditions and the following 
! disclaimer.

! 2. Redistributions in binary form, in whole or in part, must reproduce the 
! above copyright notice, this list of conditions and the following disclaimer 
! in the documentation and/or other materials provided with the distribution.

! 3. The names of the contributors may not be used to endorse or promote from 
! products derived from this software without specific prior written 
! permission.

! 4. Redistribution of this source code, including any modifications, may 
! not be intentionally obfuscated.

! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
! IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
! THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
! PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND
! CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
! EXEMPLARARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
! PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
! OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
! WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
! OTHERWISE), ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
! ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Module commandLineUtils
!! Utilities for checking a main program command line for arguments 

!! Author  : Richard Weed

!! Version : 0.1 (Beta release 1)
!! Date    : August 18, 2024

  USE ISO_C_BINDING,   ONLY : C_NULL_CHAR
  USE ISO_FORTRAN_ENV, ONLY : stdout=>output_unit, stderr=>error_unit

  Implicit NONE

  Integer, Parameter, Private :: MAXLEN=132 
  !! Maximum length for a command line character string
 
  Private :: C_NULL_CHAR, stdout, stderr
 
Contains

  Function checkForParameters() Result(has_params)
  !! Check if command line arguments are present. Return .TRUE. if they are. 
    Logical :: has_params

    has_params = .FALSE.

    has_params = (0 < COMMAND_ARGUMENT_COUNT()) 

  End Function checkForParameters


  Subroutine getCommandLine(commandline)
  !! Get the entire command line string

    Character(LEN=:), Allocatable, Intent(INOUT) :: commandLine
    !! Output the current command line
  
    Integer :: lineLen
    
    Call GET_COMMAND(LENGTH=lineLen)
    If (lineLen > 0) Then
      ALLOCATE(character(LEN=lineLen) :: commandLine) 
      commandLine = REPEAT(" ",lineLen)
      Call GET_COMMAND(commandLine)
    Else
      ALLOCATE(character(LEN=1) :: commandLine)
      commandLine = C_NULL_CHAR 
    EndIf
  
  End Subroutine getCommandLine

  Subroutine getCommandArg(narg, arg)
  !! Check for a return a given argument 

    Integer,          Intent(IN)                 :: narg
    !! Argument position in command line 
    Character(LEN=:), Allocatable, Intent(INOUT) :: arg
    !! Argument character string
 
    Integer :: argLen

    Call GET_COMMAND_ARGUMENT(narg, LENGTH=argLen)
    If (ALLOCATED(arg)) DEALLOCATE(arg)
    ALLOCATE(Character(LEN=arglen) :: arg)
    arg = REPEAT(" ",arglen)
    Call GET_COMMAND_ARGUMENT(narg, arg)

  End Subroutine getCommandArg

  Subroutine parseCommandArg(prefix, argument, have_arg)
  !! Parse a command argument for a given prefix value and return the
  !! argument without the prefix
 
    Character(LEN=*),              Intent(IN)    :: prefix
    !! prefix value (something like -i or -o
    Character(LEN=:), Allocatable, Intent(INOUT) :: argument
    !! argument without the prefix
    Logical,                       Intent(OUT)   :: have_arg
    !! true if argument was found

    Logical                       :: has_parameters
    Integer                       :: numargs, ii, is, is2, ie, argLen, iarg
    Character(LEN=:), Allocatable :: argbuf
    
    have_arg = .FALSE.
  
    If (ALLOCATED(argument)) DEALLOCATE(argument)
    ALLOCATE(Character(LEN=1) :: argument)
    argument = C_NULL_CHAR 
    has_parameters = checkForParameters()
    If (has_parameters) Then
      numargs = COMMAND_ARGUMENT_COUNT()
      ii = 0
      
      ParseArg: Do
        ii = ii+1
        If (ii > numargs) EXIT ParseArg
        iarg = ii 
        Call getCommandArg(iarg,argBuf)
        argLen = LEN(argBuf)
        is = INDEX(argbuf, ADJUSTL(TRIM(prefix)))
        If (is > 0) Then
          If (prefix == argBuf) Then ! -prefix arg
            iarg = ii+1 
            If (ALLOCATED(argBuf)) DEALLOCATE(argBuf)
            Call getCommandArg(iarg,argBuf)
            argLen = LEN(argBuf)
            If (ALLOCATED(argument)) DEALLOCATE(argument)
            ALLOCATE(Character(LEN=argLen) :: argument)
            argument(1:argLen) = argBuf(1:argLen)
            have_arg = .TRUE.
            EXIT ParseArg
          Else
            is2 = LEN(prefix)+1
            ie = LEN_TRIM(argBuf)
            If (argBuf(is2:is2)=="=") is2=is2+1
            If (ALLOCATED(argument)) DEALLOCATE(argument)
            ALLOCATE(Character(LEN=(ie-is2+1)) :: argument)
            argument(1:(ie-is2+1)) = argBuf(is2:ie)
            have_arg=.TRUE.
            EXIT ParseArg
          EndIf
       Else
          If (prefix == argBuf) ii = ii+1
        EndIf
       
      EndDo ParseArg

    EndIf

  End Subroutine parseCommandArg

  Subroutine getFileNames(input_filename, output_filename)
  !! Uses command line utils to find/set input and output filenames
  !! from command line input. Input files are specified on the command line
  !! using the -i=filename option. Output files are specified on the command
  !! line using the -o=filename option. If no output filename is specified
  !! a filename is constructed from the base part of the input filename with
  !! a .out file extension appended

    Character(:), Allocatable,           Intent(INOUT) :: input_filename
    !! An input file name from command line
    Character(:), Allocatable, Optional, Intent(INOUT) :: output_filename
    !! An output file name from command line or set to input_name.out if
    !! not present

    Character(LEN=:), Allocatable :: argBuf

    Logical :: has_command_line_params, have_arg
    Integer :: idot
 
    has_command_line_params = checkForParameters()
    If (has_command_line_params) Then
      Call parseCommandArg("-i", argbuf, have_arg)
      If (have_arg) Then
        input_filename = TRIM(ADJUSTL(argbuf))
      Else
        Write(stderr,*) ''
        ERROR STOP " getFileNames: An input filename was not specified on command line"
      EndIf
      If (PRESENT(output_filename)) Then
        Call parseCommandArg("-o", argbuf, have_arg)
        If (have_arg) Then
          output_filename = TRIM(ADJUSTL(argbuf))
          If (ALLOCATED(argbuf)) DEALLOCATE(argbuf)
        Else
          idot = INDEX(input_filename,'.')
          output_filename = TRIM(ADJUSTL(input_filename(1:idot-1)))//".out"
          Write(stdout,*) ''
          Write(stdout,'(" No output filename was specified on command line")')
          Write(stdout,'(" Output filename set to : ",A)') output_filename
        End If
      EndIf
    Else
      Write(stderr,*) ''
      Write(stderr,'(" getFileNames : No filenames specified on commandline")')
      Write(stderr,'(" An input filename is required ")')
      ERROR STOP
    End If  
 
  End Subroutine getFileNames 

End Module commandLineUtils
