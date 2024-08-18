! This file is part of the forIGES libary
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
Module IGES_utils
!! Utility routines used by forIGES library routines 

!! Author  : Richard Weed

!! Version : 0.1 (Beta release 1)
!! Date    : August 18, 2024

  USE IGES_params, ONLY : C_ENUM, forIGES_entities, string_t

  Implicit NONE

  PRIVATE

  PUBLIC :: getUnitName, is_supported_entity, statusString, fieldsToRecs,     &
            DE2PTR, PTR2DE, convertCompToOldIGES, convertOldIGEStoComp

Contains

  Subroutine getUnitName(unit_flag, unit_name)
!! Return a character string with the unit name corresponding to the input
!! unit flag variable
 
    Integer(C_ENUM),             Intent(IN)    :: unit_flag
    !! unit flag value 
    Character(:),   Allocatable, Intent(INOUT) :: unit_name
    !! output unit_name string

    Integer :: uflag

    uflag = unit_flag

! Define unit name based on unit_flag

    Select Case(uflag)

      Case(1)
        unit_name = "INCH"
      Case(2)
        unit_name = "MM"
      Case(3)
        unit_name = "FT"
      Case(4)
        unit_name = "MI"
      Case(5)
        unit_name = "M"
      Case(6)
        unit_name = "KM"
      Case(7)
        unit_name = "MIL"
      Case(8)
        unit_name = "UM"
      Case(9)
        unit_name = "CM"
      Case(10)
        unit_name = "UIN"
      Case Default
        unit_name = "INCH"
    End Select

  End Subroutine getUnitName

  Function is_supported_entity(entity) RESULT(is_supported)
!! Check to see if an input entity number is a supported entity

    Implicit NONE

    Integer, Intent(IN) :: entity       !! entity number to be checked
    Logical             :: is_supported !! true if supported, false if not     

! Check forIGES_entities array to see if input entity number is supported

    is_supported = .FALSE.
    is_supported = ANY(entity==forIGES_entities)

  End Function is_supported_entity

  Function statusString(status_numbers) Result(stat)
  !! Constructs a status string from the input values of status numbers
   
     Integer,      Intent(IN) :: status_numbers(4)
     Character(8)             :: stat

     Integer :: i

     stat = "        "

! Create status string from status numbers

     If (ANY(status_numbers /= 0)) Then
       Write(stat(7:8),'(i2)') status_numbers(4)
       Write(stat(5:6),'(i2)') status_numbers(3)
       Write(stat(3:4),'(i2)') status_numbers(2)
       Write(stat(1:2),'(i2)') status_numbers(1)
       Do i=1,8
         If(stat(i:i) == " ") stat(i:i) = "0"
       End Do
     End If

  End Function statusString

  Subroutine fieldsToRecs(fields, recbuf, reclen)
!! Convert individual data fields to records and return the
!! records array and nrecs

    Type(string_t),              Intent(IN)    :: fields(:)
    !!  Input array of string_t type fields
    Type(string_t), Allocatable, Intent(INOUT) :: recbuf(:)
    !!  output array of string_t type recbuf 
    Integer,        Optional,    Intent(IN)    :: reclen
    !! Optional record length to define the length of the part of a record
    !! that this entity or directory entry is to be written to. For P
    !! records this has a maximum length of 64, for the Gsection its 72
    Integer              :: i, irec, nrecs, nchars, nfields, rlen
    Integer, Allocatable :: frec(:)

    rlen = 72 ! maximum number of characters allowed in Gsection string
              ! Psection data will be set to 64

    If (PRESENT(reclen)) rlen = reclen

    nrecs = 1
    nchars = 0
    nfields = SIZE(fields)
    ALLOCATE(frec(nfields), SOURCE=0)

! For each field str, compute the number of fields (characters) that will fit in
! the desired reclen and store that info in the frec array

    Do i=1, nfields
     nchars = nchars+LEN_TRIM(fields(i)%str)
      If (nchars <= reclen) Then
        frec(i) = nrecs
      Else
        nchars = 0
        nrecs = nrecs+1
        nchars = LEN_TRIM(fields(i)%str)
        frec(i) = nrecs
      End If
    End Do

! Now use the frec array to concatenate the field strings into a 80 
! Character record

    ALLOCATE(recbuf(nrecs))
    Do i=1,nrecs
      recbuf(i)%str = ""
    End Do
    Do i=1,nfields
      irec = frec(i)
      recbuf(irec)%str = recbuf(irec)%str//fields(i)%str
    End Do

  End Subroutine fieldsToRecs

  Elemental Function DE2PTR(DE) Result(ptr)
!! Convert a Dictionary entry index to a sequence number pointer

     Integer,      Intent(IN) :: DE
     !! Inputs Dictionary entry index
     Integer                  :: ptr
     !! Outputs a sequence number in a  Directionary records array

     ptr = 0
     If (ABS(DE) > 0) ptr = 2*ABS(DE)-1

  End Function DE2PTR

  Elemental Function PTR2DE(PTR) Result(DE)
!! Convert a Dictionary entry sequence number pointer to an index

     Integer,      Intent(IN) :: PTR
     !! Inputs a sequence number in a Directionary records array
     Integer                  :: DE
     !! Outputs a Dictionary entry index

     DE = 0
     IF (ABS(PTR) > 0) DE = (ABS(PTR)+1)/2

  End Function PTR2DE

!**************************************************************************

  Character(8) Function blnk(buf)
!! Used by compressed ascii routines

! START FUNCTION BLNK HERE

! WRITTEN BY P. R. KENNICOTT 9-29-83.
! GENERAL ELECTRIC CORP. RE. & DEV.

! RE-WRITTEN BY LEE KLEIN 9-2-84.
! GENERAL DYNAMICS CAD/CAM POMONA DIV.

! REVISED BY LEE KLEIN 8-7-86
! GENERAL DYNAMICS CAD/CAM POMONA DIV.

! REVISED BY ROBERT COLSHER 22 AUG 1986
! IGES DATA ANALYSIS COMPANY

! REFACTORED BY: Dr. Richard Weed
!                rweedmsu@gmail.com
!                July, 2024


! PURPOSE:

!  TO REMOVE BLANKS FROM END OF A CHARACTER STRING (RIGHT JUSTIFY)

! INPUT:

!      BUF STRING WITH TRAILING BLANKS

! OUTPUT:

!       BLNK STRING WITH TRAILING BLANKS REMOVED

! METHOD:

! FIND FIRST BLANK, THEN TRANSLATE OUTPUT STRING


! RESTRICTIONS:

! 1. BUF <= 512 CHARACTERS.
! 2. LENGTHS OF BUF & BLNK MUST BE =.
! 3. FIRST CHARACTER MUST NOT BE BLANK OR NO CONVERSION.

! VARIABLE DECLARATIONS...

    Character(*), Intent(inout) :: buf
    Integer :: i, m, n
    Character(512) :: ibuf

! SET UP COUNTERS...

    n=index(buf(1:),' ')-1
    m=len(buf)

! CHECK FOR SIZE TOO BIG...

    If (m >  512) Stop 'Buffer too big at Function BLNK'
 

! CHECK FOR FIRST CHAR A BLANK...

    If (buf(1:1) == ' '.OR.buf(m:m) /= ' ') Then
      blnk=buf
      Return
    EndIf

! OK PROCESS STRING...

    Do i=1,n
      ibuf(m-i+1:m-i+1)=buf(n-i+1:n-i+1)
    End Do
    ibuf(1:m-n)=' '
    blnk=ibuf
    Return

  End Function blnk 

!**************************************************************************

  Subroutine convertCompToOldIGES(infile,outfil)
!! Convert a compressed ascii file to standard IGES

! OLD FORM CONVERSION

! PROGRAM ORIGANALLY WRITTEN BY J. M. SPAETH 7-24-84
! GENERAL ELECTRIC CORP. RE. & DEV.

! RE-WRITTEN BY LEE KLEIN 9-20-84
! GENERAL DYNAMICS CAD/CAM POMONA DIV.

! REVISED BY ROBERT COLSHER 22 AUG 1986
! IGES DATA ANALYSIS COMPANY

! REFACTORED BY: Dr. Richard Weed
!                rweedmsu@gmail.com
!                July, 2024

! PURPOSE:

! TO CONVERT NEW FORM OF IGES OUTPUT TO OLD FORM...

! VARIABLE DECLARATIONS...

    Character(*), Intent(IN) :: infile, outfil

    Character(8)   :: inarr(20),newarr(20)
    Character(80)  :: inline
    Character(160) :: outarr
    Integer        :: icnt1,icnt2,icnt3,ia,ib,ic
    Integer        :: i,ip, j, ij,ik,il,it, iend
    Integer        :: u1, u2, u3, u4, u9, u10
 
! INITIALIZE OUTARR TO BLANKS...

    outarr(1:160)=' '

! OPEN OUTPUT AND TEMP FILES...

!    Open(newunit=u1,file='FILE1.TMP',status='NEW',carriagecontrol='LIST')
!    Open(newunit=u2,file='FILE2.TMP',status='NEW',carriagecontrol='LIST')
!    Open(newunit=u3,file='FILE3.TMP',status='NEW',carriagecontrol='LIST')
!    Open(newunit=u4,file='FILE4.TMP',status='NEW',carriagecontrol='LIST')
!    Open(newunit=u9,file=outfil,status='NEW',carriagecontrol='LIST')
    Open(newunit=u1,file='FILE1.TMP',status='NEW', FORM="FORMATTED")
    Open(newunit=u2,file='FILE2.TMP',status='NEW', FORM="FORMATTED")
    Open(newunit=u3,file='FILE3.TMP',status='NEW', FORM="FORMATTED")
    Open(newunit=u4,file='FILE4.TMP',status='NEW', FORM="FORMATTED")
    Open(newunit=u9,file=outfil,     status='NEW', FORM="FORMATTED")
    Open(newunit=u10,file=infile,    status='OLD', FORM="FORMATTED")

! INITIALIZE COUNTERS...

    icnt1 = -1
    icnt2 = 1
    icnt3 = 0
    il = 1

! READ THE FILE AND SEPARATE INTO PARTS...

    LOOP2000: Do
      Read(u10,2900,iostat=iend) inline
      If (iend < 0) EXIT LOOP2000 
      If (inline(73:73) == "C") CYCLE LOOP2000

      If ((inline(73:73) == 'S').OR.(inline(73:73) == 'G')) Then 
        Write(u1,2900)inline
        CYCLE LOOP2000
       
      Else If (inline(73:73) == 'T') Then
        Write(u2,2900) inline
        CYCLE LOOP2000

      Else If (inline(73:73) == 'P') Then
        Write(u4,2910) inline(1:64),icnt1,'P',icnt2
        icnt2 = icnt2 + 1
        CYCLE LOOP2000

      Else If ((inline(1:1) == '@').OR.(inline(1:1) == 'D')) Then 

! REWRITE TO DE LINES IN THE NEW FORM...
! GO THRU INLINE ONE CHAR. AT A TIME LOOKING FOR THE
! DELIMETER (@, ,_)...

        il = 1
        LOOP2050: Do
          If (il >  80) CYCLE LOOP2000 
          If (inline(il:il) == ';') Then
            Write(u3,2920) outarr(1:8),icnt2,outarr(17:80)
            Write(u3,2900) outarr(81:160)
            icnt1 = icnt1 + 2
            CYCLE LOOP2000
          End If
          If (inline(il:il) == '@') Then 

! DETERMINE IF THE FIELD IS ONE OR TWO CHARACTERS...

            If (inline(il+2:il+2) == '_') Then
              ic=ICHAR(inline(il+1:il+1))-48
              il=il+2
            Else
              ia=ICHAR(inline(il+1:il+1))-48
              ib=ICHAR(inline(il+2:il+2))-48
              ic=10*ia+ib
              il=il+3
            EndIf

! AT THIS POINT IC IS THE NUMBER OF THE RECORD FIELD BEING
! PROCESSED, AND INLINE(IL)=USCORE

            it=0
            ik=0
            ij=(ic-1)*8+1

! RESET THE FIELD TO BE CHANGED TO ALL BLANKS IN ORDER TO CREATE
! A COMPLETELY NEW FILED...

            outarr(ij:ij+7)=' '

! WE WILL NOW CONTINUE THRU THE LINE PICKING OFF THE CHAR.
! OF THE RECORD FIELD ONE AT A TIME UNTIL A DELIMETER IS HIT...

            LOOP2060: Do
              ik=ik+1
              If (inline(il+ik:il+ik) == '@') EXIT LOOP2060 
              If (inline(il+ik:il+ik) == ' ') Then
                If (inline(il+ik+1:il+ik+1) == ' ') EXIT LOOP2060 
              EndIf
              If (inline(il+ik:il+ik) == ';') EXIT LOOP2060 
              it=it+1
              ij=(ic-1)*8+it
              outarr(ij:ij)=inline(il+ik:il+ik)
              If (ic == 1) Then
                outarr(ij+80:ij+80)=inline(il+ik:il+ik)
              EndIf
            End Do LOOP2060
            il=il+it
          End If
          il = il + 1
        End Do Loop2050
      End If
    End Do Loop2000

! REWIND ALL FILES BEFORE WE WRITE THEM TO OUTPUT...

    Rewind (u1)
    Rewind (u2)
    Rewind (u3)
    Rewind (u4)

! WRITE START AND GLOBAL RECORDS TO OUTPUT FILE...

    LOOP2100: Do
      Read(u1,2900,iostat=iend) inline
      If (iend < 0) EXIT LOOP2100
      Write(u9,2900) inline
    End Do LOOP2100

! WRITE THE DE RECORDS TO OUTPUT FILE.
! THEY NOW BECOME RE-FORMATTED...

    LOOP2110: Do    
      Read(u3,2930,iostat=iend) (inarr(i),i=1,10)
      If (iend < 0) EXIT LOOP2110
      Read(u3,2930) (inarr(i),i=11,20)

! CHANGE THOSE FIELDS THAT MUST BE RIGHT JUSTIFIED

      Do ip=1,20
        If ((ip /= 9).AND.(ip /= 18)) Then 
          newarr(ip)=blnk(inarr(ip))
        Else
          newarr(ip)=inarr(ip)
        End If
      End Do
      icnt3=icnt3+1
      Write(u9,2940) (newarr(j),j=1,9),'D',icnt3
      icnt3=icnt3+1
      Write(u9,2940) (newarr(j),j=11,19),'D',icnt3
    End Do LOOP2110

! WRITE THE PD LINES TO THE OUTPUT FILE...

    LOOP2140: Do
      Read(u4,2900,iostat=iend) inline
      If (iend < 0) EXIT LOOP2140
      Write(u9,2900) inline
    End Do LOOP2140

! WRITE THE TERMINATE LINES TO THE OUTPUT FILE...

    LOOP2150: Do
      Read(u2,2900,iostat=iend) inline
      If (iend < 0) EXIT LOOP2150
      Write(u9,2900) inline
    End Do LOOP2150

! NOW CLOSE THE FILES AND DELETE THE TEMP ONES...

    Close(unit=u1,status='DELETE')
    Close(unit=u2,status='DELETE')
    Close(unit=u3,status='DELETE')
    Close(unit=u4,status='DELETE')
    Close(u9)
    Close(u10)
    Return

! FORMATS

2900  Format(a80)
2910  Format(a64,1i8,1a1,1i7)
2920  Format(a8,i8,a64)
2930  Format(10a8)
2940  Format(9a8,a,i7)
 
  End Subroutine convertCompToOldIGES 

!**************************************************************************

  Subroutine convertOldIGEStoComp(infile, outfil)
!! Convert a standard IGES file to compressed ascii format

! START NEW SUBROUTINE HERE

! PROGRAM ORIGANALLY WRITTEN BY J. M. SPAETH 7-24-84
! GENERAL ELECTRIC CORP. RE. & DEV.

! RE-WRITTEN BY LEE KLEIN 9-20-84
! GENERAL DYNAMICS CAD/CAM POMONA DIV.

! REVISED BY LEE KLEIN 8-7-86
! GENERAL DYNAMICS CAD/CAM POMONA DIV.

! REVISED BY ROBERT COLSHER 22 AUG 1986
! IGES DATA ANALYSIS COMPANY

! REFACTORED BY: Dr. Richard Weed
!                rweedmsu@gmail.com
!                July, 2024

! PURPOSE:

! TO CONVERT OLD FORM OF IGES OUTPUT TO NEW FORM...

! VARIABLE DECLARATIONS...

    Character(*), Intent(IN) :: infile, outfil

    Integer       :: pdrcd, iend
    Character(80) :: inline
    Integer :: u1, u2, u3, u5, u4, u10

! OPEN THE INPUT AND TEMP FILES...

    Open(unit=u10,file=infile,status='OLD', FORM="FORMATTED")
!    Open(unit=u1,file='TEST.TMP',status='NEW',carriagecontrol='LIST')
    Open(unit=u1,file='TEST.TMP',status='NEW',FORM="FORMATTED")
    Open(unit=u2,file='FILE2.TMP',status='NEW',recl=80, &
     access='DIRECT',form="FORMATTED")
    Open(unit=u3,file='FILE3.TMP',status='NEW', FORM="FORMATTED")
    Open(unit=u5,file='FILE5.TMP',status='NEW', FORM="FORMATTED")

! WRITE THE HEADER WITH A "C" TO SHOW COMPRESSED ASCII FORM...

    Write(1,4900) 'C',1

! SEPERATE THE PD AND DE RECORDS, WHILE WRITING G,S, &T LINES
! TO THE OUTPUT FILE...

    LOOP4000: Do
      Read(u10,4910,iostat=iend) inline
      If (iend < 0) EXIT LOOP4000 

! WRITE DIRECTORY LINES INTO FILE3.TMP...

      If (inline(73:73) == 'D') Then 
        Write (u3,4910) inline

! WRITE DIRECTORY LINES INTO FILE3.TMP...

      ElseIf (inline(73:73) == 'P') Then 
        Read (inline(74:80),4920) pdrcd
        Write (u2,rec=pdrcd,fmt=4910) inline
      ElseIf (inline(73:73) == 'T') Then 
        Write (u5,4910) inline
        EXIT LOOP4000
      Else 

! WRITE HEADER LINES INTO A TEST.TMP...

        Write (u1,4910) inline
      End If
    End Do LOOP4000

! WRITE TERMINATE RECORD INTO FILE5.TMP...

    Call xpd
    Rewind(u5)
    LOOP4050: Do
      Read(u5,4910,iostat=iend) inline
      If (iend < 0) EXIT LOOP4050
      Write (u1,4910) inline
    End Do LOOP4050
    Call cmpres(outfil)

! CLOSE FILES AND DELETE TEMP ONES...

    Close(unit=u1,status='DELETE')
    Close(unit=u2,status='DELETE')
    Close(unit=u3,status='DELETE')
    Close(unit=u4)
    Close(unit=u5,status='DELETE')
    Close(unit=u10)
    Return

! FORMATS

4900  Format(72x,a,i7)
4910  Format(a80)
4920  Format(i7)

  Contains

!**************************************************************************

    Subroutine xpd

! PROGRAM ORIGINALLY WRITTEN BY J. M. SPAETH 7-24-84
! GENERAL ELECTRIC CORP. RE. & DEV.

! REVISED BY LEE KLEIN 8-7-86
! GENERAL DYNAMICS CAD/CAM POMONA DIV.

! REVISED BY ROBERT COLSHER 22 AUG 1986
! IGES DATA ANALYSIS COMPANY

! REFACTORED BY: Dr. Richard Weed
!                rweedmsu@gmail.com
!                July, 2024


! PURPOSE:

! TO TRANSFER ALL PD & DE RECORDS FORM TEMPORY FILES TO
! OUTPUT FILE IN MERGED FORM...


! VARIABLE DECLARATIONS...

      Character(1)   :: scoln
      Character(8)   :: nudat
      Character(8)   :: lstdat(20)
      Character(8)   :: newdat(20)
      Character(80)  :: pdline,delin1,delin2
      Character(160) :: newde
      Integer        :: fldnum,fldbeg,fldend, iend
      Integer        :: chrptr,newptr,pdptr,pdcnt
      Integer        :: lfld(20)
      Integer        :: i, il
      scoln = ":"

! REWIND THE FILES...

      Rewind(u3) 

! INITIALIZE LAST DATA SO AS NOT TO EQUAL NEXT DATA...

      Do fldnum=1,20
        lstdat(fldnum) = 'XXXXXXXX'
      End Do

! GET NEW DE RECORD

      LOOP5010: Do
        Read(u3,5900,iostat=iend) delin1
        If (iend < 0) EXIT LOOP5010
        Read(u3,5900) delin2
        Read(delin1(9:16),5910) pdptr
        Read(delin2(25:32),5910) pdcnt
        delin1(73:73)=' '
        delin2(73:73)=' '

! CLEAR OUT THE DE RECORD BUFFER...

        newde(1:160) = ' '
 

! GET THE DATA FROM EACH FIELD OF THE DE RECORD SET...

        fldbeg = -7
        fldend = 0
        Do fldnum=1,10
          fldbeg=fldbeg + 8
          fldend=fldend + 8
          Read(delin1(fldbeg:fldend),5920) newdat(fldnum)
          Read(delin2(fldbeg:fldend),5920) newdat(fldnum+10)
        End Do

! FIELD 9 MUST BE ZERO FILLED

        Do i = 1,8
          If (newdat(9)(i:i) == ' ')newdat(9)(i:i) = '0'
        End Do

! FIELD 18 MUST BE RIGHT JUSTIFIED...

        newdat(18)=blnk(newdat(18))

! DETERMINE THE LENGTH OF THE DATA WITHIN EACH FIELD

        LOOP5050 : Do fldnum=1,20
          LOOP5040: Do i=1,8
            If (newdat(fldnum)(i:i) /= ' ') Then
              lfld(fldnum)= 9 - i
              EXIT LOOP5040 
            EndIf
          End Do LOOP5040
        End Do LOOP5050

! WRITE THE DE SEQUENCE NUMBER AT THE BEGINNING OF THE OUTPUT DE RECORD

        nudat=newdat(10)
!        encode(lfld(10)+1,5930,newde(1:lfld(10)+1)) nudat(9-lfld(10):8)
        Write(newde(1:lfld(10)+1), 5930) nudat(9-lfld(10):8)
        chrptr = lfld(10) + 2

! SEARCH NEW DE RECORD SET FOR CHANGED DATA; WHEN FOUND WRITE CHANGED
! DATA TO OUTPUT DE RECORD...

        Do fldnum=1,20

! SKIP FIELDS THAT NO LONGER NEED PROCESSING...

          If (fldnum==2.OR. fldnum==10 .OR. fldnum==1 .OR. fldnum==20) CYCLE 
          If (newdat(fldnum) /= lstdat(fldnum)) Then
            If (fldnum >  9) Then
!              encode(4,5940,newde(chrptr:chrptr+3)) fldnum
              Write(newde(chrptr:chrptr+3), 5940) fldnum
              chrptr=chrptr+4
            Else
!              encode(3,5950,newde(chrptr:chrptr+2)) fldnum
              Write(newde(chrptr:chrptr+2), 5950) fldnum
              chrptr=chrptr+3
            EndIf
            If (lfld(fldnum) /= 0) Then
              If (fldnum /= 9) Then
                Read(newdat(fldnum)(9-lfld(fldnum):8),5960) &
                newde(chrptr:chrptr-1+lfld(fldnum))
                chrptr=chrptr+lfld(fldnum)
              Else

! FIELD 9 IS A SPECIAL CASE...

                Read(newdat(9)(1:8),5960) newde(chrptr:chrptr+7)
                chrptr=chrptr+8
              EndIf
            EndIf
          EndIf

! STORE DATA FROM CURRENT DE RECORD SET TO COMPARE WITH NEXT SET

          lstdat(fldnum)=newdat(fldnum)
        End Do
        newde(chrptr:chrptr) = scoln

! IF OUTPUT DE RECORD > 80 CHAR'S, WRITE 2 LINES...

        If (chrptr >  80) Then
          Do i=1,11
            If (newde(82-i:82-i) == '@') EXIT 
          End Do
          Write(u1,5970) TRIM(ADJUSTL(newde(1:81-i)))
          newde(1:80)=newde(82-i:161-i)
        EndIf
        Write(u1,5900) newde(1:80)

! ERASE UNNECESSARY DATA FROM PD RECORD AND WRITE TO OUTPUT FILE;

        Do il=1,pdcnt
          Read (u2,5900,rec=pdptr) pdline
          pdptr = pdptr+1
          pdline(65:80) = ' '
          Write(u1,5900) pdline
          pdline(1:80) = ' '
        End Do

! END OF LOOP GET NEXT DE RECORD...

      End Do LOOP5010 
 
      Return

! FORMATS

5900  Format(a80)
5910  Format(i8)
5920  Format(a8)
5930  Format ('D',a)
5940  Format('@',i2,'_')
5950  Format('@',i1,'_')
5960  Format(a)
5970  Format(a)

    End Subroutine xpd

!**************************************************************************

    Subroutine cmpres(outfil)

! START OF SUBROUTINE

! PROGRAM ORIGINALLY WRITTEN BY J. M. SPAETH 7-24-84
! GENERAL ELECTRIC CORP. RE. & DEV.

! REVISED BY LEE KLEIN 8-7-86
! GENERAL DYNAMICS CAD/CAM POMONA DIV.

! REVISED BY ROBERT COLSHER 22 AUG 1986
! IGES DATA ANALYSIS COMPANY

! REFACTORED BY: Dr. Richard Weed
!                rweedmsu@gmail.com
!                July, 2024


! PURPOSE:

! TO CLEAR AWAY ALL TRAILING BLANKS FROM THE OUTPUT FILE

! VARIABLE DECLARATIONS...

      Character(*), Intent(IN)  :: outfil

      Integer       :: ierr, iend
      Character(80) :: text
      Integer       :: length
      Logical       :: exists

! REWIND THE INPUT FILE AND OPEN THE OUTPUT FILE

      Rewind(u1)
      INQUIRE(FILE=outfil, EXIST=exists)
      If (.NOT.exists) Then
!        Open (newunit=u4,file=outfil,status='NEW',carriagecontrol='LIST') 
        Open (newunit=u4,file=outfil,status='NEW', FORM="FORMATTED") 
      Else 
!
! GETS HERE IF THERE IS AN ERROR IN THE OUTPUT FILE NAME...

 
        Write(*,6900)'Error in OUTPUT file name. Output written To file', &
        ' IGES.OUT'
!        Open (newunit=u4,file='IGES.OUT',status='NEW',carriagecontrol='LIST')
        Open (newunit=u4,file='IGES.OUT',status='NEW', FORM="FORMATTED")
      End If

! READ RECORD LINES INTO BUFFER ONE AT A TIME...

      LOOP6010: Do
        Read(u1,6910,iostat=iend) text
        If (iend < 0) EXIT LOOP6010
        length = 80

! GO THRU EACH LINE DELETING TRAILING BLANKS

        LOOP6020: Do
          If (text(length:length) /= ' ') EXIT LOOP6020 
          length=length-1
          If (length <= 1  ) EXIT LOOP6020
        End Do LOOP6020 

! WRITE PROCESSED LINES TO THE OUTPUT FILE

        Write(u4,6920) text(1:length)
      End Do LOOP6010

! FORMATS

6900  Format(1x,a,a)
6910  Format(a80)
6920  Format(a)

    End Subroutine cmpres

  End Subroutine convertOldIGEStoComp

End Module IGES_utils
